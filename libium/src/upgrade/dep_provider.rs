mod structs;

use futures_util::{future::try_join_all, FutureExt};
use pubgrub::{PubGrubError, VersionSet};
use reqwest::Client;
use std::{any::Any, cell::RefCell, cmp, fmt, rc::Rc, sync::mpsc, thread};
use structs::Package;

use crate::{
    config::{
        filters::Filter,
        structs::{ModIdentifier, Profile},
    },
    upgrade::{
        self,
        dep_provider::{
            structs::{ManifestLike, RangeLike, ResolutionData, Resolver, Version, VersionRange},
            two_way::TwoWayChannel,
        },
        mod_downloadable, DownloadData,
    },
};

pub use structs::Fabric;
pub use structs::Forge;

mod cache;
mod two_way;

// TODO: Sometimes it randomly panics with `add_derivation should not be called after a decision`
//       I have no idea why but it needs to be fixed! [see related issue](https://github.com/pubgrub-rs/pubgrub/issues/222)
//       most likely related to how `structs::VersionRange` is implemented.

pub struct DependencyProvider<R: Resolver> {
    dependencies: DataDependencies<R>,
    cache: RefCell<Cache<R>>,
}

type DataDependencies<R> =
    pubgrub::DependencyConstraints<Package, VersionRange<<R as Resolver>::Range>>;
type DataPacket<R> = (ModIdentifier, Vec<ResolutionData<R>>);

impl<R: Resolver> DependencyProvider<R> {
    pub fn new(
        dependencies: DataDependencies<R>,
        channel: TwoWayChannel<ModIdentifier, DataPacket<R>>,
    ) -> Result<Self, ErrorIn> {
        Ok(Self {
            dependencies,
            cache: RefCell::new(Cache {
                resoluton_data: cache::Cache::new(channel),
            }),
        })
    }
}

pub async fn solve<R: Resolver>(profile: Profile) -> Result<Vec<DownloadData>, Error<R>> {
    let (data_channel, res_channel) = two_way::channel();
    let handle = start_resolver_thread::<R>(&profile, res_channel);
    process_data_requests(handle, profile.filters, data_channel).await
}

fn start_resolver_thread<R: Resolver>(
    profile: &Profile,
    channel: TwoWayChannel<ModIdentifier, DataPacket<R>>,
) -> thread::JoinHandle<Result<Vec<DownloadData>, Error<R>>> {
    let dependencies: DataDependencies<R> = profile
        .mods
        .iter()
        .map(|mod_| (Package::Id(mod_.identifier.clone()), VersionRange::full()))
        .collect();

    std::thread::spawn(|| {
        let dep_provider = DependencyProvider::<R>::new(dependencies, channel)?;
        let pkgs = pubgrub::resolve(&dep_provider, Package::Root, Version::Root)?;
        println!("\nDONE! pkgs: {pkgs:?}");
        let mut ret = Vec::with_capacity(pkgs.len());

        for (pkg, version) in pkgs {
            let id = match pkg {
                Package::Root => continue,
                Package::Id(id) => id,
            };

            let data = dep_provider.fetch_download_files(&id)?;
            let Some(data) = get_package_version(&data, &version)? else {
                continue;
            };

            ret.push(data.data.clone());
        }

        Ok(ret)
    })
}

async fn process_data_requests<R: Resolver>(
    resolver_thread: thread::JoinHandle<Result<Vec<DownloadData>, Error<R>>>,
    filters: Vec<Filter>,
    ch: TwoWayChannel<DataPacket<R>, ModIdentifier>,
) -> Result<Vec<DownloadData>, Error<R>> {
    let client = Client::new();
    let mut futs = vec![];

    loop {
        if resolver_thread.is_finished() {
            break;
        }

        while let Ok(id) = ch.rx.try_recv() {
            println!("GET {id}");
            let fut = fetch_data::<R>(client.clone(), id.clone(), filters.clone());
            let fut = fut.map(|data| data.map(|d| (id, d)));
            futs.push(tokio::task::spawn(fut));
        }

        let mut new_futs = vec![];

        for fut in futs {
            if fut.is_finished() {
                let data = fut.await.map_err(ErrorIn::JoinThread)??;
                ch.tx.send(data).map_err(Error::ThreadSend)?;
            } else {
                new_futs.push(fut);
            }
        }

        futs = new_futs;

        tokio::task::yield_now().await;
    }

    resolver_thread
        .join()
        .map_err(ErrorIn::JoinResolverThread)?
}

impl<R: Resolver> pubgrub::DependencyProvider for DependencyProvider<R> {
    type P = Package;

    type V = Version<<R::Range as RangeLike>::Version>;

    type VS = VersionRange<R::Range>;

    type Priority = u32;

    type M = String;

    type Err = ErrorIn;

    fn prioritize(
        &self,
        package: &Self::P,
        range: &Self::VS,
        package_conflicts_counts: &pubgrub::PackageResolutionStatistics,
    ) -> Self::Priority {
        println!("\nprioritize({package}, {range})");
        let count = package_conflicts_counts.conflict_count();
        println!("package_conflicts_counts: {count}");
        count
    }

    fn choose_version(
        &self,
        package: &Self::P,
        range: &Self::VS,
    ) -> Result<Option<Self::V>, Self::Err> {
        println!("\nchoose_version({package}, {range})");
        let id = match package {
            Package::Root => return Ok(Some(Version::Root)),
            Package::Id(id) => id,
        };

        let data = self.fetch_download_files(id)?;

        let iter = data
            .iter()
            .filter(|x| x.manifest.as_ref().is_some_and(|x| x.matches(range)));

        let mut found = None;

        for a in iter {
            match found {
                Some(b) if compare_data(a, b)?.is_gt() => {
                    found = Some(a);
                }
                Some(_) => {}
                None => found = Some(a),
            }
        }

        Ok(found
            .and_then(|x| x.manifest.as_ref())
            .and_then(|x| x.version()))
    }

    fn get_dependencies(
        &self,
        package: &Self::P,
        version: &Self::V,
    ) -> Result<pubgrub::Dependencies<Self::P, Self::VS, Self::M>, Self::Err> {
        println!("\nget_dependencies({package}, {version})");
        let id = match package {
            Package::Root => {
                return Ok(pubgrub::Dependencies::Available(self.dependencies.clone()));
            }
            Package::Id(id) => id,
        };

        let data = self.fetch_download_files(id)?;
        let data = get_package_version(&data, version)?;

        match data {
            Some(x) => {
                let mut dependencies = pubgrub::DependencyConstraints::default();
                for id in &x.data.dependencies {
                    dependencies.insert(Package::Id(id.clone()), VersionRange::full());
                }
                Ok(pubgrub::Dependencies::Available(dependencies))
            }
            None => Ok(pubgrub::Dependencies::Available(
                pubgrub::DependencyConstraints::default(),
            )),
        }
    }
}

fn get_package_version<'c, R: Resolver>(
    data: &'c Rc<Vec<ResolutionData<R>>>,
    version: &Version<<R::Range as RangeLike>::Version>,
) -> Result<Option<&'c ResolutionData<R>>, ErrorIn> {
    Ok(data.iter().find(|x| {
        x.manifest
            .as_ref()
            .and_then(|x| x.version())
            .is_some_and(|x| x == *version)
    }))
}

fn compare_data<R: Resolver>(
    a: &ResolutionData<R>,
    b: &ResolutionData<R>,
) -> Result<cmp::Ordering, ErrorIn> {
    Ok(match (&a.manifest, &b.manifest) {
        (Some(a), Some(b)) => a.compare(b),
        (Some(_), None) => cmp::Ordering::Greater,
        (None, Some(_)) => cmp::Ordering::Less,
        (None, None) => cmp::Ordering::Equal,
    })
}
struct Cache<R: Resolver> {
    resoluton_data: cache::Cache<ModIdentifier, Vec<ResolutionData<R>>>,
}

impl<R: Resolver> DependencyProvider<R> {
    /// Fetch a list of possible downloads for a mod.
    fn fetch_download_files(
        &self,
        id: &ModIdentifier,
    ) -> Result<Rc<Vec<ResolutionData<R>>>, ErrorIn> {
        println!("fetch_download_files({id})");
        let mut cache = self.cache.borrow_mut();
        let files = cache.resoluton_data.get(id.clone())?;

        // Prefetch data
        for data in files.as_ref() {
            for id in &data.data.dependencies {
                cache.resoluton_data.try_request(id.clone())?;
            }
            for id in &data.data.conflicts {
                cache.resoluton_data.try_request(id.clone())?;
            }
        }

        Ok(files)
    }
}

pub async fn fetch_data<R: Resolver>(
    client: Client,
    id: ModIdentifier,
    profile_filters: Vec<Filter>,
) -> Result<Vec<ResolutionData<R>>, ErrorIn> {
    let data = id.fetch_download_files(profile_filters).await?;

    async fn fetch<R: Resolver>(
        client: &Client,
        data: DownloadData,
    ) -> Result<ResolutionData<R>, ErrorIn> {
        Ok(ResolutionData {
            manifest: R::Manifest::fetch_manifest(&client, &data.download_url, Some(data.length))
                .await?,
            data,
        })
    }

    let futs = data.into_iter().map(|x| fetch(&client, x));
    println!("fetch manifest");
    let x = try_join_all(futs).await?;
    println!("DONE");
    Ok(x)
}

#[derive(thiserror::Error)]
#[error(transparent)]
pub enum Error<R: Resolver> {
    Internal(#[from] ErrorIn),
    Resolution(#[from] PubGrubError<DependencyProvider<R>>),
    #[error("send resolution data: {0}")]
    ThreadSend(mpsc::SendError<DataPacket<R>>),
}

impl<R: Resolver> fmt::Debug for Error<R> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::Internal(arg0) => f.debug_tuple("Internal").field(arg0).finish(),
            Self::Resolution(arg0) => f.debug_tuple("Resolution").field(arg0).finish(),
            Self::ThreadSend(arg0) => f.debug_tuple("ThreadSend").field(arg0).finish(),
        }
    }
}

#[derive(thiserror::Error, Debug)]
pub enum ErrorIn {
    #[error("download: {0}")]
    Download(#[from] mod_downloadable::Error),
    #[error("upgrade: {0}")]
    Upgrade(#[from] upgrade::Error),
    #[error("join request thread: {0}")]
    JoinThread(tokio::task::JoinError),
    #[error("join resolver thread: {0:?}")]
    JoinResolverThread(Box<dyn Any + Send + 'static>),
    #[error("resolver: {0}")]
    Cache(#[from] cache::Error<ModIdentifier>),
}
