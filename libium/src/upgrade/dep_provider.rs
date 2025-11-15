mod structs;

use futures_util::{future::try_join_all, FutureExt};
use pubgrub::{PubGrubError, VersionSet};
use reqwest::Client;
use std::{
    any::Any,
    cell::RefCell,
    cmp,
    collections::{hash_map::Entry, HashMap},
    fmt,
    rc::Rc,
    sync::mpsc,
    thread,
};
use structs::Package;

use crate::{
    config::{
        filters::Filter,
        structs::{ModIdentifier, Profile},
    },
    upgrade::{
        self,
        dep_provider::structs::{
            ManifestLike, RangeLike, ResolutionData, Resolver, Version, VersionRange,
        },
        mod_downloadable, DownloadData,
    },
};

pub use structs::Fabric;
pub use structs::Forge;

pub struct DependencyProvider<R: Resolver> {
    dependencies: DataDependencies<R>,
    cache: RefCell<Cache<R>>,
    tx: mpsc::Sender<ModIdentifier>,
    rx: mpsc::Receiver<DataPacket<R>>,
}

type DataDependencies<R> =
    pubgrub::DependencyConstraints<Package, VersionRange<<R as Resolver>::Range>>;
type DataPacket<R> = (ModIdentifier, Vec<ResolutionData<R>>);

impl<R: Resolver> DependencyProvider<R> {
    pub fn new(
        dependencies: DataDependencies<R>,
        tx: mpsc::Sender<ModIdentifier>,
        rx: mpsc::Receiver<DataPacket<R>>,
    ) -> Result<Self, ErrorIn> {
        Ok(Self {
            dependencies,
            cache: RefCell::new(Cache {
                download_files: HashMap::new(),
            }),
            tx,
            rx,
        })
    }
}

pub async fn solve<R: Resolver>(profile: Profile) -> Result<Vec<DownloadData>, Error<R>> {
    let (res_tx, data_rx) = mpsc::channel();
    let (data_tx, res_rx) = mpsc::channel();

    let handle = start_resolver_thread::<R>(&profile, res_tx, res_rx);
    process_data_requests(handle, profile.filters, data_tx, data_rx).await
}

fn start_resolver_thread<R: Resolver>(
    profile: &Profile,
    tx: mpsc::Sender<ModIdentifier>,
    rx: mpsc::Receiver<DataPacket<R>>,
) -> thread::JoinHandle<Result<Vec<DownloadData>, Error<R>>> {
    let dependencies: DataDependencies<R> = profile
        .mods
        .iter()
        .map(|mod_| (Package::Id(mod_.identifier.clone()), VersionRange::full()))
        .collect();

    std::thread::spawn(|| {
        let dep_provider = DependencyProvider::<R>::new(dependencies, tx, rx)?;
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
    tx: mpsc::Sender<DataPacket<R>>,
    rx: mpsc::Receiver<ModIdentifier>,
) -> Result<Vec<DownloadData>, Error<R>> {
    let client = Client::new();
    let mut futs = vec![];

    loop {
        if resolver_thread.is_finished() {
            break;
        }

        while let Ok(id) = rx.try_recv() {
            println!("GET {id}");
            let fut = fetch_data::<R>(client.clone(), id.clone(), filters.clone());
            let fut = fut.map(|data| data.map(|d| (id, d)));
            futs.push(tokio::task::spawn(fut));
        }

        let mut new_futs = vec![];

        for fut in futs {
            if fut.is_finished() {
                let data = fut.await.map_err(ErrorIn::JoinThread)??;
                tx.send(data).map_err(Error::ThreadSend)?;
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
                println!("dependencies: {:?}", dependencies);
                Ok(pubgrub::Dependencies::Available(dependencies))
            }
            None => Ok(pubgrub::Dependencies::Available(dbg!(
                pubgrub::DependencyConstraints::default()
            ))),
        }
    }

    fn should_cancel(&self) -> Result<(), Self::Err> {
        println!("\nshould_cancel()");
        Ok(())
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
    download_files: HashMap<ModIdentifier, Data<R>>,
}

enum Data<R: Resolver> {
    Pending,
    Resolved(Rc<Vec<ResolutionData<R>>>),
}

impl<R: Resolver> DependencyProvider<R> {
    /// Fetch a list of possible downloads for a mod.
    fn fetch_download_files(
        &self,
        id: &ModIdentifier,
    ) -> Result<Rc<Vec<ResolutionData<R>>>, ErrorIn> {
        println!("fetch_download_files({id})");
        let mut cache = self.cache.borrow_mut();
        let files = self.fetch_download_files_inner(&mut cache, id)?;

        // Prefetch data
        for data in files.as_ref() {
            for id in &data.data.dependencies {
                self.prefetch_download_files(&mut cache, id)?;
            }
            for id in &data.data.conflicts {
                self.prefetch_download_files(&mut cache, id)?;
            }
        }

        Ok(files)
    }

    fn fetch_download_files_inner(
        &self,
        cache: &mut Cache<R>,
        id: &ModIdentifier,
    ) -> Result<Rc<Vec<ResolutionData<R>>>, ErrorIn> {
        match cache.download_files.entry(id.clone()) {
            Entry::Occupied(mut entry) => match entry.get_mut() {
                Data::Pending => self.wait_for(cache, id),
                Data::Resolved(data) => Ok(data.clone()),
            },
            Entry::Vacant(_) => {
                self.request_id(cache, id)?;
                self.wait_for(cache, id)
            }
        }
    }

    /// Make a request to get mod metadata and keep it in the cache so future requests for this mod don't have to wait.
    fn prefetch_download_files(
        &self,
        cache: &mut Cache<R>,
        id: &ModIdentifier,
    ) -> Result<(), ErrorIn> {
        if !cache.download_files.contains_key(id) {
            self.request_id(cache, id)?;
        }
        Ok(())
    }

    fn wait_for(
        &self,
        cache: &mut Cache<R>,
        id: &ModIdentifier,
    ) -> Result<Rc<Vec<ResolutionData<R>>>, ErrorIn> {
        println!("WAIT FOR {id}");
        loop {
            let (recv_id, data) = self.rx.recv()?;
            println!("RECV {recv_id}");
            let data = Rc::new(data);
            let is_match = recv_id == *id;
            self.store_in_cache(cache, recv_id, data.clone());
            if is_match {
                break Ok(data);
            }
        }
    }

    fn request_id(&self, cache: &mut Cache<R>, id: &ModIdentifier) -> Result<(), ErrorIn> {
        self.tx.send(id.clone())?;
        cache.download_files.insert(id.clone(), Data::Pending);
        Ok(())
    }

    fn store_in_cache(
        &self,
        cache: &mut Cache<R>,
        id: ModIdentifier,
        data: Rc<Vec<ResolutionData<R>>>,
    ) {
        cache.download_files.insert(id, Data::Resolved(data));
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
    SendRequest(#[from] mpsc::SendError<ModIdentifier>),
    #[error("resolver: {0}")]
    RecvRequest(#[from] mpsc::RecvError),
}
