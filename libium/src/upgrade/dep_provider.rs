mod structs;

use futures_executor::ThreadPool;
use futures_util::{
    future::{try_join_all, RemoteHandle},
    task::SpawnExt,
};
use pubgrub::{PubGrubError, VersionSet};
use reqwest::Client;
use std::{
    cell::RefCell,
    cmp,
    collections::{hash_map::Entry, HashMap},
    io,
    rc::Rc,
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

pub struct DependencyProvider<R: Resolver> {
    profile: Profile,
    pool: ThreadPool,
    cache: RefCell<Cache<R>>,
}

impl<R: Resolver> DependencyProvider<R> {
    pub fn new(profile: Profile) -> Result<Self, ErrorIn> {
        Ok(Self {
            profile,
            pool: ThreadPool::new().map_err(ErrorIn::CreateThreadPool)?,
            cache: RefCell::new(Cache {
                download_files: HashMap::new(),
            }),
        })
    }

    pub fn solve(self) -> Result<Vec<DownloadData>, Error<R>> {
        let pkgs = pubgrub::resolve(&self, Package::Root, Version::Root)?;
        let mut ret = Vec::with_capacity(pkgs.len());

        for (pkg, version) in pkgs {
            let id = match pkg {
                Package::Root => continue,
                Package::Id(id) => id,
            };

            let data = self.fetch_download_files(&id)?;
            let Some(data) = get_package_version(&data, &version)? else {
                continue;
            };

            ret.push(data.data.clone());
        }

        Ok(ret)
    }
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
        _package: &Self::P,
        _range: &Self::VS,
        package_conflicts_counts: &pubgrub::PackageResolutionStatistics,
    ) -> Self::Priority {
        package_conflicts_counts.conflict_count()
    }

    fn choose_version(
        &self,
        package: &Self::P,
        range: &Self::VS,
    ) -> Result<Option<Self::V>, Self::Err> {
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
        let id = match package {
            Package::Root => {
                let mut dependencies = pubgrub::DependencyConstraints::default();
                for mod_ in &self.profile.mods {
                    dependencies.insert(Package::Id(mod_.identifier.clone()), VersionRange::full());
                }
                return Ok(pubgrub::Dependencies::Available(dependencies));
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
    download_files: HashMap<ModIdentifier, Data<R>>,
}

/// A thread handle for an api request.
type RequestHandle<R> = RemoteHandle<Result<Vec<ResolutionData<R>>, ErrorIn>>;

enum Data<R: Resolver> {
    Pending(RequestHandle<R>),
    Resolved(Rc<Vec<ResolutionData<R>>>),
}

impl<R: Resolver> DependencyProvider<R> {
    /// Fetch a list of possible downloads for a mod.
    fn fetch_download_files(
        &self,
        id: &ModIdentifier,
    ) -> Result<Rc<Vec<ResolutionData<R>>>, ErrorIn> {
        let files = self.fetch_download_files_inner(id)?;

        // Prefetch data
        for data in files.as_ref() {
            for id in &data.data.dependencies {
                self.prefetch_download_files(id)?;
            }
            for id in &data.data.conflicts {
                self.prefetch_download_files(id)?;
            }
        }

        Ok(files)
    }

    fn fetch_download_files_inner(
        &self,
        id: &ModIdentifier,
    ) -> Result<Rc<Vec<ResolutionData<R>>>, ErrorIn> {
        match self.cache.borrow_mut().download_files.entry(id.clone()) {
            Entry::Occupied(mut entry) => Ok(match entry.get_mut() {
                Data::Pending(handle) => Rc::new(futures_executor::block_on(handle)?),
                Data::Resolved(data) => data.clone(),
            }),
            Entry::Vacant(entry) => {
                let handle = self.fetch_handle(id)?;
                let data = Rc::new(futures_executor::block_on(handle)?);
                entry.insert(Data::Resolved(data.clone()));
                Ok(data)
            }
        }
    }

    /// Make a request to get mod metadata and keep it in the cache so future requests for this mod don't have to wait.
    fn prefetch_download_files(&self, id: &ModIdentifier) -> Result<(), ErrorIn> {
        match self.cache.borrow_mut().download_files.entry(id.clone()) {
            Entry::Occupied(_) => Ok(()),
            Entry::Vacant(entry) => {
                let handle = self.fetch_handle(id)?;
                entry.insert(Data::Pending(handle));
                Ok(())
            }
        }
    }

    /// Make a request to get mod metadata and return a thread handle to the requesting process.
    fn fetch_handle(&self, id: &ModIdentifier) -> Result<RequestHandle<R>, ErrorIn> {
        let fut = fetch_data(Client::new(), id.clone(), self.profile.filters.clone());
        Ok(self.pool.spawn_with_handle(fut)?)
    }
}

async fn fetch_data<R: Resolver>(
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

    try_join_all(futs).await
}

#[derive(thiserror::Error, Debug)]
pub enum Error<R: Resolver> {
    Internal(#[from] ErrorIn),
    Resolution(#[from] PubGrubError<DependencyProvider<R>>),
}

#[derive(thiserror::Error, Debug)]
pub enum ErrorIn {
    #[error("thread pool spawn: {0}")]
    ThreadPoolSpawn(#[from] futures_util::task::SpawnError),
    #[error("download: {0}")]
    Download(#[from] mod_downloadable::Error),
    #[error("upgrade: {0}")]
    Upgrade(#[from] upgrade::Error),
    #[error("create thread pool: {0}")]
    CreateThreadPool(io::Error),
}
