use std::{
    cmp,
    collections::HashMap,
    fmt::{self, Debug, Display},
    future::Future,
    rc::Rc,
};

use derive_more::Display;
use mod_version::{
    fabric::{
        self,
        version::{FabricVersion, FabricVersionRange},
    },
    forge::{
        self,
        version::{ForgeVersion, ForgeVersionRange},
    },
};
use pubgrub::VersionSet;
use reqwest::Client;
use url::Url;

use crate::{
    config::structs::ModIdentifier,
    upgrade::{self, fetch_fabric_manifest, fetch_forge_manifest, DownloadData},
};

pub trait Resolver: 'static {
    type Range: RangeLike;
    type Manifest: ManifestLike<R = Self> + Send;
}

pub struct Forge;

impl Resolver for Forge {
    type Range = ForgeVersionRange;
    type Manifest = forge::ModsToml;
}

pub struct Fabric;

impl Resolver for Fabric {
    type Range = FabricVersionRange;
    type Manifest = fabric::ModJson;
}

#[derive(Hash, PartialEq, Eq, Clone, Debug, Display)]
pub enum Package {
    Root,
    Id(ModIdentifier),
}

#[derive(PartialEq, Eq, PartialOrd, Ord, Clone, Display, Debug)]
pub enum Version<V> {
    Root,
    Version(V),
}

#[derive(PartialEq, Eq, Clone, Debug, PartialOrd, Ord)]
pub enum VersionRange<R: RangeLike> {
    Eq(R),
    Not(Rc<VersionRange<R>>),
    And(Rc<(VersionRange<R>, VersionRange<R>)>),
}

impl<R: RangeLike> pubgrub::VersionSet for VersionRange<R> {
    type V = Version<R::Version>;

    fn empty() -> Self {
        Self::Not(Rc::new(Self::Eq(R::empty())))
    }

    fn singleton(v: Self::V) -> Self {
        match v {
            Version::Root => todo!(),
            Version::Version(v) => Self::Eq(R::only(v)),
        }
    }

    fn complement(&self) -> Self {
        Self::Not(Rc::new(self.clone()))
    }

    fn intersection(&self, other: &Self) -> Self {
        Self::And(Rc::new((self.clone(), other.clone())))
    }

    fn contains(&self, v: &Self::V) -> bool {
        match self {
            VersionRange::Eq(r) => match v {
                Version::Root => true,
                Version::Version(v) => r.matches(v),
            },
            VersionRange::Not(range) => !range.contains(v),
            VersionRange::And(x) => x.0.contains(v) && x.1.contains(v),
        }
    }
}

impl<R: RangeLike> fmt::Display for VersionRange<R> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            VersionRange::Eq(r) => write!(f, "{r}"),
            VersionRange::Not(r) => write!(f, "not {r}"),
            VersionRange::And(x) => write!(f, "({} and {})", x.0, x.1),
        }
    }
}

pub trait VersionLike: Ord + Clone + Display + Debug {}
impl VersionLike for ForgeVersion {}
impl VersionLike for FabricVersion {}

pub trait RangeLike: Eq + Clone + Display + Debug {
    type Version: VersionLike;
    fn empty() -> Self;
    fn only(v: Self::Version) -> Self;
    fn matches(&self, v: &Self::Version) -> bool;
}
impl RangeLike for ForgeVersionRange {
    type Version = ForgeVersion;
    fn empty() -> Self {
        Self::empty()
    }
    fn only(v: Self::Version) -> Self {
        Self::only(v)
    }
    fn matches(&self, v: &Self::Version) -> bool {
        self.matches(v)
    }
}
impl RangeLike for FabricVersionRange {
    type Version = FabricVersion;
    fn empty() -> Self {
        Self::empty()
    }
    fn only(v: Self::Version) -> Self {
        Self::only(v)
    }
    fn matches(&self, v: &Self::Version) -> bool {
        self.matches(v)
    }
}

pub trait ManifestLike {
    type R: Resolver;

    fn fetch_manifest(
        client: &Client,
        url: &Url,
        filesize: Option<usize>,
    ) -> impl Future<Output = upgrade::Result<Option<Self>>> + Send
    where
        Self: Sized;

    fn compare(&self, b: &Self) -> cmp::Ordering;
    fn matches(&self, range: &VersionRange<<Self::R as Resolver>::Range>) -> bool;
    fn version(&self) -> Option<Version<<<Self::R as Resolver>::Range as RangeLike>::Version>>;
}

impl ManifestLike for forge::ModsToml {
    type R = Forge;

    async fn fetch_manifest(
        client: &Client,
        url: &Url,
        filesize: Option<usize>,
    ) -> upgrade::Result<Option<Self>> {
        fetch_forge_manifest(client, url, filesize).await
    }

    fn compare(&self, b: &Self) -> cmp::Ordering {
        // If a version is higher it increases, if a version is lower it decreases,
        // so it finds what is the most up-to-date.
        let mut score: i32 = 0;

        let mods: HashMap<&String, &forge::Mod> =
            self.mods.iter().map(|mod_| (&mod_.mod_id, mod_)).collect();

        for b in &b.mods {
            let Some(a) = mods.get(&b.mod_id) else {
                continue;
            };

            match a.version.cmp(&b.version) {
                cmp::Ordering::Less => score -= 1,
                cmp::Ordering::Equal => {}
                cmp::Ordering::Greater => score += 1,
            }
        }

        score.cmp(&0)
    }

    fn matches(&self, range: &VersionRange<<Self::R as Resolver>::Range>) -> bool {
        self.mods
            .iter()
            .any(|mod_| range.contains(&Version::Version(mod_.version.clone())))
    }

    fn version(&self) -> Option<Version<<<Self::R as Resolver>::Range as RangeLike>::Version>> {
        self.mods
            .first()
            .map(|x| Version::Version(x.version.clone()))
    }
}

impl ManifestLike for fabric::ModJson {
    type R = Fabric;

    async fn fetch_manifest(
        client: &Client,
        url: &Url,
        filesize: Option<usize>,
    ) -> upgrade::Result<Option<Self>>
    where
        Self: Sized,
    {
        fetch_fabric_manifest(client, url, filesize).await
    }

    fn compare(&self, b: &Self) -> cmp::Ordering {
        self.version.cmp(&b.version)
    }

    fn matches(&self, range: &VersionRange<<Self::R as Resolver>::Range>) -> bool {
        range.contains(&Version::Version(self.version.clone()))
    }

    fn version(&self) -> Option<Version<<<Self::R as Resolver>::Range as RangeLike>::Version>> {
        Some(Version::Version(self.version.clone()))
    }
}

pub struct ResolutionData<R: Resolver> {
    pub data: DownloadData,
    pub manifest: Option<R::Manifest>,
}
