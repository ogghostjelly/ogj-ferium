use crate::add;

use derive_more::derive::Display;
use ferinth::structures::project::ProjectType;
use semver::Prerelease;
use serde::{de::Visitor, Deserialize, Serialize};
use std::{
    collections::{hash_map::Entry, HashMap},
    env::current_dir,
    fmt,
    fs::File,
    io,
    marker::PhantomData,
    ops::{Deref, DerefMut},
    path::{Component, Path, PathBuf},
    str::FromStr,
};
use thiserror::Error;
use url::Url;
use zip::{result::ZipError, ZipArchive};

use super::{options::OptionsOverrides, read_profile, write_profile};

#[derive(Deserialize, Serialize, Debug, Default)]
#[serde(rename_all = "kebab-case")]
pub struct Config {
    #[serde(skip_serializing_if = "is_zero")]
    #[serde(default)]
    pub active_profile: usize,

    #[serde(skip_serializing_if = "Vec::is_empty")]
    #[serde(default)]
    pub profiles: Vec<ProfileItem>,
}

#[derive(Deserialize, Serialize, Debug)]
#[serde(rename_all = "kebab-case")]
pub struct ProfileItem {
    /// The profile source
    pub profile: ProfileSource,
    #[serde(flatten)]
    pub config: ProfileItemConfig,
}

#[derive(Deserialize, Serialize, Debug)]
#[serde(rename_all = "kebab-case")]
pub struct ProfileItemConfig {
    /// The unique name of the profile.
    pub name: String,
    /// The `.minecraft` directory to download mod files to
    pub minecraft_dir: PathBuf,
}

/// The path to the profile `.toml` file or the profile data itself.
#[derive(Serialize, Deserialize, Debug)]
#[serde(untagged)]
pub enum ProfileSource {
    Path(PathBuf),
    Embedded(Box<Profile>),
}

macro_rules! fn_get_body {
    ( $self:expr, $ctor:ident ) => {
        match $self {
            ProfileSource::Path(path) => {
                let Some(profile) = read_profile(&path)? else {
                    return Ok(None);
                };

                Ok(Some($ctor::Path(path, Box::new(profile))))
            }
            ProfileSource::Embedded(profile) => Ok(Some($ctor::Embedded(profile))),
        }
    };
}

impl ProfileSource {
    pub fn get(&self) -> Result<Option<ProfileSourceRef<'_>>, io::Error> {
        fn_get_body!(self, ProfileSourceRef)
    }

    pub fn get_mut(&mut self) -> Result<Option<ProfileSourceMut<'_>>, io::Error> {
        fn_get_body!(self, ProfileSourceMut)
    }
}

pub enum ProfileSourceMut<'a> {
    Path(&'a PathBuf, Box<Profile>),
    Embedded(&'a mut Profile),
}

impl<'a> ProfileSourceMut<'a> {
    pub fn to_ref(self) -> ProfileSourceRef<'a> {
        match self {
            ProfileSourceMut::Path(path, profile) => ProfileSourceRef::Path(path, profile),
            ProfileSourceMut::Embedded(profile) => ProfileSourceRef::Embedded(profile),
        }
    }
}

pub enum ProfileSourceRef<'a> {
    Path(&'a PathBuf, Box<Profile>),
    Embedded(&'a Profile),
}

impl<'a> DerefMut for ProfileSourceMut<'a> {
    fn deref_mut(&mut self) -> &mut Self::Target {
        match self {
            ProfileSourceMut::Path(_, profile) => profile,
            ProfileSourceMut::Embedded(profile) => profile,
        }
    }
}

macro_rules! impl_profile_source_ref {
    ( $t:ty ) => {
        impl<'a> Deref for $t {
            type Target = Profile;

            fn deref(&self) -> &Self::Target {
                match self {
                    Self::Path(_, profile) => profile,
                    Self::Embedded(profile) => profile,
                }
            }
        }

        impl<'a> $t {
            pub fn write(self) -> Result<(), io::Error> {
                match self {
                    Self::Path(path, profile) => write_profile(path, &profile),
                    Self::Embedded(_) => Ok(()),
                }
            }
        }
    };
}

impl_profile_source_ref!(ProfileSourceRef<'a>);
impl_profile_source_ref!(ProfileSourceMut<'a>);

impl ProfileItem {
    pub fn new(profile: ProfileSource, name: String, minecraft_dir: PathBuf) -> Self {
        Self {
            profile,
            config: ProfileItemConfig {
                name,
                minecraft_dir,
            },
        }
    }

    pub fn infer_path(path: Option<PathBuf>, name: &str) -> std::io::Result<PathBuf> {
        let path = match path {
            Some(path) => path,
            None => {
                let mut path = current_dir()?.join(name);
                path.set_extension("toml");
                path
            }
        };

        let _ = File::create(&path)?;

        let path = path.canonicalize()?;

        Ok(path)
    }
}

const fn is_zero(n: &usize) -> bool {
    *n == 0
}

#[derive(Serialize, Deserialize, Debug, Clone)]
#[serde(rename_all = "kebab-case")]
pub struct Profile {
    #[serde(flatten)]
    pub filters: Filters,
    #[serde(default, skip_serializing_if = "Vec::is_empty")]
    pub imports: Vec<ProfileImport>,
    #[serde(default, skip_serializing_if = "OptionsOverrides::is_empty")]
    pub options: OptionsOverrides,
    #[serde(default, skip_serializing_if = "Option::is_none")]
    pub overrides: Option<Overrides>,
    #[serde(default, skip_serializing_if = "HashMap::is_empty")]
    pub mods: HashMap<String, Source>,
    #[serde(default, skip_serializing_if = "HashMap::is_empty")]
    pub shaders: HashMap<String, Source>,
    #[serde(default, skip_serializing_if = "HashMap::is_empty")]
    pub modpacks: HashMap<String, Source>,
    #[serde(default, skip_serializing_if = "HashMap::is_empty")]
    pub resourcepacks: HashMap<String, Source>,
}

#[derive(Serialize, Deserialize, Debug, Clone)]
#[serde(untagged)]
pub enum ProfileImport {
    Short(ProfilePath),
    Long {
        src: ProfilePath,
        hash: Option<String>,
    },
}

/// The source of a profile import, e.g './path/to/profile.toml' or 'https://example.com/profile.toml'
#[derive(Serialize, Deserialize, Debug, Clone)]
#[serde(untagged)]
pub enum ProfilePath {
    Url(Url),
    Path(PathBuf),
}

/// The parent of a profile path, e.g './path/to' or 'https://example.com'.
/// Used as the current working directory of a profile.
#[derive(Clone)]
pub enum SrcPath {
    Url(Url),
    Path(PathBuf),
}

impl SrcPath {
    pub fn join(&self, path: &Path) -> Result<Self, UrlJoinError> {
        Ok(match self {
            SrcPath::Url(url) => Self::Url(join_url(url.clone(), path)?),
            SrcPath::Path(path_buf) => Self::Path(path_buf.join(path)),
        })
    }
}

pub fn join_url(mut url: Url, path: &Path) -> Result<Url, UrlJoinError> {
    let Ok(mut segs) = url.path_segments_mut() else {
        return Err(UrlJoinError::UrlCannotBeBase(url));
    };

    for c in path.components() {
        match c {
            Component::RootDir => segs.clear(),
            Component::CurDir => &mut segs,
            Component::ParentDir => segs.pop(),
            Component::Normal(s) => match s.to_str() {
                Some(s) => segs.push(s),
                None => return Err(UrlJoinError::InvalidUtf8Path(path.to_path_buf())),
            },
            Component::Prefix(_) => return Err(UrlJoinError::PathWithPrefix(path.to_path_buf())),
        };
    }

    drop(segs);
    Ok(url)
}

#[derive(Error, Debug)]
pub enum UrlJoinError {
    #[error("malformed url: cannot be base: '{0}'")]
    UrlCannotBeBase(Url),
    #[error("path is not valid utf-8: '{0}'")]
    InvalidUtf8Path(PathBuf),
    #[error("paths in url imported profiles cannot contain prefix components: '{0}'")]
    PathWithPrefix(PathBuf),
}

#[derive(Serialize, Deserialize, Debug, Clone)]
#[serde(untagged, rename_all = "kebab-case")]
pub enum Overrides {
    Directory(PathBuf),
    Expanded {
        #[serde(default, skip_serializing_if = "Option::is_none")]
        path: Option<PathBuf>,
        #[serde(flatten, default, skip_serializing_if = "HashMap::is_empty")]
        files: HashMap<PathBuf, String>,
    },
}

impl Profile {
    pub fn overrides_path(&self) -> Option<&Path> {
        self.overrides
            .as_ref()
            .and_then(|overrides| match overrides {
                Overrides::Directory(path) => Some(path.as_path()),
                Overrides::Expanded { path, .. } => path.as_deref(),
            })
    }

    pub fn overrides_files(&self) -> Option<&HashMap<PathBuf, String>> {
        self.overrides
            .as_ref()
            .and_then(|overrides| match overrides {
                Overrides::Directory(_) => None,
                Overrides::Expanded { files, .. } => Some(files),
            })
    }
}

impl Profile {
    /// A simple contructor that automatically deals with converting to filters
    pub fn new(versions: Option<Vec<Version>>, mod_loader: ModLoader) -> Self {
        Self {
            filters: Filters {
                versions,
                mod_loaders: match mod_loader {
                    ModLoader::Fabric | ModLoader::Quilt => {
                        Some(vec![ModLoader::Fabric, ModLoader::Quilt])
                    }
                    mod_loader => Some(vec![mod_loader]),
                },
                ..Filters::empty()
            },
            imports: Vec::new(),
            options: OptionsOverrides::default(),
            overrides: None,
            mods: HashMap::new(),
            shaders: HashMap::new(),
            modpacks: HashMap::new(),
            resourcepacks: HashMap::new(),
        }
    }

    pub fn map_mut(&mut self, kind: SourceKind) -> &mut HashMap<String, Source> {
        match kind {
            SourceKind::Mods => &mut self.mods,
            SourceKind::Resourcepacks => &mut self.resourcepacks,
            SourceKind::Shaders => &mut self.shaders,
            SourceKind::Modpacks => &mut self.modpacks,
        }
    }

    pub fn map(&self, kind: SourceKind) -> &HashMap<String, Source> {
        match kind {
            SourceKind::Mods => &self.mods,
            SourceKind::Resourcepacks => &self.resourcepacks,
            SourceKind::Shaders => &self.shaders,
            SourceKind::Modpacks => &self.modpacks,
        }
    }

    pub fn push(&mut self, kind: SourceKind, id: String, source: Source) -> Result<(), add::Error> {
        let map = self.map_mut(kind);

        for source_id in source.ids() {
            let has_duplicates = map
                .iter()
                .flat_map(|(_, source)| source.ids())
                .any(|mod_id| mod_id == source_id);
            if has_duplicates {
                return Err(add::Error::AlreadyAdded);
            }
        }

        match map.entry(id.clone()) {
            Entry::Occupied(e) => {
                let source = match e.remove() {
                    e @ Source::Single(_) | e @ Source::Detailed { .. } => {
                        Source::Multiple(vec![e, source])
                    }
                    Source::Multiple(mut sources) => {
                        sources.push(source);
                        Source::Multiple(sources)
                    }
                };

                if map.contains_key(&id) {
                    return Err(add::Error::AlreadyAdded);
                }

                map.insert(id.clone(), source);
            }
            Entry::Vacant(e) => {
                let _ = e.insert(source);
            }
        }

        Ok(())
    }

    pub fn ids(&self) -> impl Iterator<Item = (SourceKind, &SourceId)> {
        let mod_ids = self
            .mods
            .iter()
            .flat_map(|(_, source)| source.ids().map(|id| (SourceKind::Mods, id)));
        let resourcepack_ids = self
            .resourcepacks
            .iter()
            .flat_map(|(_, source)| source.ids().map(|id| (SourceKind::Resourcepacks, id)));
        let shaderpack_ids = self
            .shaders
            .iter()
            .flat_map(|(_, source)| source.ids().map(|id| (SourceKind::Shaders, id)));
        let modpack_ids = self
            .modpacks
            .iter()
            .flat_map(|(_, source)| source.ids().map(|id| (SourceKind::Modpacks, id)));
        mod_ids
            .chain(resourcepack_ids)
            .chain(shaderpack_ids)
            .chain(modpack_ids)
    }

    pub fn top_sources(&self) -> impl Iterator<Item = (SourceKind, (&String, &Source))> {
        let mod_ids = self.mods.iter().map(|id| (SourceKind::Mods, id));
        let resourcepack_ids = self
            .resourcepacks
            .iter()
            .map(|id| (SourceKind::Resourcepacks, id));
        let shaderpack_ids = self.shaders.iter().map(|id| (SourceKind::Shaders, id));
        let modpack_ids = self.modpacks.iter().map(|id| (SourceKind::Modpacks, id));
        mod_ids
            .chain(resourcepack_ids)
            .chain(shaderpack_ids)
            .chain(modpack_ids)
    }
}

#[derive(Serialize, Deserialize, Debug, Clone)]
#[serde(untagged)]
pub enum Source {
    Single(SourceId),
    Multiple(Vec<Source>),
    Detailed {
        #[serde(flatten)]
        filters: Filters,
        src: Box<Source>,
    },
}

impl Source {
    pub fn filters(&self) -> Option<&Filters> {
        match self {
            Source::Single(_) => None,
            Source::Multiple(_) => None,
            Source::Detailed { filters, .. } => Some(filters),
        }
    }

    pub fn ids(&self) -> SourceIdsIter<'_> {
        SourceIdsIter { srcs: vec![self] }
    }

    pub fn each_sources<'a>(
        &'a self,
        filters: Vec<&'a Filters>,
        mut f: impl FnMut(Vec<&'a Filters>, &'a SourceId),
    ) -> impl FnMut(Vec<&'a Filters>, &'a SourceId) {
        match self {
            Source::Single(source_id) => f(filters, source_id),
            Source::Multiple(sources) => {
                for source in sources {
                    f = source.each_sources(filters.clone(), f);
                }
            }
            Source::Detailed {
                filters: new_filter,
                src,
            } => {
                let mut filters = filters;
                filters.push(new_filter);
                f = src.each_sources(filters, f);
            }
        }
        f
    }

    pub fn github(owner: String, repo: String, filters: Filters) -> Self {
        Self::from_id(SourceId::Github(owner, repo), filters)
    }

    pub fn curseforge(id: i32, filters: Filters) -> Self {
        Self::from_id(SourceId::Curseforge(id), filters)
    }

    pub fn modrinth(id: String, filters: Filters) -> Self {
        Self::from_id(SourceId::Modrinth(id), filters)
    }

    pub fn url(id: Url, hash: String, mut filters: Filters) -> Self {
        filters.hashes = Some(match filters.hashes {
            Some(mut hashes) => {
                hashes.push(hash);
                hashes
            }
            None => vec![hash],
        });

        Self::from_id(SourceId::Url(id), filters)
    }

    pub fn from_id(source_id: SourceId, filters: Filters) -> Self {
        let source = Self::Single(source_id);

        if filters.is_empty() {
            source
        } else {
            Self::Detailed {
                filters,
                src: Box::new(source),
            }
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum SourceId {
    Curseforge(i32),
    Modrinth(String),
    Github(String, String),
    File(PathBuf),
    Url(Url),

    PinnedCurseforge(i32, i32),
    PinnedModrinth(String, String),
    PinnedGithub((String, String), i32),
}

impl fmt::Display for SourceId {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            SourceId::Curseforge(id) => write!(f, "cf:{id}"),
            SourceId::Modrinth(id) => write!(f, "mr:{id}"),
            SourceId::Github(owner, repo) => write!(f, "gh:{owner}/{repo}"),
            SourceId::File(file) => write!(f, "file:{}", file.display()),
            SourceId::Url(url) => write!(f, "url:{url}"),
            SourceId::PinnedCurseforge(id, pin) => write!(f, "cf:{id}*{pin}"),
            SourceId::PinnedModrinth(id, pin) => write!(f, "mr:{id}*{pin}"),
            SourceId::PinnedGithub((owner, repo), pin) => write!(f, "gh:{owner}/{repo}*{pin}"),
        }
    }
}

impl Serialize for SourceId {
    fn serialize<S>(&self, serializer: S) -> Result<S::Ok, S::Error>
    where
        S: serde::Serializer,
    {
        serializer.serialize_str(&self.to_string())
    }
}

impl<'de> Deserialize<'de> for SourceId {
    fn deserialize<D>(deserializer: D) -> Result<Self, D::Error>
    where
        D: serde::Deserializer<'de>,
    {
        deserializer.deserialize_str(SourceTagVisitor)
    }
}

struct SourceTagVisitor;

impl<'de> Visitor<'de> for SourceTagVisitor {
    type Value = SourceId;

    fn expecting(&self, formatter: &mut std::fmt::Formatter) -> std::fmt::Result {
        write!(formatter, "a source tag e.g \"modrinth:abc\" or \"cf:123\"")
    }

    fn visit_str<E>(self, v: &str) -> Result<Self::Value, E>
    where
        E: serde::de::Error,
    {
        let Some((tag, id)) = v.split_once(':') else {
            return Err(E::custom(format!(
                "missing `:` separator in source tag {v:?}"
            )));
        };

        fn parse_with_pin<'inp, Id, Pin>(
            inp: &'inp str,
            f_id: impl Fn(&'inp str) -> Id,
            f_pin: impl Fn(&'inp str) -> Pin,
        ) -> (Id, Option<Pin>) {
            match inp.rsplit_once('*') {
                Some((id, pin)) => (f_id(id), Some(f_pin(pin))),
                None => (f_id(inp), None),
            }
        }

        match tag {
            "cf" | "curseforge" => match parse_with_pin(id, |id| id.parse(), |pin| pin.parse()) {
                (Ok(id), None) => Ok(SourceId::Curseforge(id)),
                (Ok(id), Some(Ok(pin))) => Ok(SourceId::PinnedCurseforge(id, pin)),
                (Err(e), _) | (_, Some(Err(e))) => Err(E::custom(e)),
            },
            "mr" | "modrinth" => match parse_with_pin(id, |id| id, |pin| pin) {
                (id, None) => Ok(SourceId::Modrinth(id.to_owned())),
                (id, Some(pin)) => Ok(SourceId::PinnedModrinth(id.to_owned(), pin.to_owned())),
            },
            "file" => Ok(SourceId::File(id.into())),
            "http" | "https" | "127.0.0.1" | "localhost" => match Url::parse(v) {
                Ok(url) => Ok(SourceId::Url(url)),
                Err(e) => Err(E::custom(e)),
            },
            "url" => match Url::parse(id) {
                Ok(url) => Ok(SourceId::Url(url)),
                Err(e) => Err(E::custom(e)),
            },
            "gh" | "github" => {
                let parsed = parse_with_pin(
                    id,
                    |id| {
                        let Some(index) = id.find('/') else {
                            return Err(E::custom(format!(
                                "missing `/` separator in github source {tag}:{id}"
                            )));
                        };

                        let (owner, repo) = id.split_at(index);
                        let repo = &repo[1..];

                        Ok((owner, repo))
                    },
                    |pin| pin.parse(),
                );

                match (parsed.0?, parsed.1) {
                    ((owner, repo), None) => {
                        Ok(SourceId::Github(owner.to_owned(), repo.to_owned()))
                    }
                    ((owner, repo), Some(Ok(pin))) => Ok(SourceId::PinnedGithub(
                        (owner.to_owned(), repo.to_owned()),
                        pin,
                    )),
                    (_, Some(Err(e))) => Err(E::custom(e)),
                }
            }
            _ => Err(E::unknown_variant(
                tag,
                &["mr", "modrinth", "cf", "curseforge", "gh", "github"],
            )),
        }
    }
}

/// Helper to recursively iterate through every id in a source.
pub struct SourceIdsIter<'a> {
    srcs: Vec<&'a Source>,
}

impl<'a> SourceIdsIter<'a> {
    fn next_source(&mut self, source: &'a Source) -> Option<&'a SourceId> {
        match source {
            Source::Single(id) => Some(id),
            Source::Multiple(sources) => {
                for source in sources.iter().rev() {
                    self.srcs.push(source);
                }
                self.next()
            }
            Source::Detailed { src, .. } => self.next_source(src),
        }
    }
}

impl<'a> Iterator for SourceIdsIter<'a> {
    type Item = &'a SourceId;

    fn next(&mut self) -> Option<Self::Item> {
        let value = self.srcs.pop()?;
        self.next_source(value)
    }
}

#[derive(Debug, Clone, Copy)]
pub enum SourceKind {
    Mods,
    Resourcepacks,
    Shaders,
    Modpacks,
}

impl SourceKind {
    pub const ARRAY: &[Self] = &[
        Self::Mods,
        Self::Resourcepacks,
        Self::Shaders,
        Self::Modpacks,
    ];

    pub fn dirname(&self) -> Option<&'static str> {
        match self {
            SourceKind::Mods => Some("mods"),
            SourceKind::Resourcepacks => Some("resourcepacks"),
            SourceKind::Shaders => Some("shaderpacks"),
            SourceKind::Modpacks => None,
        }
    }

    pub fn directory(&self, inferred_kind: Option<SourceKindWithModpack>) -> &'static Path {
        let kind = match inferred_kind {
            Some(kind) => &kind.to_kind(),
            None => self,
        };

        Path::new(kind.dirname().unwrap_or(""))
    }
}

pub fn check_exists<R: io::Read + io::Seek>(
    archive: &mut ZipArchive<R>,
    name: &str,
) -> io::Result<bool> {
    match archive.by_name(name) {
        Ok(_) => Ok(true),
        Err(ZipError::FileNotFound) => Ok(false),
        Err(e) => Err(e.into()),
    }
}

#[derive(Debug, Clone, Copy)]
pub enum SourceKindWithModpack {
    Mods,
    Resourcepacks,
    Shaders,
    ModpacksModrinth,
    ModpacksCurseforge,
}

impl From<SourceKindWithModpack> for SourceKind {
    fn from(value: SourceKindWithModpack) -> Self {
        match value {
            SourceKindWithModpack::Mods => Self::Mods,
            SourceKindWithModpack::Resourcepacks => Self::Resourcepacks,
            SourceKindWithModpack::Shaders => Self::Shaders,
            SourceKindWithModpack::ModpacksModrinth => Self::Modpacks,
            SourceKindWithModpack::ModpacksCurseforge => Self::Modpacks,
        }
    }
}

impl SourceKindWithModpack {
    pub fn to_kind(self) -> SourceKind {
        self.into()
    }

    pub fn from_cf_class_id(class_id: i32) -> Option<Self> {
        match class_id {
            12 => Some(Self::Resourcepacks),
            6 => Some(Self::Mods),
            6552 => Some(Self::Shaders),
            4471 => Some(Self::ModpacksCurseforge),
            _ => None,
        }
    }

    pub fn from_mr_project_type(project_type: ProjectType) -> Option<Self> {
        match project_type {
            ProjectType::Mod => Some(Self::Mods),
            ProjectType::Shader => Some(Self::Shaders),
            ProjectType::Modpack => Some(Self::ModpacksModrinth),
            ProjectType::ResourcePack => Some(Self::Resourcepacks),
            ProjectType::Project => None,
            ProjectType::Plugin => None,
            ProjectType::Datapack => None,
        }
    }

    pub fn infer(path: &Path) -> io::Result<Option<SourceKindWithModpack>> {
        Ok(if path.extension().is_some_and(|ext| ext == "jar") {
            Some(SourceKindWithModpack::Mods)
        } else if path.extension().is_some_and(|ext| ext == "mrpack") {
            Some(SourceKindWithModpack::ModpacksModrinth)
        } else if path.extension().is_some_and(|ext| ext == "zip") {
            let mut file = io::BufReader::new(File::open(path)?);
            let mut archive = ZipArchive::new(&mut file)?;

            if check_exists(&mut archive, "pack.mcmeta")? {
                Some(SourceKindWithModpack::Resourcepacks)
            } else if check_exists(&mut archive, "manifest.json")? {
                Some(SourceKindWithModpack::ModpacksCurseforge)
            } else if check_exists(&mut archive, "shaders/")? {
                Some(SourceKindWithModpack::Shaders)
            } else {
                None
            }
        } else {
            None
        })
    }
}

#[derive(Serialize, Deserialize, Debug, Clone, Default)]
#[serde(rename_all = "kebab-case")]
pub struct Filters {
    #[serde(default, alias = "version", with = "MaybeListOrSingle")]
    pub versions: Option<Vec<Version>>,
    #[serde(default, alias = "mod-loader", with = "MaybeListOrSingle")]
    pub mod_loaders: Option<Vec<ModLoader>>,
    #[serde(default, alias = "release-channel")]
    pub release_channels: Option<Vec<ReleaseChannel>>,
    #[serde(default)]
    pub filename: Option<Vec<Regex>>,
    #[serde(default)]
    pub title: Option<Vec<Regex>>,
    #[serde(default)]
    pub description: Option<Vec<Regex>>,
    #[serde(default)]
    pub install_overrides: Option<bool>,
    #[serde(default, with = "MaybeListOrSingle")]
    #[serde(alias = "hash")]
    pub hashes: Option<Vec<String>>,
}

#[derive(Deserialize)]
#[serde(untagged)]
enum MaybeListOrSingle<T> {
    Single(T),
    Multiple(Vec<T>),
}

impl<T> MaybeListOrSingle<T> {
    pub fn serialize<S>(data: &Option<Vec<T>>, serializer: S) -> Result<S::Ok, S::Error>
    where
        S: serde::Serializer,
        T: serde::Serialize,
    {
        match data {
            Some(data) => {
                if data.len() == 1 {
                    data[0].serialize(serializer)
                } else {
                    data.serialize(serializer)
                }
            }
            None => data.serialize(serializer),
        }
    }

    pub fn deserialize<'de, D>(deserializer: D) -> Result<Option<Vec<T>>, D::Error>
    where
        D: serde::Deserializer<'de>,
        T: serde::Deserialize<'de>,
    {
        match <Self as serde::Deserialize>::deserialize(deserializer)? {
            MaybeListOrSingle::Single(item) => Ok(Some(vec![item])),
            MaybeListOrSingle::Multiple(items) => Ok(Some(items)),
        }
    }
}

impl Filters {
    pub fn empty() -> Filters {
        Filters::default()
    }

    pub fn concat(self, other: Filters) -> Filters {
        fn concat_opts<T: Clone>(a: Option<Vec<T>>, b: Option<Vec<T>>) -> Option<Vec<T>> {
            match (a, b) {
                (None, None) => None,
                (a, b) => Some([a.unwrap_or(vec![]), b.unwrap_or(vec![])].concat()),
            }
        }

        Filters {
            versions: self.versions.or(other.versions),
            mod_loaders: self.mod_loaders.or(other.mod_loaders),
            release_channels: concat_opts(self.release_channels, other.release_channels),
            filename: concat_opts(self.filename, other.filename),
            title: concat_opts(self.title, other.title),
            description: concat_opts(self.description, other.description),
            install_overrides: other.install_overrides.or(self.install_overrides),
            hashes: concat_opts(self.hashes, other.hashes),
        }
    }

    pub fn release_channel_matches(&self, release_channel: &ReleaseChannel) -> bool {
        let Some(release_channels) = &self.release_channels else {
            return true;
        };

        release_channels.iter().any(|c| c == release_channel)
    }

    pub fn filename_matches(&self, filename: &str) -> bool {
        let Some(filename_pat) = &self.filename else {
            return true;
        };

        filename_pat.iter().all(|pat| pat.0.is_match(filename))
    }

    pub fn title_matches(&self, title: &str) -> bool {
        let Some(title_pat) = &self.title else {
            return true;
        };

        title_pat.iter().all(|pat| pat.0.is_match(title))
    }

    pub fn description_matches(&self, description: &str) -> bool {
        let Some(description_pat) = &self.description else {
            return true;
        };

        description_pat
            .iter()
            .all(|pat| pat.0.is_match(description))
    }

    pub fn mod_loader_matches(&self, mod_loader: &ModLoader) -> bool {
        let Some(mod_loaders) = &self.mod_loaders else {
            return true;
        };

        mod_loaders.iter().any(|p| p == mod_loader)
    }

    pub fn game_version_matches(&self, version: &str) -> bool {
        let Some(versions) = &self.versions else {
            return true;
        };

        versions.iter().any(|p| p.matches(version))
    }

    pub fn is_empty(&self) -> bool {
        self.mod_loaders.is_none()
            && self.versions.is_none()
            && self.release_channels.is_none()
            && self.filename.is_none()
            && self.title.is_none()
            && self.description.is_none()
            && self.install_overrides.is_none()
            && self.hashes.is_none()
    }
}

macro_rules! impl_serde_for_parse {
    ($t:ty) => {
        impl Serialize for $t {
            fn serialize<S>(&self, serializer: S) -> Result<S::Ok, S::Error>
            where
                S: serde::Serializer,
            {
                serializer.serialize_str(&self.0.to_string())
            }
        }

        impl<'de> Deserialize<'de> for $t {
            fn deserialize<D>(deserializer: D) -> Result<Self, D::Error>
            where
                D: serde::Deserializer<'de>,
            {
                deserializer.deserialize_str(ParseVisitor::<Self>(PhantomData))
            }
        }
    };
}

struct ParseVisitor<T>(PhantomData<T>);

impl<'de, T> Visitor<'de> for ParseVisitor<T>
where
    T: FromStr,
    <T as FromStr>::Err: std::fmt::Display,
{
    type Value = T;

    fn expecting(&self, formatter: &mut fmt::Formatter) -> fmt::Result {
        write!(formatter, "a unix-style glob pattern")
    }

    fn visit_str<E>(self, v: &str) -> Result<Self::Value, E>
    where
        E: serde::de::Error,
    {
        match v.parse() {
            Ok(value) => Ok(value),
            Err(e) => Err(E::custom(e)),
        }
    }
}

#[derive(Clone)]
pub struct Regex(regex::Regex);

impl fmt::Debug for Regex {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.debug_tuple("Regex").field(&self.0.to_string()).finish()
    }
}

impl fmt::Display for Regex {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.0)
    }
}

impl FromStr for Regex {
    type Err = regex::Error;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        Ok(Self(regex::Regex::new(s)?))
    }
}

impl_serde_for_parse!(Regex);

#[derive(Clone)]
pub struct Version(semver::VersionReq);

impl fmt::Debug for Version {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.debug_tuple("Version").field(&self.0.to_string()).finish()
    }
}

impl Version {
    pub fn matches(&self, s: &str) -> bool {
        let (rest, tag) = match s.find('-') {
            Some(index) => s.split_at(index),
            None => (s, ""),
        };

        let tag = if tag.is_empty() {
            None
        } else {
            Prerelease::new(&tag[1..])
                .inspect_err(|e| eprintln!("WARN: semver tag parse error: {e}"))
                .ok()
        };

        fn find_split(s: &str) -> (&str, &str) {
            match s.find('.') {
                Some(index) => {
                    let (s, rest) = s.split_at(index);
                    (s, &rest[1..])
                }
                None => (s, ""),
            }
        }

        let (major, rest) = find_split(rest);
        let (minor, rest) = find_split(rest);
        let (patch, rest) = find_split(rest);

        if !rest.is_empty() {
            eprintln!(
                "WARN: semver parse error ({s:?}): unexpected eof, discarded data ({rest:?})"
            );
        }

        fn parse_part(s: &str) -> u64 {
            if s.is_empty() {
                return 0;
            }
            s.parse().ok().unwrap_or(0)
        }

        let major = parse_part(major);
        let minor = parse_part(minor);
        let patch = parse_part(patch);

        let version = {
            let mut version = semver::Version::new(major, minor, patch);
            if let Some(tag) = tag {
                version.pre = tag;
            };
            version
        };

        self.0.matches(&version)
    }

    pub fn is_strict(&self) -> bool {
        self.0
            .comparators
            .iter()
            .any(|comp| matches!(comp.op, semver::Op::Exact) && comp.minor.is_some())
    }

    pub fn into_req(self) -> semver::VersionReq {
        self.0
    }
}

impl fmt::Display for Version {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.0)
    }
}

impl FromStr for Version {
    type Err = semver::Error;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        Ok(Self(semver::VersionReq::parse(s)?))
    }
}

impl_serde_for_parse!(Version);

#[derive(
    Deserialize, Serialize, Debug, Display, Clone, Copy, PartialEq, Eq, clap::ValueEnum, Hash,
)]
pub enum ModLoader {
    Quilt,
    Fabric,
    Forge,
    #[clap(name = "neoforge")]
    NeoForge,
}

#[derive(thiserror::Error, Debug, PartialEq, Eq)]
#[error("The given string is not a recognised mod loader")]
pub struct ModLoaderParseError;

impl FromStr for ModLoader {
    type Err = ModLoaderParseError;

    // This implementation is case-insensitive
    fn from_str(from: &str) -> Result<Self, Self::Err> {
        match from.trim().to_lowercase().as_str() {
            "quilt" => Ok(Self::Quilt),
            "fabric" => Ok(Self::Fabric),
            "forge" => Ok(Self::Forge),
            "neoforge" => Ok(Self::NeoForge),
            _ => Err(Self::Err {}),
        }
    }
}

#[derive(Deserialize, Serialize, Debug, Display, Clone, Copy, PartialEq, Eq, clap::ValueEnum)]
pub enum ReleaseChannel {
    Release,
    Beta,
    Alpha,
}

/// The 'fabric.mod.json' file in a fabric mod.
#[derive(Serialize, Deserialize)]
#[serde(rename_all = "camelCase")]
pub struct FabricMetadata {
    pub schema_version: u64,
    pub id: String,
    pub version: String,
}

/// The `META-INF/mods.toml` file in a forge mod.
#[derive(Serialize, Deserialize)]
#[serde(rename_all = "camelCase")]
pub struct ForgeMetadata {
    pub mod_loader: String,
    pub loader_version: String,
    pub license: String,
    pub mods: Vec<ForgeMetadataMod>,
}

#[derive(Serialize, Deserialize)]
#[serde(rename_all = "camelCase")]
pub struct ForgeMetadataMod {
    pub mod_id: String,
}
