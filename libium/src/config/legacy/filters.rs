use super::structs::ModLoader;
use crate::{
    config::{structs, MigrateError},
    iter_ext::IterExt as _,
};
use derive_more::derive::Display;
use serde::{Deserialize, Serialize};

#[derive(Deserialize, Serialize, Debug, Display, Clone)]
pub enum Filter {
    /// Prefers files in the order of the given loaders
    ///
    /// Implementation detail: This filter only works as intended if it is run last on an already filtered list.
    #[display("Mod Loader ({})", _0.iter().display(", "))]
    ModLoaderPrefer(Vec<ModLoader>),

    /// Selects files that are compatible with any of the given loaders
    #[display("Mod Loader Either ({})", _0.iter().display(", "))]
    ModLoaderAny(Vec<ModLoader>),

    /// Selects files strictly compatible with the versions specified
    #[display("Game Version ({})", _0.iter().display(", "))]
    GameVersionStrict(Vec<String>),

    /// Selects files compatible with the versions specified and related versions that are
    /// considered to not have breaking changes (determined using Modrinth's game version tag list)
    #[display("Game Version Minor ({})", _0.iter().display(", "))]
    GameVersionMinor(Vec<String>),

    /// Selects files matching the channel provided or more stable channels
    #[display("Release Channel ({_0})")]
    ReleaseChannel(ReleaseChannel),

    /// Selects the files with filenames matching the provided regex
    #[display("Filename ({_0})")]
    Filename(String),

    /// Selects files with titles matching the provided regex
    #[display("Title ({_0})")]
    Title(String),

    /// Selects files with descriptions matching the provided regex
    #[display("Description ({_0})")]
    Description(String),
}

impl TryInto<structs::Filters> for Filter {
    type Error = MigrateError;

    fn try_into(self) -> Result<structs::Filters, Self::Error> {
        match self {
            Filter::ModLoaderPrefer(mod_loaders) => Ok(structs::Filters {
                mod_loaders: Some(mod_loaders.into_iter().map(Into::into).collect()),
                ..Default::default()
            }),
            Filter::ModLoaderAny(mod_loaders) => Ok(structs::Filters {
                mod_loaders: Some(mod_loaders.into_iter().map(Into::into).collect()),
                ..Default::default()
            }),
            Filter::GameVersionStrict(items) => Ok(structs::Filters {
                versions: Some({
                    let mut versions = vec![];
                    for mut item in items {
                        item.insert(0, '=');
                        versions.push(item.parse()?);
                    }
                    versions
                }),
                ..Default::default()
            }),
            Filter::GameVersionMinor(items) => Ok(structs::Filters {
                versions: Some({
                    let mut versions = vec![];
                    for mut item in items {
                        item.insert(0, '~');
                        versions.push(item.parse()?);
                    }
                    versions
                }),
                ..Default::default()
            }),
            Filter::ReleaseChannel(release_channel) => Ok(structs::Filters {
                release_channels: Some(vec![release_channel.into()]),
                ..Default::default()
            }),
            Filter::Filename(filename) => Ok(structs::Filters {
                filename: Some(vec![filename.parse()?]),
                ..Default::default()
            }),
            Filter::Title(title) => Ok(structs::Filters {
                title: Some(vec![title.parse()?]),
                ..Default::default()
            }),
            Filter::Description(description) => Ok(structs::Filters {
                description: Some(vec![description.parse()?]),
                ..Default::default()
            }),
        }
    }
}

#[derive(Deserialize, Serialize, Debug, Display, Clone, Copy, PartialEq, Eq, clap::ValueEnum)]
pub enum ReleaseChannel {
    Release,
    Beta,
    Alpha,
}

impl From<ReleaseChannel> for structs::ReleaseChannel {
    fn from(val: ReleaseChannel) -> Self {
        match val {
            ReleaseChannel::Release => structs::ReleaseChannel::Release,
            ReleaseChannel::Beta => structs::ReleaseChannel::Beta,
            ReleaseChannel::Alpha => structs::ReleaseChannel::Alpha,
        }
    }
}
