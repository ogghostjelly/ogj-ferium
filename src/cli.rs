#![deny(missing_docs)]

use crate::{subcommands::profile, DEFAULT_PARALLEL_TASKS};
use clap::{Args, Parser, Subcommand, ValueEnum, ValueHint};
use clap_complete::Shell;
use libium::config::structs::{Filters, ModLoader, Regex, ReleaseChannel, Version};
use std::path::PathBuf;

#[derive(Clone, Debug, Parser)]
#[clap(author, version, about)]
pub struct Ferium {
    #[clap(subcommand)]
    pub subcommand: SubCommands,
    /// Sets the number of worker threads the tokio runtime will use.
    /// You can also use the environment variable `TOKIO_WORKER_THREADS`.
    #[clap(long, short)]
    pub threads: Option<usize>,
    /// Specify the maximum number of simultaneous parallel tasks.
    #[clap(long, short = 'p', default_value_t = DEFAULT_PARALLEL_TASKS)]
    pub parallel_tasks: usize,
    /// Set a GitHub personal access token for increasing the GitHub API rate limit.
    #[clap(long, visible_alias = "gh", env = "GITHUB_TOKEN")]
    pub github_token: Option<String>,
    /// Set a custom Curseforge API key.
    #[clap(long, visible_alias = "cf", env = "CURSEFORGE_API_KEY")]
    pub curseforge_api_key: Option<String>,
    /// Set the file to read the config from.
    /// This does not change the `cache` and `tmp` directories.
    /// You can also use the environment variable `OGJ_FERIUM_CONFIG_FILE`.
    #[clap(long, short, visible_aliases = ["config", "conf"])]
    #[clap(value_hint(ValueHint::FilePath))]
    pub config_file: Option<PathBuf>,
}

#[derive(Clone, Debug, Subcommand)]
pub enum SubCommands {
    /*  TODO:
        Use this for filter arguments:
        https://docs.rs/clap/latest/clap/_derive/_tutorial/chapter_3/index.html#argument-relations
    */
    /// Add mods to the profile
    Add {
        /// The identifier(s) of the mod/project/repository
        ///
        /// The Modrinth project ID is specified at the bottom of the left sidebar under 'Technical information'.
        /// You can also use the project slug in the URL.
        /// The Curseforge project ID is specified at the top of the right sidebar under 'About Project'.
        /// The GitHub identifier is the repository's full name, e.g. `OgGhostJelly/ferium`.
        #[clap(required = true)]
        identifiers: Vec<String>,

        /// Temporarily ignore game version and mod loader checks and add the mod anyway
        #[clap(long, short, visible_alias = "override")]
        force: bool,

        /// Pin a mod to a specific version
        #[clap(long, short, visible_alias = "lock")]
        pin: Option<String>,

        #[command(flatten)]
        filters: FilterArguments,
    },
    /// Scan the profile's output directory (or the specified directory) for mods and add them to the profile
    Scan {
        /// The platform you prefer mods to be added from.
        /// If a mod isn't available from this platform, the other platform will still be used.
        #[clap(long, short, default_value_t)]
        platform: Platform,
        /// The .minecraft directory to scan mods, resourcepacks, shaderpacks etc. from.
        /// Defaults to the profile's .minecraft directory.
        #[clap(long)]
        minecraft_dir: Option<PathBuf>,
        /// The directory to scan mods from.
        /// Defaults to the profile's output directory.
        #[clap(long, short, alias = "mods")]
        mods_dir: Option<PathBuf>,
        /// The directory to scan resourcepacks from.
        /// Defaults to the profile's .minecraft/mods directory.
        #[clap(long, short, alias = "resourcepacks")]
        resourcepacks_dir: Option<PathBuf>,
        /// The directory to scan mods from.
        /// Defaults to the profile's .minecraft/shaderpacks directory.
        #[clap(long, short, alias = "shaderpacks")]
        shaderpacks_dir: Option<PathBuf>,
        /// Temporarily ignore game version and mod loader checks and add the mods anyway
        #[clap(long, short, visible_alias = "override")]
        force: bool,
    },
    /// Print shell auto completions for the specified shell
    Complete {
        /// The shell to generate auto completions for
        #[clap(value_enum)]
        shell: Shell,
    },
    /// List all the mods in the profile, and with some their metadata if verbose
    #[clap(visible_alias = "mods")]
    List {
        /// Show additional information about the mod
        #[clap(long, short)]
        verbose: bool,
        /// Output information in markdown format and alphabetical order
        ///
        /// Useful for creating modpack mod lists.
        /// Complements the verbose flag.
        #[clap(long, short, visible_alias = "md")]
        markdown: bool,
    },
    /// Create, configure, delete, switch, or list profiles
    Profile {
        #[clap(subcommand)]
        subcommand: Option<ProfileSubCommands>,
    },
    /// List all the profiles with their data
    Profiles,
    /// Remove mods and/or repositories from the profile.
    /// Optionally, provide a list of names or IDs of the mods to remove.
    #[clap(visible_alias = "rm")]
    Remove {
        /// List of project IDs or case-insensitive names of mods to remove
        mod_names: Vec<String>,
    },
    /// Download and install the latest compatible version of your mods
    #[clap(visible_aliases = ["download", "install"])]
    Upgrade {
        #[command(flatten)]
        filters: FilterArguments,
    },
    /// Migrate a ferium config to ogj-ferium, be warned this may not work
    Migrate {
        #[clap(long, short)]
        #[clap(value_hint(ValueHint::FilePath))]
        /// The path to the old ferium config file
        config: Option<PathBuf>,
        /// Whether to skip the overwrite config confirmation
        #[clap(long, short)]
        force: bool,
    },
    Configure(profile::configure::Args),
    Embed(profile::embed::Args),
    Unembed(profile::unembed::Args),
    Import(profile::import::Args),
    Switch(profile::switch::Args),
    Create(profile::create::Args),
}

#[derive(Clone, Debug, Subcommand)]
pub enum ProfileSubCommands {
    Configure(profile::configure::Args),
    /// Delete a profile.
    /// Optionally, provide the name of the profile to delete.
    #[clap(visible_aliases = ["remove", "rm"])]
    Delete {
        /// The name of the profile to delete
        profile_name: Option<String>,
        /// The name of the profile to switch to afterwards
        #[clap(long, short)]
        switch_to: Option<String>,
    },
    /// Show information about the current profile
    Info,
    /// List all the profiles with their data
    List,
    Create(profile::create::Args),
    Embed(profile::embed::Args),
    Unembed(profile::unembed::Args),
    Import(profile::import::Args),
    Switch(profile::switch::Args),
}

#[derive(Clone, Default, Debug, Args)]
#[group(id = "loader", multiple = false)]
pub struct FilterArguments {
    #[clap(long, short = 'l', group = "loader")]
    pub mod_loaders: Option<Vec<ModLoader>>,

    #[clap(long, short = 'v', group = "version")]
    pub game_versions: Option<Vec<Version>>,

    #[clap(long, short = 'c')]
    pub release_channels: Option<Vec<ReleaseChannel>>,

    #[clap(long, short = 'n')]
    pub filename: Option<Vec<Regex>>,
    #[clap(long, short = 't')]
    pub title: Option<Vec<Regex>>,
    #[clap(long, short = 'd')]
    pub description: Option<Vec<Regex>>,
    #[clap(long, short = 'o')]
    pub install_overrides: Option<bool>,
    #[clap(long, short = 'r')]
    #[clap(alias = "hash")]
    pub hashes: Option<Vec<String>>,
}

impl From<FilterArguments> for Filters {
    fn from(value: FilterArguments) -> Self {
        Filters {
            versions: value.game_versions,
            mod_loaders: value.mod_loaders,
            release_channels: value.release_channels,
            filename: value.filename,
            title: value.title,
            description: value.description,
            install_overrides: value.install_overrides,
            hashes: value.hashes,
        }
    }
}

#[derive(Clone, Copy, Debug, Default, ValueEnum)]
pub enum Platform {
    #[default]
    #[clap(alias = "mr")]
    Modrinth,
    #[clap(alias = "cf")]
    Curseforge,
}

impl std::fmt::Display for Platform {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Modrinth => write!(f, "modrinth"),
            Self::Curseforge => write!(f, "curseforge"),
        }
    }
}
