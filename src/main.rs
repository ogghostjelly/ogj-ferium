#![cfg_attr(test, feature(assert_matches))]
#![deny(
    clippy::all,
    clippy::perf,
    clippy::cargo,
    clippy::style,
    clippy::pedantic,
    clippy::suspicious,
    clippy::complexity,
    clippy::create_dir,
    clippy::unwrap_used,
    clippy::correctness,
    clippy::allow_attributes
)]
#![deny(clippy::expect_used, reason = "Use anyhow::Context instead")]
#![warn(clippy::dbg_macro)]
#![expect(clippy::multiple_crate_versions, clippy::too_many_lines)]

mod add;
mod cli;
mod download;
mod file_picker;
mod log;
mod subcommands;

#[cfg(test)]
mod tests;

use anyhow::{anyhow, bail, ensure, Context as _, Result};
use clap::{CommandFactory, Parser};
use cli::{Ferium, ProfileSubCommands, SubCommands};
use colored::{ColoredString, Colorize};
use indicatif::ProgressStyle;
use inquire::Select;
use libium::{
    config::{
        self, read_config,
        structs::{
            Config, Filters, Profile, ProfileItem, ProfileItemConfig, ProfileSource,
            ProfileSourceMut, SourceId, SourceKind,
        },
    },
    iter_ext::IterExt as _,
};
use std::{
    env::{set_var, var_os},
    path::PathBuf,
    process::ExitCode,
    sync::{LazyLock, OnceLock},
};
use subcommands::profile;
use tokio::sync::Semaphore;

const CROSS: &str = "×";
static TICK: LazyLock<ColoredString> = LazyLock::new(|| "✓".green());

pub const DEFAULT_PARALLEL_TASKS: usize = 50;
pub static SEMAPHORE: OnceLock<Semaphore> = OnceLock::new();
#[must_use]
pub const fn default_semaphore() -> Semaphore {
    Semaphore::const_new(DEFAULT_PARALLEL_TASKS)
}

/// Indicatif themes
#[expect(clippy::expect_used)]
pub static STYLE_NO: LazyLock<ProgressStyle> = LazyLock::new(|| {
    ProgressStyle::default_bar()
        .template("{spinner} {elapsed} [{wide_bar:.cyan/blue}] {pos:.cyan}/{len:.blue}")
        .expect("Progress bar template parse failure")
        .progress_chars("#>-")
});
#[expect(clippy::expect_used)]
pub static STYLE_BYTE: LazyLock<ProgressStyle> = LazyLock::new(|| {
    ProgressStyle::default_bar()
        .template(
            "{spinner} {bytes_per_sec} [{wide_bar:.cyan/blue}] {bytes:.cyan}/{total_bytes:.blue}",
        )
        .expect("Progress bar template parse failure")
        .progress_chars("#>-")
});

fn main() -> ExitCode {
    #[cfg(windows)]
    // Enable colours on conhost (command prompt or powershell)
    {
        #[expect(clippy::unwrap_used, reason = "There is actually no error")]
        colored::control::set_virtual_terminal(true).unwrap();
    }

    let cli = Ferium::parse();

    let mut builder = tokio::runtime::Builder::new_multi_thread();
    builder.enable_all();
    builder.thread_name("ferium-worker");
    if let Some(threads) = cli.threads {
        builder.worker_threads(threads);
    }
    #[expect(clippy::expect_used)] // No error handling yet
    let runtime = builder.build().expect("Could not initialise Tokio runtime");

    if let Err(err) = runtime.block_on(actual_main(cli)) {
        if !err.to_string().is_empty() {
            eprintln!("{}", err.to_string().red().bold());
            if err
                .to_string()
                .to_lowercase()
                .contains("error trying to connect")
                || err
                    .to_string()
                    .to_lowercase()
                    .contains("error sending request")
            {
                eprintln!(
                    "{}",
                    "Verify that you are connnected to the internet"
                        .yellow()
                        .bold()
                );
            }
        }
        ExitCode::FAILURE
    } else {
        ExitCode::SUCCESS
    }
}

async fn actual_main(mut cli_app: Ferium) -> Result<()> {
    // The complete command should not require a config.
    // See [#139](https://github.com/gorilla-devs/ferium/issues/139) for why this might be a problem.
    if let SubCommands::Complete { shell } = cli_app.subcommand {
        clap_complete::generate(
            shell,
            &mut Ferium::command(),
            "ferium",
            &mut std::io::stdout(),
        );
        return Ok(());
    }
    // Alias `ferium profiles` to `ferium profile list`
    if let SubCommands::Profiles = cli_app.subcommand {
        cli_app.subcommand = SubCommands::Profile {
            subcommand: Some(ProfileSubCommands::List),
        };
    }

    if let Some(token) = cli_app.github_token {
        if !token.is_empty() {
            set_var("GITHUB_TOKEN", token);
        }
    }
    if let Some(key) = cli_app.curseforge_api_key {
        if !key.is_empty() {
            set_var("CURSEFORGE_API_KEY", key);
        }
    }

    let _ = SEMAPHORE.set(Semaphore::new(cli_app.parallel_tasks));

    let old_default_config_path = libium::BASE_DIRS
        .home_dir()
        .join(".config")
        .join("ferium")
        .join("ogj-config.toml");
    let config_path = &cli_app
        .config_file
        .or_else(|| var_os("OGJ_FERIUM_CONFIG_FILE").map(Into::into))
        .unwrap_or({
            #[cfg(target_os = "macos")]
            {
                old_default_config_path.clone()
            }
            #[cfg(not(target_os = "macos"))]
            {
                libium::PROJECT_DIRS.config_dir().join("ogj-config.toml")
            }
        });

    // Handle old configs which may be in a different path
    if !config_path.exists() && old_default_config_path.exists() {
        std::fs::rename(old_default_config_path, config_path)
            .context("Failed to relocate config file to the new path, try doing so manually.")?;
    }

    let mut config = config::read_config(config_path)?;
    handle_invalid_paths(config_path, &mut config).await?;

    let mut did_add_fail = false;

    // Run function(s) based on the sub(sub)command to be executed
    match cli_app.subcommand {
        SubCommands::Complete { .. } | SubCommands::Profiles => {
            unreachable!();
        }
        SubCommands::Scan {
            platform,
            minecraft_dir,
            mods_dir,
            resourcepacks_dir,
            shaderpacks_dir,
            force,
        } => {
            let (item, mut profile) = get_active_profile(&mut config)?;

            let spinner = indicatif::ProgressBar::new_spinner().with_message("Reading files");
            spinner.enable_steady_tick(std::time::Duration::from_millis(100));

            let minecraft_dir = minecraft_dir.as_ref().unwrap_or(&item.minecraft_dir);

            let mut ids = vec![];

            for (dir, name) in [
                (mods_dir, "mods"),
                (shaderpacks_dir, "shaderpacks"),
                (resourcepacks_dir, "resourcepacks"),
            ] {
                let dir = dir.unwrap_or(minecraft_dir.join(name));

                if !dir.exists() {
                    continue;
                }

                ids.append(
                    &mut libium::scan(dir, || {
                        spinner.set_message("Querying servers");
                    })
                    .await?,
                );
            }

            spinner.set_message("Adding sources");

            let mut send_ids = Vec::new();
            for id in ids {
                use libium::config::structs::SourceId;
                match id {
                    (filename, None, None) => {
                        println!("{} {}", "Unknown file:".yellow(), filename.dimmed());
                    }
                    (_, Some(mr_id), None) => {
                        send_ids.push(SourceId::Modrinth(mr_id));
                    }
                    (_, None, Some(cf_id)) => {
                        send_ids.push(SourceId::Curseforge(cf_id));
                    }
                    (_, Some(mr_id), Some(cf_id)) => match platform {
                        cli::Platform::Modrinth => {
                            send_ids.push(SourceId::Modrinth(mr_id));
                        }
                        cli::Platform::Curseforge => {
                            send_ids.push(SourceId::Curseforge(cf_id));
                        }
                    },
                }
            }

            let (successes, failures) =
                libium::add(&mut profile, send_ids, !force, Filters::empty()).await?;
            spinner.finish_and_clear();

            did_add_fail = add::display_successes_failures(&successes, failures);

            profile.write()?;
        }
        SubCommands::Add {
            identifiers,
            force,
            pin,
            filters,
        } => {
            let (_item, mut profile) = get_active_profile(&mut config)?;
            let filters: Filters = filters.into();

            ensure!(
                // If filters are specified, there should only be one mod
                filters.is_empty() || identifiers.len() == 1,
                "You can only configure filters when adding a single mod!"
            );
            ensure!(
                // If a pin is specified, there should only be one mod
                pin.is_none() || identifiers.len() == 1,
                "You can only pin a version when adding a single mod!"
            );

            let identifiers = if let Some(pin) = pin {
                let id = libium::add::parse_id(identifiers[0].clone());
                vec![match id {
                    SourceId::Curseforge(project_id) => SourceId::PinnedCurseforge(
                        project_id,
                        pin.parse().context("Invalid file ID for CurseForge file")?,
                    ),
                    SourceId::Modrinth(project_id) => SourceId::PinnedModrinth(project_id, pin),
                    SourceId::Github(owner, repo) => SourceId::PinnedGithub(
                        (owner, repo),
                        pin.parse().context("Invalid asset ID for GitHub")?,
                    ),
                    _ => unreachable!(),
                }]
            } else {
                identifiers
                    .into_iter()
                    .map(libium::add::parse_id)
                    .collect_vec()
            };

            let (successes, failures) =
                libium::add(&mut profile, identifiers, !force, filters).await?;

            did_add_fail = add::display_successes_failures(&successes, failures);

            profile.write()?;
        }
        SubCommands::List { verbose, markdown } => {
            let (item, mut profile) = get_active_profile(&mut config)?;
            check_empty_profile(&profile)?;

            if verbose {
                subcommands::list::verbose(&mut profile, markdown).await?;
            } else {
                println!(
                    "{} {} on {} {}\n",
                    item.name.bold(),
                    format!("({} mods)", profile.mods.len()).yellow(),
                    profile
                        .filters
                        .mod_loaders
                        .as_ref()
                        .unwrap_or(&vec![])
                        .iter()
                        .map(|l| l.to_string().purple())
                        .display(" or "),
                    profile
                        .filters
                        .versions
                        .as_ref()
                        .unwrap_or(&vec![])
                        .iter()
                        .map(|v| v.to_string().green())
                        .display(", "),
                );

                for kind in SourceKind::ARRAY {
                    let map = profile.map(*kind);
                    if map.is_empty() {
                        continue;
                    }

                    println!("{}", format!("{kind:?}").bold());
                    for name in map.keys() {
                        println!("• {}", name.bold());
                    }
                    println!();
                }
            }
        }
        SubCommands::Profile { subcommand } => {
            let mut default_flag = false;
            let subcommand = subcommand.unwrap_or_else(|| {
                default_flag = true;
                ProfileSubCommands::Info
            });
            match subcommand {
                ProfileSubCommands::Configure(args) => {
                    subcommands::profile::configure(&mut config, args).await?;
                }
                ProfileSubCommands::Create(args) => {
                    subcommands::profile::create(&mut config, args).await?;
                }
                ProfileSubCommands::Delete {
                    profile_name,
                    switch_to,
                } => {
                    subcommands::profile::delete(&mut config, profile_name, switch_to)?;
                }
                ProfileSubCommands::Info => {
                    let (item, profile) = get_active_profile(&mut config)?;
                    subcommands::profile::info(item, &profile, true);
                }

                ProfileSubCommands::List => {
                    for (i, (item, profile)) in try_iter_profiles(&mut config.profiles).enumerate()
                    {
                        subcommands::profile::info(item, &profile, i == config.active_profile);
                    }
                }

                ProfileSubCommands::Switch(args) => {
                    subcommands::profile::switch(&mut config, args)?;
                }
                ProfileSubCommands::Embed(args) => {
                    subcommands::profile::embed(&mut config, args).await?;
                }
                ProfileSubCommands::Unembed(args) => {
                    subcommands::profile::unembed(&mut config, args).await?;
                }
                ProfileSubCommands::Import(args) => {
                    subcommands::profile::import(&mut config, args).await?;
                }
            }
            if default_flag {
                println!(
                    "{} ogj-ferium profile help {}",
                    "Use".yellow(),
                    "for more information about this subcommand".yellow()
                );
            }
        }
        SubCommands::Remove { mod_names } => {
            let (_item, mut profile) = get_active_profile(&mut config)?;
            check_empty_profile(&profile)?;
            subcommands::remove(&mut profile, mod_names)?;
            profile.write()?;
        }
        SubCommands::Upgrade { filters } => {
            let path = get_active_working_dir(&mut config)?;
            let (item, profile) = get_active_profile(&mut config)?;
            check_empty_profile(&profile)?;
            subcommands::upgrade(path.as_deref(), item, &profile, filters.into()).await?;
        }
        SubCommands::Migrate {
            config: old_config_path,
            force,
        } => {
            subcommands::migrate(old_config_path, force).await?;
            config = read_config(config_path)?;
        }
        SubCommands::Configure(args) => {
            subcommands::profile::configure(&mut config, args).await?;
        }
        SubCommands::Create(args) => subcommands::profile::create(&mut config, args).await?,
        SubCommands::Switch(args) => {
            subcommands::profile::switch(&mut config, args)?;
        }
        SubCommands::Embed(args) => {
            subcommands::profile::embed(&mut config, args).await?;
        }
        SubCommands::Unembed(args) => {
            subcommands::profile::unembed(&mut config, args).await?;
        }
        SubCommands::Import(args) => {
            subcommands::profile::import(&mut config, args).await?;
        }
    }

    // Update config file with possibly edited config
    config::write_config(config_path, &config)?;

    if did_add_fail {
        Err(anyhow!(""))
    } else {
        Ok(())
    }
}

/// Get the active profile with error handling
fn get_active_profile<'a>(
    config: &'a mut Config,
) -> Result<(&'a mut ProfileItemConfig, ProfileSourceMut<'a>)> {
    let index = get_active_profile_index(config)?;
    let ProfileItem { profile, config } = &mut config.profiles[index];

    let path = match &profile {
        ProfileSource::Path(path) => path.display().to_string().blue().underline(),
        ProfileSource::Embedded(_) => "Embedded".blue(),
    };

    let Some(profile) = profile.get_mut()? else {
        bail!(
            "The active profile '{}' at {path} no longer exists.",
            config.name,
        );
    };

    Ok((config, profile))
}

fn get_active_working_dir(config: &mut Config) -> Result<Option<PathBuf>> {
    let index = get_active_profile_index(config)?;
    match &config.profiles[index].profile {
        ProfileSource::Path(path) => Ok(Some(
            path.parent()
                .context("Internal error: Path should have a parent but it does not")?
                .to_path_buf(),
        )),
        ProfileSource::Embedded(_) => Ok(None),
    }
}

fn get_active_profile_index(config: &mut Config) -> Result<usize> {
    match config.profiles.len() {
        0 => {
            bail!(
                "There are no profiles configured, add a profile using `ogj-ferium profile create`"
            )
        }
        1 => config.active_profile = 0,
        n if config.active_profile >= n => {
            println!(
                "{}",
                "Active profile specified incorrectly, please pick a profile to use"
                    .red()
                    .bold()
            );
            subcommands::profile::switch(config, profile::switch::Args { profile_name: None })?;
        }
        _ => (),
    }

    Ok(config.active_profile)
}

fn try_iter_profiles<'a>(
    iter: impl IntoIterator<Item = &'a mut ProfileItem>,
) -> impl Iterator<Item = (&'a mut ProfileItemConfig, ProfileSourceMut<'a>)> {
    iter.into_iter().filter_map(|item| {
        let path = match &item.profile {
            ProfileSource::Path(path) => path.display().to_string().blue().underline(),
            ProfileSource::Embedded(_) => "Embedded".blue(),
        };

        let Some(profile) = item.profile.get_mut().transpose() else {
            warn!(
                "The profile '{}' at path {path} no longer exists.",
                item.config.name
            );
            return None;
        };

        let profile = profile
            .inspect_err(|e| {
                eprintln!(
                    "{}",
                    format!(
                        "Failed to check profile '{}' at {path}: {e}, skipping",
                        item.config.name,
                    )
                    .red()
                );
            })
            .ok()?;

        Some((&mut item.config, profile))
    })
}

/// Check if `profile` is empty, and if so return an error
fn check_empty_profile(profile: &Profile) -> Result<()> {
    ensure!(
        profile.top_sources().next().is_some()
            || !profile.imports.is_empty()
            || !profile.options.is_empty(),
        "Your currently selected profile is empty! Run `ferium help` to see how to add mods"
    );
    Ok(())
}

/// Warn invalid paths and remove them if needed.
async fn handle_invalid_paths(config_path: &PathBuf, config: &mut Config) -> Result<()> {
    let mut remove_indicies: Vec<usize> = vec![];

    let profiles = config
        .profiles
        .iter()
        .filter_map(|item| match &item.profile {
            ProfileSource::Path(path) => Some((
                item.config.name.clone(),
                path.clone(),
                item.config.minecraft_dir.clone(),
            )),
            ProfileSource::Embedded(_) => None,
        })
        .enumerate()
        .collect_vec();

    for (index, (name, path, minecraft_dir)) in profiles {
        if path.exists() {
            continue;
        }

        let selection = Select::new(
            &format!(
                "Profile '{}' at {} no longer exists. What do you want to do?",
                name,
                path.display().to_string().blue().underline()
            ),
            vec!["Ignore", "Delete", "Reimport"],
        )
        .prompt()
        .unwrap_or("Ignore");

        match selection {
            "Delete" => remove_indicies.push(index),
            "Reimport" => {
                subcommands::profile::import(
                    config,
                    profile::import::Args {
                        name: Some(name),
                        path: None,
                        minecraft_dir: Some(minecraft_dir),
                        embed: false,
                    },
                )
                .await?;

                remove_indicies.push(index);
            }
            "Ignore" => {}
            _ => bail!("Unexpected option in handling invalid profile path."),
        }
    }

    remove_indicies.sort_unstable_by(|a, b| a.cmp(b).reverse());

    for index in remove_indicies {
        config.profiles.remove(index);
    }

    config::write_config(config_path, config)?;
    Ok(())
}
