use super::{check_output_directory, pick_minecraft_version, pick_mod_loader};
use crate::{file_picker::pick_folder, try_iter_profiles};
use anyhow::{bail, ensure, Context as _, Result};
use colored::Colorize as _;
use inquire::{
    validator::{ErrorMessage, Validation},
    Confirm, Select, Text,
};
use libium::{
    config::{
        self,
        structs::{Config, ModLoader, Profile, ProfileItem, ProfileSource, Version},
    },
    get_minecraft_dir,
    iter_ext::IterExt,
};
use std::path::PathBuf;

/// Create a new profile.
/// Optionally, provide the settings as arguments.
/// Use the import flag to import mods from another profile.
#[derive(clap::Args, Clone, Debug)]
#[clap(visible_alias = "new")]
pub struct Args {
    /// Copy over the mods from an existing profile.
    /// Optionally, provide the name of the profile to import mods from.
    #[clap(long, short, visible_aliases = ["copy", "duplicate"])]
    #[expect(clippy::option_option)]
    pub import: Option<Option<String>>,
    /// The Minecraft version to check compatibility for
    #[clap(long, short = 'v')]
    pub game_versions: Option<Vec<Version>>,
    /// The mod loader to check compatibility for
    #[clap(long, short = 'l')]
    #[clap(value_enum)]
    pub mod_loader: Option<ModLoader>,
    /// The name of the profile
    #[clap(long, short)]
    pub name: Option<String>,
    /// The `.minecraft` directory to output mods and other files to
    #[clap(long, short)]
    #[clap(value_hint(clap::ValueHint::DirPath))]
    pub minecraft_dir: Option<PathBuf>,
    /// The path to the profile
    #[clap(long, short)]
    #[clap(value_hint(clap::ValueHint::FilePath))]
    pub profile_path: Option<PathBuf>,
    /// Whether or not to embed the profile,
    /// i.e not make a file for it and instead store it directly in the ferium/ogj-config.toml
    #[clap(long, short)]
    pub embed: bool,
}

pub async fn create(
    config: &mut Config,
    Args {
        import,
        game_versions,
        mod_loader,
        name,
        minecraft_dir,
        profile_path,
        embed,
    }: Args,
) -> Result<()> {
    let item = match (game_versions, mod_loader, name, minecraft_dir) {
        (Some(game_versions), Some(mod_loader), Some(name), minecraft_dir) => {
            for item in &config.profiles {
                ensure!(
                    !item.config.name.eq_ignore_ascii_case(&name),
                    "A profile with that name already exists"
                );
            }
            let minecraft_dir = minecraft_dir.unwrap_or_else(|| get_minecraft_dir().join("mods"));
            ensure!(
                minecraft_dir.is_absolute(),
                "The provided minecraft directory is not absolute, i.e. it is a relative path"
            );

            let mut profile = Profile::new(Some(game_versions), mod_loader);

            import_from(config, import, &mut profile)?;

            if embed {
                ProfileItem::new(
                    ProfileSource::Embedded(Box::new(profile)),
                    name,
                    minecraft_dir,
                )
            } else {
                let path = ProfileItem::infer_path(profile_path, &name)?;
                config::write_profile(&path, &profile)?;
                ProfileItem::new(ProfileSource::Path(path), name, minecraft_dir)
            }
        }
        (None, None, None, None) => {
            let mut minecraft_dir = get_minecraft_dir();
            println!(
                "The default .minecraft directory is {}",
                minecraft_dir.display()
            );
            if Confirm::new("Would you like to specify a custom .minecraft directory?")
                .prompt()
                .unwrap_or_default()
            {
                if let Some(dir) = pick_folder(
                    &minecraft_dir,
                    "Pick an output directory",
                    "Output Directory",
                )? {
                    check_output_directory(&dir).await?;
                    minecraft_dir = dir;
                }
            }

            let profiles = config
                .profiles
                .iter()
                .map(|item| item.config.name.clone())
                .collect_vec();
            let name = Text::new("What should this profile be called")
                .with_validator(move |s: &str| {
                    Ok(
                        if profiles.iter().any(|name| name.eq_ignore_ascii_case(s)) {
                            Validation::Invalid(ErrorMessage::Custom(
                                "A profile with that name already exists".to_owned(),
                            ))
                        } else {
                            Validation::Valid
                        },
                    )
                })
                .prompt()?;

            let mut profile = Profile::new(
                Some(pick_minecraft_version(&[]).await?),
                pick_mod_loader(None)?,
            );

            import_from(config, import, &mut profile)?;

            if embed {
                ProfileItem::new(
                    ProfileSource::Embedded(Box::new(profile)),
                    name,
                    minecraft_dir,
                )
            } else {
                let path = ProfileItem::infer_path(profile_path, &name)?;
                config::write_profile(&path, &profile)?;
                ProfileItem::new(ProfileSource::Path(path), name, minecraft_dir)
            }
        }
        _ => {
            bail!("Provide the name, game version, mod loader, and output directory options to create a profile")
        }
    };

    println!(
        "{}",
        "After adding your mods, remember to run `ogj-ferium upgrade` to download them!".yellow()
    );

    config.profiles.push(item);
    config.active_profile = config.profiles.len() - 1; // Make created profile active
    Ok(())
}

#[expect(clippy::option_option)]
fn import_from(
    config: &mut Config,
    import: Option<Option<String>>,
    profile: &mut Profile,
) -> Result<()> {
    if let Some(from) = import {
        ensure!(
            !config.profiles.is_empty(),
            "There are no profiles configured to import mods from"
        );

        // If the profile name has been provided as an option
        if let Some(profile_name) = from {
            let (_, import_profile) = try_iter_profiles(&mut config.profiles)
                .find(|(item, _)| item.name.eq_ignore_ascii_case(&profile_name))
                .context("The profile name provided does not exist")?;

            for (kind, (name, source)) in import_profile.top_sources() {
                profile.map_mut(kind).insert(name.clone(), source.clone());
            }
        } else {
            let mut profile_names = vec![];
            let mut profiles = vec![];

            for (item, profile) in try_iter_profiles(&mut config.profiles) {
                profile_names.push(item.name.clone());
                profiles.push(profile);
            }
            if let Ok(selection) =
                Select::new("Select which profile to import mods from", profile_names)
                    .with_starting_cursor(config.active_profile)
                    .raw_prompt()
            {
                let import_profile = profiles.swap_remove(selection.index);
                for (kind, (name, source)) in import_profile.top_sources() {
                    profile.map_mut(kind).insert(name.clone(), source.clone());
                }
            }
        }
    }
    Ok(())
}
