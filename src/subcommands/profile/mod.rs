pub mod configure;
pub mod create;
mod delete;
pub mod embed;
pub mod import;
mod info;
pub mod switch;
pub mod unembed;
pub use configure::configure;
pub use create::create;
pub use delete::delete;
pub use embed::embed;
pub use import::import;
pub use info::info;
pub use switch::switch;
pub use unembed::unembed;

use crate::{file_picker::pick_folder, warn};
use anyhow::{ensure, Context as _, Result};
use colored::{ColoredString, Colorize as _};
use ferinth::Ferinth;
use fs_extra::dir::{copy, CopyOptions};
use inquire::{
    list_option::ListOption,
    validator::{ErrorMessage, Validation},
    Confirm, MultiSelect, Select,
};
use libium::{
    config::structs::{ModLoader, Version},
    iter_ext::IterExt as _,
    BASE_DIRS,
};
use std::{
    fs::{create_dir_all, read_dir},
    path::PathBuf,
};

#[expect(clippy::unwrap_used, reason = "All variants are present")]
pub fn pick_mod_loader(default: Option<&ModLoader>) -> Result<ModLoader> {
    let options = [
        ModLoader::Fabric,
        ModLoader::Quilt,
        ModLoader::NeoForge,
        ModLoader::Forge,
    ];
    let mut picker = Select::new("Which mod loader do you use?", options.into());
    if let Some(default) = default {
        picker.starting_cursor = options.iter().position(|l| l == default).unwrap();
    }
    Ok(picker.prompt()?)
}

pub async fn pick_minecraft_version(default: &[String]) -> Result<Vec<Version>> {
    let mut versions = Ferinth::default().tag_list_game_versions().await?;
    versions.sort_by(|a, b| {
        // Sort by release type (release > snapshot > beta > alpha) then in reverse chronological order
        a.version_type
            .cmp(&b.version_type)
            .then(b.date.cmp(&a.date))
    });
    let mut default_indices = vec![];
    let display_versions = versions
        .iter()
        .enumerate()
        .map(|(i, v)| {
            if default.contains(&v.version) {
                default_indices.push(i);
            }
            if v.major {
                v.version.bold()
            } else {
                v.version.clone().into()
            }
        })
        .collect_vec();

    let selected_versions =
        MultiSelect::new("Which version of Minecraft do you play?", display_versions)
            .with_validator(|x: &[ListOption<&ColoredString>]| {
                if x.is_empty() {
                    Ok(Validation::Invalid(ErrorMessage::Custom(
                        "You need to select atleast one version".to_owned(),
                    )))
                } else {
                    Ok(Validation::Valid)
                }
            })
            .with_default(&default_indices)
            .raw_prompt()?
            .into_iter()
            .map(|s| s.index)
            .collect_vec();

    let iter = versions.into_iter().enumerate().filter_map(|(i, v)| {
        if selected_versions.contains(&i) {
            Some(v.version)
        } else {
            None
        }
    });

    let mut versions = vec![];
    for mut value in iter {
        value.insert(0, '=');
        versions.push(value.parse()?);
    }
    Ok(versions)
}

pub async fn check_output_directory(output_dir: &PathBuf) -> Result<()> {
    ensure!(
        output_dir.is_absolute(),
        "The provided output directory is not absolute, i.e. it is a relative path"
    );
    if output_dir.file_name() != Some(std::ffi::OsStr::new(".minecraft")) {
        warn!("The output directory is not called `.minecraft`. Are you sure this is the right folder?");
    }

    let mut backup = false;
    if output_dir.exists() {
        for file in read_dir(output_dir)? {
            let file = file?;
            if file.path().is_file() && file.file_name() != ".DS_Store" {
                backup = true;
                break;
            }
        }
    }
    if backup {
        println!(
            "There are files in your output directory, these will be deleted when you upgrade."
        );
        if Confirm::new("Would like to create a backup?")
            .prompt()
            .unwrap_or_default()
        {
            let backup_dir = pick_folder(
                BASE_DIRS.home_dir(),
                "Where should the backup be made?",
                "Output Directory",
            )?
            .context("Please pick a backup directory")?;
            create_dir_all(&backup_dir)?;
            copy(output_dir, backup_dir, &CopyOptions::new())?;
        }
    }
    Ok(())
}
