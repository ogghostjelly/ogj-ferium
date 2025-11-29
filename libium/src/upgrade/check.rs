use super::Metadata;
use crate::{config::structs::Filters, iter_ext::IterExt, MODRINTH_API};
use ferinth::structures::tag::GameVersionType;
use std::{collections::HashSet, sync::OnceLock};

#[derive(thiserror::Error, Debug)]
#[error(transparent)]
pub enum Error {
    VersionGrouping(#[from] ferinth::Error),
    FilenameGlob(#[from] regex::Error),
    #[error("The following filter(s) were empty: {}", _0.iter().display(", "))]
    FilterEmpty(Vec<String>),
    #[error("Failed to find a compatible combination")]
    IntersectFailure,
}
pub type Result<T> = std::result::Result<T, Error>;

static VERSION_GROUPS: OnceLock<Vec<Vec<String>>> = OnceLock::new();

/// Gets groups of versions that are considered minor updates in terms of mod compatibility
///
/// This is determined by Modrinth's `major` parameter for game versions.
pub async fn get_version_groups() -> Result<&'static Vec<Vec<String>>> {
    if let Some(v) = VERSION_GROUPS.get() {
        Ok(v)
    } else {
        let versions = MODRINTH_API.tag_list_game_versions().await?;
        let mut v = vec![vec![]];
        for version in versions {
            if version.version_type == GameVersionType::Release {
                // Push the version to the latest group
                v.last_mut().unwrap().push(version.version);
                // Create a new group if a new major versions is present
                if version.major {
                    v.push(vec![]);
                }
            }
        }
        let _ = VERSION_GROUPS.set(v);

        Ok(VERSION_GROUPS.get().unwrap())
    }
}

impl Filters {
    /// Returns the indices of `download_files` that have successfully filtered through `self`
    ///
    /// This function fails if getting version groups fails, or the regex files to parse.
    pub async fn filter(
        &self,
        download_files: impl Iterator<Item = (usize, &Metadata)> + Clone,
    ) -> Result<HashSet<usize>> {
        // Filter mod loader
        let download_files = download_files.filter(|(_, f)| {
            f.loaders.is_empty() || f.loaders.iter().any(|l| self.mod_loader_matches(l))
        });

        // Filter game version
        let download_files = download_files.filter(|(_, f)| {
            f.game_versions.is_empty()
                || f.game_versions.iter().any(|v| self.game_version_matches(v))
        });

        // Filter release channel
        let download_files =
            download_files.filter(|(_, f)| self.release_channel_matches(&f.channel));

        // Filter filename
        let download_files = download_files.filter(|(_, f)| self.filename_matches(&f.filename));

        // Filter title
        let download_files = download_files.filter(|(_, f)| self.title_matches(&f.title));

        // Filter description
        let download_files =
            download_files.filter(|(_, f)| self.description_matches(&f.description));

        Ok(download_files.map(|(i, _)| i).collect_hashset())
    }
}

/// Assumes that the provided `download_files` are sorted in the order of preference (e.g. chronological)
pub async fn select_latest(
    download_files: impl Iterator<Item = &Metadata> + Clone,
    filters: Vec<&Filters>,
) -> Result<usize> {
    // Filter download_files
    let filtered_futs = filters
        .iter()
        .map(|f| {
            let download_files = download_files.clone().enumerate();
            let fut = f.filter(download_files);
            fut
        })
        .collect_vec();

    // Await the filtered futures
    let mut filtered = {
        let mut vec = vec![];
        for fut in filtered_futs {
            vec.push(fut.await?);
        }
        vec.into_iter()
    };

    // Intersect the filtered downloads to find matches that satisfy all filters
    let index = filtered
        .next()
        .and_then(|set_1| {
            filtered
                .fold(set_1.clone(), |set_a, set_b| {
                    set_a.intersection(&set_b).copied().collect_hashset()
                })
                .into_iter()
                .min()
        })
        .ok_or(Error::IntersectFailure)?;

    Ok(index)
}
