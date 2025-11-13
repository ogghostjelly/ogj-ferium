use futures_util::future::try_join_all;

use super::{
    from_gh_asset, from_gh_releases, from_mr_version, try_from_cf_file, DistributionDeniedError,
    DownloadData,
};
use crate::{
    config::{filters::Filter, structs::ModIdentifier},
    iter_ext::IterExt as _,
    upgrade::Metadata,
    CURSEFORGE_API, GITHUB_API, MODRINTH_API,
};
use std::cmp::Reverse;

#[derive(Debug, thiserror::Error)]
#[error(transparent)]
pub enum Error {
    DistributionDenied(#[from] DistributionDeniedError),
    CheckError(#[from] super::check::Error),
    #[error("The pin provided is an invalid identifier")]
    InvalidPinID(#[from] std::num::ParseIntError),
    #[error("Modrinth: {0}")]
    ModrinthError(#[from] ferinth::Error),
    #[error("CurseForge: {0}")]
    CurseForgeError(#[from] furse::Error),
    #[error("GitHub: {0:#?}")]
    GitHubError(#[from] octocrab::Error),
    #[error("{0}")]
    Update(#[from] super::Error),
}
type Result<T> = std::result::Result<T, Error>;

impl ModIdentifier {
    pub async fn fetch_download_files(
        self,
        profile_filters: Vec<Filter>,
    ) -> Result<Vec<DownloadData>> {
        let download_files = match &self {
            ModIdentifier::PinnedCurseForgeProject(mod_id, pin) => vec![try_from_cf_file(
                CURSEFORGE_API.get_mod_file(*mod_id, *pin).await?,
            )?],
            ModIdentifier::PinnedModrinthProject(_, pin) => {
                vec![from_mr_version(MODRINTH_API.version_get(pin).await?)]
            }
            ModIdentifier::PinnedGitHubRepository((owner, repo), pin) => {
                return Ok(vec![from_gh_asset(
                    GITHUB_API
                        .repos(owner, repo)
                        .release_assets()
                        .get(*pin as u64)
                        .await?,
                )])
            }
            ModIdentifier::CurseForgeProject(id) => {
                let mut files = CURSEFORGE_API.get_mod_files(*id).await?;
                files.sort_unstable_by_key(|f| Reverse(f.file_date));
                files
                    .into_iter()
                    .map(|f| try_from_cf_file(f).map_err(Into::into))
                    .collect::<Result<Vec<_>>>()?
            }
            ModIdentifier::ModrinthProject(id) => MODRINTH_API
                .version_list(id)
                .await?
                .into_iter()
                .map(from_mr_version)
                .collect_vec(),
            ModIdentifier::GitHubRepository(owner, repo) => GITHUB_API
                .repos(owner, repo)
                .releases()
                .list()
                .send()
                .await
                .map(|r| from_gh_releases(r.items))?,
        };

        apply_filters(download_files, profile_filters).await
    }
}

/// Filter out the data that doesn't match the given profile filters.
pub async fn apply_filters(
    data: Vec<(Metadata, DownloadData)>,
    profile_filters: Vec<Filter>,
) -> Result<Vec<DownloadData>> {
    // Pass the data through all the filters.
    let mut filter_futs = vec![];

    for filter in &profile_filters {
        let iter = data.iter().map(|(m, _)| m).enumerate();
        filter_futs.push(filter.filter(iter));
    }

    // Join the results of all the filters together.
    // Get the intersection between all the hash-sets.
    let results = try_join_all(filter_futs).await?;
    let mut allowed = None; // the indicies that pass the filters.

    for set in results {
        let Some(allowed) = &mut allowed else {
            allowed = Some(set);
            continue;
        };

        *allowed = allowed.intersection(&set).copied().collect_hashset();
    }

    let Some(allowed) = allowed else {
        return Ok(vec![]);
    };

    // Filter the data that is not allowed.
    let data = data
        .into_iter()
        .enumerate()
        .filter(|(i, _)| allowed.contains(i))
        .map(|(_, (_, data))| data)
        .collect_vec();

    Ok(data)
}
