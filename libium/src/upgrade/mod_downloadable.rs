use futures_util::future::{join, join_all};

use super::{
    check, from_file, from_gh_releases, from_mr_version, from_url, try_from_cf_file,
    DistributionDeniedError, DownloadData,
};
use crate::{
    config::structs::{Filters, Source, SourceId, SourceKind},
    iter_ext::IterExt as _,
    upgrade::from_gh_asset,
    CURSEFORGE_API, GITHUB_API, MODRINTH_API,
};
use std::{cmp::Reverse, path::Path};

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
    #[error("No compatible mod sources found")]
    NoCompatibleSources,
    #[error("'file:' cannot be used in an embedded profile")]
    CantUseFileSource,
    #[error(transparent)]
    Io(#[from] crate::upgrade::Error),
}
type Result<T> = std::result::Result<T, Error>;

impl Source {
    pub async fn fetch_download_file(
        &self,
        src_path: Option<&Path>,
        kind: SourceKind,
        filters: Vec<&Filters>,
    ) -> Result<DownloadData> {
        let mut download_files = vec![];
        let _ = self.each_sources(filters, |filters, id| {
            download_files.push(id.fetch_download_file(src_path, kind, filters));
        });

        for file in join_all(download_files).await {
            match file {
                Ok(data) => return Ok(data),
                Err(Error::CheckError(check::Error::IntersectFailure)) => {}
                Err(e) => return Err(e),
            }
        }

        Err(Error::NoCompatibleSources)
    }
}

impl SourceId {
    pub async fn fetch_download_file(
        &self,
        src_path: Option<&Path>,
        kind: SourceKind,
        filters: Vec<&Filters>,
    ) -> Result<DownloadData> {
        let mut download_files = match self {
            SourceId::Curseforge(id) => {
                let (files, mod_) = join(
                    CURSEFORGE_API.get_mod_files(*id),
                    CURSEFORGE_API.get_mod(*id),
                )
                .await;
                let (mut files, mod_) = (files?, mod_?);

                files.sort_unstable_by_key(|f| Reverse(f.file_date));
                files
                    .into_iter()
                    .map(|f| try_from_cf_file(kind, f, mod_.class_id).map_err(Into::into))
                    .collect::<Result<Vec<_>>>()?
            }
            SourceId::Modrinth(id) => {
                let project = MODRINTH_API.project_get(id).await?;

                MODRINTH_API
                    .version_list(id)
                    .await?
                    .into_iter()
                    .map(|version| from_mr_version(kind, version, Some(project.project_type)))
                    .collect_vec()
            }
            SourceId::Github(owner, repo) => GITHUB_API
                .repos(owner, repo)
                .releases()
                .list()
                .send()
                .await
                .map(|r| from_gh_releases(kind, r.items))?,
            SourceId::File(path) => match src_path {
                Some(src_path) => vec![from_file(kind, src_path, path)?],
                None => return Err(Error::CantUseFileSource),
            },
            SourceId::Url(url) => vec![from_url(kind, url).await?],
            SourceId::PinnedCurseforge(mod_id, pin) => {
                let (mod_file, mod_) = join(
                    CURSEFORGE_API.get_mod_file(*mod_id, *pin),
                    CURSEFORGE_API.get_mod(*mod_id),
                )
                .await;
                let (mod_file, mod_) = (mod_file?, mod_?);

                let cf = try_from_cf_file(kind, mod_file, mod_.class_id)?;
                return Ok(cf.1);
            }
            SourceId::PinnedModrinth(id, pin) => {
                let (mr_version, mr_project) =
                    join(MODRINTH_API.version_get(pin), MODRINTH_API.project_get(id)).await;
                let (mr_version, mr_project) = (mr_version?, mr_project?);

                let mr = from_mr_version(kind, mr_version, Some(mr_project.project_type));
                return Ok(mr.1);
            }
            SourceId::PinnedGithub((owner, repo), pin) => {
                return Ok(from_gh_asset(
                    kind,
                    GITHUB_API
                        .repos(owner, repo)
                        .release_assets()
                        .get(*pin as u64)
                        .await?,
                ))
            }
        };

        let hashes = {
            let mut hashes = vec![];
            for filter in &filters {
                let Some(new_hashes) = &filter.hashes else {
                    continue;
                };

                for value in new_hashes {
                    hashes.push(value.clone())
                }
            }
            hashes
        };

        for (_, downloadable) in &mut download_files {
            downloadable.user_hash = hashes.clone()
        }

        let index =
            super::check::select_latest(download_files.iter().map(|(m, _)| m), filters).await?;
        Ok(download_files.into_iter().nth(index).unwrap().1)
    }
}
