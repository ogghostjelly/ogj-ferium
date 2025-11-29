use crate::{
    config::structs::{
        Filters, ModLoader, Profile, ReleaseChannel, Source, SourceId, SourceKind,
        SourceKindWithModpack,
    },
    get_tmp_dir,
    iter_ext::IterExt as _,
    upgrade::{calculate_sha512, check, Metadata},
    CURSEFORGE_API, GITHUB_API, MODRINTH_API,
};
use serde::Deserialize;
use std::{collections::HashMap, fs::File, io::Write as _, path::Path, str::FromStr};
use url::Url;

#[derive(thiserror::Error, Debug)]
pub enum Error {
    #[error(
        "The developer of this project has denied third party applications from downloading it"
    )]
    /// The user can manually download the mod and place it in the `user` folder of the output directory to mitigate this.
    /// However, they will have to manually update the mod.
    DistributionDenied,
    #[error("The project has already been added")]
    AlreadyAdded,
    #[error("The project is not compatible because {_0}")]
    Incompatible(#[from] check::Error),
    #[error("The project does not exist")]
    DoesNotExist,
    #[error("The project class id '{0:?}' is not supported")]
    UnsupportedClassId(Option<i32>),
    #[error("The project type '{0:?}' is not supported")]
    UnsupportedProjectType(ProjectType),
    #[error("The file type '{0}' is not supported or unknown")]
    UnsupportedFileType(String),
    #[error("GitHub: {0}")]
    GitHubError(String),
    #[error("GitHub: {0:#?}")]
    OctocrabError(#[from] octocrab::Error),
    #[error("Modrinth: {0}")]
    ModrinthError(#[from] ferinth::Error),
    #[error("CurseForge: {0}")]
    CurseForgeError(#[from] furse::Error),
    #[error(transparent)]
    Io(#[from] std::io::Error),
    #[error(transparent)]
    Reqwest(#[from] reqwest::Error),
}
type Result<T> = std::result::Result<T, Error>;

#[derive(Deserialize, Debug)]
struct GraphQlResponse {
    data: HashMap<String, Option<ResponseData>>,
    #[serde(default)]
    errors: Vec<GraphQLError>,
}

#[derive(Deserialize, Debug)]
struct GraphQLError {
    #[serde(rename = "type")]
    type_: String,
    path: Vec<String>,
    message: String,
}

#[derive(Deserialize, Debug)]
struct ResponseData {
    owner: OwnerData,
    name: String,
    releases: ReleaseConnection,
}
#[derive(Deserialize, Debug)]
struct OwnerData {
    login: String,
}
#[derive(Deserialize, Debug)]
struct ReleaseConnection {
    nodes: Vec<Release>,
}
#[derive(Deserialize, Debug)]
#[serde(rename_all = "camelCase")]
struct Release {
    name: String,
    description: String,
    is_prerelease: bool,
    release_assets: ReleaseAssetConnection,
}
#[derive(Deserialize, Debug)]
struct ReleaseAssetConnection {
    nodes: Vec<ReleaseAsset>,
}
#[derive(Deserialize, Debug)]
struct ReleaseAsset {
    name: String,
}

pub fn parse_id(id: String) -> SourceId {
    if let Ok(id) = id.parse() {
        return SourceId::Curseforge(id);
    } else if let Ok(id) = id.parse() {
        return SourceId::Url(id);
    } else if let Some((owner, repo)) = id.split_once('/') {
        return SourceId::Github(owner.to_owned(), repo.to_owned());
    } else if !id.chars().all(|c| c.is_alphabetic()) {
        return SourceId::File(id.parse().expect("PathBuf parse is infallible"));
    }

    SourceId::Modrinth(id)
}

/// Adds mods from `identifiers`, and returns successful mods with their names, and unsuccessful mods with an error
///
/// Classifies the `identifiers` into the appropriate platforms, sends batch requests to get the necessary information,
/// checks details about the projects, and adds them to `profile` if suitable.
/// Performs checks on the mods to see whether they're compatible with the profile if `perform_checks` is true
pub async fn add(
    profile: &mut Profile,
    identifiers: Vec<SourceId>,
    perform_checks: bool,
    filters: Filters,
) -> Result<(Vec<String>, Vec<(String, Error)>)> {
    let mut mr_ids = Vec::new();
    let mut cf_ids = Vec::new();
    let mut gh_ids = Vec::new();
    let mut file_ids = Vec::new();
    let mut url_ids = Vec::new();
    let mut errors = Vec::new();

    for id in identifiers {
        match id {
            SourceId::Curseforge(id) => cf_ids.push(id),
            SourceId::Modrinth(id) => mr_ids.push(id),
            SourceId::Github(o, r) => gh_ids.push((o, r)),
            SourceId::File(path) => file_ids.push(path),
            SourceId::Url(url) => url_ids.push(url),
            SourceId::PinnedCurseforge(_, _)
            | SourceId::PinnedModrinth(_, _)
            | SourceId::PinnedGithub(_, _) => {
                todo!("\nAdding pinned projects from `add` is not yet supported\nPlease manually add the pinned project into the profile, like so: `my_mod = \"{id}\"`\n")
            }
        }
    }

    let cf_projects = if !cf_ids.is_empty() {
        cf_ids.sort_unstable();
        cf_ids.dedup();
        CURSEFORGE_API.get_mods(cf_ids.clone()).await?
    } else {
        Vec::new()
    };

    let mr_projects = if !mr_ids.is_empty() {
        mr_ids.sort_unstable();
        mr_ids.dedup();
        MODRINTH_API
            .project_get_multiple(&mr_ids.iter().map(AsRef::as_ref).collect_vec())
            .await?
    } else {
        Vec::new()
    };

    let gh_repos =
        {
            // Construct GraphQl query using raw strings
            let mut graphql_query = "{".to_string();
            for (i, (owner, name)) in gh_ids.iter().enumerate() {
                graphql_query.push_str(&format!(
                    "_{i}: repository(owner: \"{owner}\", name: \"{name}\") {{
                    owner {{
                        login
                    }}
                    name
                    releases(first: 100) {{
                        nodes {{
                            name
                            description
                            isPrerelease
                            releaseAssets(first: 10) {{
                                nodes {{
                                    name
                                }}
                            }}
                        }}
                    }}
                }}"
                ));
            }
            graphql_query.push('}');

            // Send the query
            let response: GraphQlResponse = if !gh_ids.is_empty() {
                GITHUB_API
                    .graphql(&HashMap::from([("query", graphql_query)]))
                    .await?
            } else {
                GraphQlResponse {
                    data: HashMap::new(),
                    errors: Vec::new(),
                }
            };

            errors.extend(response.errors.into_iter().map(|v| {
                (
                    {
                        let id = &gh_ids[v.path[0]
                            .strip_prefix('_')
                            .and_then(|s| s.parse::<usize>().ok())
                            .expect("Unexpected response data")];
                        format!("{}/{}", id.0, id.1)
                    },
                    if v.type_ == "NOT_FOUND" {
                        Error::DoesNotExist
                    } else {
                        Error::GitHubError(v.message)
                    },
                )
            }));

            response
                .data
                .into_values()
                .flatten()
                .map(|d| {
                    (
                        (d.owner.login, d.name),
                        d.releases
                            .nodes
                            .into_iter()
                            .flat_map(|release| {
                                release.release_assets.nodes.into_iter().map(move |asset| {
                                    Metadata {
                                        title: release.name.clone(),
                                        description: release.description.clone(),
                                        channel: if release.is_prerelease {
                                            ReleaseChannel::Beta
                                        } else {
                                            ReleaseChannel::Release
                                        },
                                        game_versions: asset
                                            .name
                                            .trim_end_matches(".jar")
                                            .trim_end_matches(".zip")
                                            .split(['-', '_', '+'])
                                            .map(|s| s.trim_start_matches("mc"))
                                            .map(ToOwned::to_owned)
                                            .collect_vec(),
                                        loaders: asset
                                            .name
                                            .trim_end_matches(".jar")
                                            .trim_end_matches(".zip")
                                            .split(['-', '_', '+'])
                                            .filter_map(|s| ModLoader::from_str(s).ok())
                                            .collect_vec(),
                                        filename: asset.name,
                                    }
                                })
                            })
                            .collect_vec(),
                    )
                })
                .collect_vec()
        };

    let mut success_names = Vec::new();

    fn to_name(name: &str, kind: Option<SourceKindWithModpack>) -> String {
        match kind {
            Some(kind) => format!(
                "{name} ({})",
                match kind {
                    SourceKindWithModpack::Mods => "Mod",
                    SourceKindWithModpack::Resourcepacks => "Resourcepack",
                    SourceKindWithModpack::Shaders => "Shader",
                    SourceKindWithModpack::ModpacksCurseforge => "CFModpack",
                    SourceKindWithModpack::ModpacksModrinth => "MRModpack",
                },
            ),
            None => name.to_owned(),
        }
    }

    for project in cf_projects {
        if let Some(i) = cf_ids.iter().position(|&id| id == project.id) {
            cf_ids.swap_remove(i);
        }

        let name = to_name(
            &project.name,
            project
                .class_id
                .and_then(SourceKindWithModpack::from_cf_class_id),
        );

        match curseforge(&project, profile, perform_checks, filters.clone()).await {
            Ok(_) => success_names.push(name),
            Err(err) => errors.push((format!("{} ({})", name, project.id), err)),
        }
    }
    errors.extend(
        cf_ids
            .iter()
            .map(|id| (id.to_string(), Error::DoesNotExist)),
    );

    for project in mr_projects {
        if let Some(i) = mr_ids
            .iter()
            .position(|id| id == &project.id || project.slug.eq_ignore_ascii_case(id))
        {
            mr_ids.swap_remove(i);
        }

        let name = to_name(
            &project.title,
            SourceKindWithModpack::from_mr_project_type(project.project_type.clone()),
        );

        match modrinth(&project, profile, perform_checks, filters.clone()).await {
            Ok(_) => success_names.push(name),
            Err(err) => errors.push((format!("{} ({})", name, project.id), err)),
        }
    }
    errors.extend(
        mr_ids
            .iter()
            .map(|id| (id.to_string(), Error::DoesNotExist)),
    );

    for (repo, asset_names) in gh_repos {
        match github(&repo, profile, Some(asset_names), filters.clone()).await {
            Ok(_) => success_names.push(format!("{}/{}", repo.0, repo.1)),
            Err(err) => errors.push((format!("{}/{}", repo.0, repo.1), err)),
        }
    }

    for path in file_ids {
        match file(&path, profile, perform_checks, filters.clone()).await {
            Ok(_) => success_names.push(format!("{}", path.display())),
            Err(err) => errors.push((format!("{}", path.display()), err)),
        }
    }

    for value in url_ids {
        match url(&value, profile, perform_checks, filters.clone()).await {
            Ok(_) => success_names.push(format!("{value}")),
            Err(err) => errors.push((format!("{value}"), err)),
        }
    }

    Ok((success_names, errors))
}

/// Check if the repo of `repo_handler` exists, releases mods, and is compatible with `profile`.
/// If so, add it to the `profile`.
///
/// Returns the name of the repository to display to the user
pub async fn github(
    id: &(impl AsRef<str> + ToString, impl AsRef<str> + ToString),
    profile: &mut Profile,
    perform_checks: Option<Vec<Metadata>>,
    filters: Filters,
) -> Result<()> {
    if let Some(download_files) = perform_checks {
        // Check if the repo is compatible
        check::select_latest(download_files.iter(), vec![&profile.filters, &filters]).await?;
    }

    let repo = id.0.as_ref().trim();
    let user = id.1.as_ref().trim();

    // Add it to the profile
    profile.push(
        SourceKind::Mods,
        format!("{repo}/{user}"),
        Source::github(repo.into(), user.into(), filters),
    )?;

    Ok(())
}

use ferinth::structures::project::{Project, ProjectType};

/// Check if the project of `project_id` has not already been added and is compatible with `profile`.
/// If so, add it to the `profile`.
pub async fn modrinth(
    project: &Project,
    profile: &mut Profile,
    perform_checks: bool,
    filters: Filters,
) -> Result<()> {
    // Check if the project is compatible
    if perform_checks {
        check::select_latest(
            [Metadata {
                filename: "".to_owned(),
                title: "".to_owned(),
                description: "".to_owned(),
                game_versions: project.game_versions.clone(),
                loaders: project
                    .loaders
                    .iter()
                    .filter_map(|s| ModLoader::from_str(s).ok())
                    .collect_vec(),
                channel: ReleaseChannel::Release,
            }]
            .iter(),
            vec![&profile.filters, &filters],
        )
        .await?;
    }

    let id = project.slug.clone();
    let source = Source::modrinth(project.id.clone(), filters);

    // Add it to the profile
    let kind = SourceKindWithModpack::from_mr_project_type(project.project_type.clone())
        .ok_or(Error::UnsupportedProjectType(project.project_type.clone()))?
        .into();

    profile.push(kind, id, source)
}

/// Check if the mod of `project_id` has not already been added, is a mod, and is compatible with `profile`.
/// If so, add it to the `profile`.
pub async fn curseforge(
    project: &furse::structures::mod_structs::Mod,
    profile: &mut Profile,
    perform_checks: bool,
    filters: Filters,
) -> Result<()> {
    // Check if it can be downloaded by third-parties
    if Some(false) == project.allow_mod_distribution {
        Err(Error::DistributionDenied)

    // Check if the mod is compatible
    } else {
        if perform_checks {
            check::select_latest(
                [Metadata {
                    filename: "".to_owned(),
                    title: "".to_owned(),
                    description: "".to_owned(),
                    game_versions: project
                        .latest_files_indexes
                        .iter()
                        .map(|i| i.game_version.clone())
                        .collect_vec(),
                    loaders: project
                        .latest_files_indexes
                        .iter()
                        .filter_map(|i| {
                            i.mod_loader
                                .as_ref()
                                .and_then(|l| ModLoader::from_str(&format!("{l:?}")).ok())
                        })
                        .collect_vec(),
                    channel: ReleaseChannel::Release,
                }]
                .iter(),
                vec![&profile.filters, &filters],
            )
            .await?;
        }

        // Add it to the profile
        profile.push(
            project
                .class_id
                .and_then(SourceKindWithModpack::from_cf_class_id)
                .ok_or(Error::UnsupportedClassId(project.class_id))?
                .into(),
            project.slug.clone(),
            Source::curseforge(project.id, filters),
        )?;

        Ok(())
    }
}

pub async fn file(
    path: &Path,
    profile: &mut Profile,
    perform_checks: bool,
    filters: Filters,
) -> Result<()> {
    let filename = path.file_name().unwrap_or_default().to_string_lossy();
    let (title, _) = filename.split_once('.').unwrap_or((&filename, ""));

    // Check if the project is compatible
    if perform_checks {
        check::select_latest(
            [Metadata {
                filename: filename.to_string(),
                title: title.to_owned(),
                description: format!("File at path {}", path.display()),
                game_versions: vec![],
                loaders: vec![],
                channel: ReleaseChannel::Release,
            }]
            .iter(),
            vec![&profile.filters, &filters],
        )
        .await?;
    }

    // Add it to the profile
    let kind = SourceKindWithModpack::infer(path)?
        .ok_or(Error::UnsupportedFileType(path.display().to_string()))?
        .into();

    let id = title.to_string();
    let source = Source::from_id(SourceId::File(path.to_path_buf()), filters);

    profile.push(kind, id, source)
}

pub async fn url(
    url: &Url,
    profile: &mut Profile,
    perform_checks: bool,
    filters: Filters,
) -> Result<()> {
    let path = url.path();
    let (_, filename) = path.split_once('/').unwrap_or(("", path));
    let (title, _) = filename.split_once('.').unwrap_or((filename, ""));

    // Check if the project is compatible
    if perform_checks {
        check::select_latest(
            [Metadata {
                filename: filename.to_string(),
                title: title.to_owned(),
                description: format!("File at url {url}"),
                game_versions: vec![],
                loaders: vec![],
                channel: ReleaseChannel::Release,
            }]
            .iter(),
            vec![&profile.filters, &filters],
        )
        .await?;
    }

    let temp_file_path = get_tmp_dir()?.join(filename);
    let mut temp_file = File::create(&temp_file_path)?;
    let mut response = reqwest::get(url.clone()).await?;
    while let Some(chunk) = response.chunk().await? {
        temp_file.write_all(&chunk)?;
    }

    let hash = calculate_sha512(&temp_file_path)?;

    // Add it to the profile
    let kind = SourceKindWithModpack::infer(&temp_file_path)?
        .ok_or(Error::UnsupportedFileType(url.to_string()))?
        .into();

    let id = title.to_string();
    let source = Source::url(url.clone(), hash[..32].to_string(), filters);

    profile.push(kind, id, source)
}

pub fn profile_contains(
    map: &HashMap<String, Source>,
    pred: impl Clone + Fn(&SourceId) -> bool,
) -> bool {
    for source in map.values() {
        if source_contains(source, pred.clone()) {
            return true;
        }
    }
    false
}

pub fn source_contains(source: &Source, pred: impl Clone + Fn(&SourceId) -> bool) -> bool {
    match source {
        Source::Single(source_id) => pred(source_id),
        Source::Multiple(sources) => {
            for source in sources {
                if source_contains(source, pred.clone()) {
                    return true;
                }
            }
            false
        }
        Source::Detailed { src, .. } => source_contains(src, pred),
    }
}
