use crate::{
    default_semaphore,
    download::{clean, download, read_overrides},
    warn, CROSS, SEMAPHORE, STYLE_NO, TICK,
};
use anyhow::{anyhow, bail, Context as _, Result};
use colored::Colorize as _;
use indicatif::ProgressBar;
use libium::{
    config::{
        modpack::{curseforge, modrinth, read_file_from_zip, zip_extract},
        options::{Options, OptionsOverrides},
        read_profile,
        structs::{
            FabricMetadata, Filters, ForgeMetadata, ModLoader, Profile, ProfileItemConfig, Source,
            SourceId, SourceKind, SourceKindWithModpack, Version,
        },
    },
    get_tmp_dir,
    upgrade::{
        from_modpack_file, mod_downloadable, try_from_cf_file, DistributionDeniedError,
        DownloadData, DownloadSource,
    },
    CURSEFORGE_API,
};
use parking_lot::Mutex;
use std::{
    collections::HashMap,
    fs::{self, File},
    io::BufReader,
    mem::take,
    path::{Component, Path, PathBuf},
    sync::{mpsc, Arc},
    time::Duration,
};
use tokio::task::JoinSet;

pub async fn upgrade(
    // The path to the profiles parent or `None` if it is embedded
    src_path: Option<&Path>,
    profile_item: &ProfileItemConfig,
    profile: &Profile,
    filters: Filters,
) -> Result<()> {
    println!("{}", "Upgrading Sources".bold());

    let mut options = OptionsOverrides::default();
    let mut to_download = vec![];

    let error =
        get_platform_downloadables(src_path, &mut options, &mut to_download, profile, filters)
            .await?;

    for kind in SourceKind::ARRAY {
        let Some(dirname) = kind.dirname() else {
            continue;
        };

        let directory = profile_item.minecraft_dir.join(dirname);
        if !directory.exists() {
            continue;
        }

        clean(
            &directory,
            &mut to_download,
            matches!(kind, SourceKind::Mods),
        )
        .await?;
    }

    if to_download.is_empty() {
        println!("\n{}", "All up to date!".bold());
    } else {
        println!("{}", "\nDownloading Source Files\n".bold());
        download(profile_item.minecraft_dir.clone(), to_download).await?;
        remove_duplicate_jars(&profile_item.minecraft_dir)?;
    }

    apply_options_overrides(&profile_item.minecraft_dir, options)?;

    if error {
        Err(anyhow!(
            "\nCould not get the latest compatible version of some sources"
        ))
    } else {
        Ok(())
    }
}

pub fn remove_duplicate_jars(minecraft_dir: &Path) -> Result<()> {
    let mods_dir = minecraft_dir.join("mods");
    let mut ids: HashMap<String, Vec<PathBuf>> = HashMap::new();

    for entry in mods_dir.read_dir()? {
        let entry = entry?;
        if !entry.file_type()?.is_file() {
            continue;
        }

        let path = entry.path();

        let Some(found_ids) = read_jar_id(&path)? else {
            continue;
        };

        for id in found_ids {
            ids.entry(id).or_default().push(path.clone());
        }
    }

    for (id, paths) in ids {
        if paths.len() < 2 {
            continue;
        }

        let message = format!("Multiple JARs with the id '{id}' were found, choose one to keep.");
        let options = paths.iter().map(|p| p.display().to_string()).collect();
        let index = inquire::Select::new(&message, options).raw_prompt()?.index;

        for (i, path) in paths.iter().enumerate() {
            if i == index {
                continue;
            }

            fs::remove_file(path)?;
        }
    }

    Ok(())
}

pub fn read_jar_id(path: &Path) -> Result<Option<Vec<String>>> {
    if let Some(contents) =
        read_file_from_zip(BufReader::new(File::open(path)?), "META-INF/mods.toml")?
    {
        let metadata: ForgeMetadata = match toml::from_str(&contents) {
            Ok(value) => value,
            Err(e) => {
                warn!(
                    "err in {} while reading 'META-INF/mods.toml': {e}",
                    path.display()
                );
                return Ok(None);
            }
        };

        return Ok(Some(
            metadata.mods.into_iter().map(|mod_| mod_.mod_id).collect(),
        ));
    }

    if let Some(contents) =
        read_file_from_zip(BufReader::new(File::open(path)?), "fabric.mod.json")?
    {
        let metadata: FabricMetadata = match serde_json::from_str(&contents) {
            Ok(value) => value,
            Err(e) => {
                warn!(
                    "err in {} while reading 'fabric.mod.json': {e}",
                    path.display()
                );
                return Ok(None);
            }
        };

        return Ok(Some(vec![metadata.id]));
    }

    Ok(None)
}

/// Apply option overrides.
pub fn apply_options_overrides(minecraft_dir: &Path, options: OptionsOverrides) -> Result<()> {
    let options_path = minecraft_dir.join("options.txt");

    let reader = File::options()
        .read(true)
        .write(true)
        .create(true)
        .truncate(false)
        .open(&options_path)?;

    let reader = BufReader::new(reader);
    let mut opts = Options::read(reader, |err| warn!("read: {err}"))?;
    opts.apply(options, |err| warn!("apply: {err}"));

    let mut writer = File::create(options_path)?;
    opts.write(&mut writer, |err| warn!("write: {err}"))?;
    Ok(())
}

/// Get the latest compatible downloadable for the sources in `profile`
///
/// If an error occurs with a resolving task, instead of failing immediately,
/// resolution will continue and the error return flag is set to true.
async fn get_platform_downloadables(
    src_path: Option<&Path>,
    options: &mut OptionsOverrides,
    to_download: &mut Vec<DownloadData>,
    profile: &Profile,
    filters: Filters,
) -> Result<bool> {
    let filters = filters.concat(profile.filters.clone());
    check_unstrict_filter(&filters);

    let mut error = false;

    options.join(&profile.options);

    if let Some(src_path) = src_path {
        for import in &profile.imports {
            let profile_path = import.download(src_path).await?;
            let path = src_path.join(&profile_path);
            let Some(profile) = read_profile(&path)? else {
                bail!("The profile at '{}' doesn't exist.", profile_path.display())
            };

            error |= Box::pin(get_platform_downloadables(
                Some(
                    path.parent()
                        .context("Profile path should have a parent directory")?,
                ),
                options,
                to_download,
                &profile,
                filters.clone(),
            ))
            .await?;
        }

        if let Some(overrides) = profile.overrides_path() {
            read_overrides(to_download, &src_path.join(overrides))?;
        }

        if let Some(files) = profile.overrides_files() {
            for (path, value) in files {
                if !sanitize_path(path) {
                    continue;
                }

                to_download.push(DownloadData {
                    src: DownloadSource::Contents(value.clone()),
                    output: path.clone(),
                    length: value.len() as u64,
                    dependencies: vec![],
                    conflicts: vec![],
                    kind: None,
                    hash: None,
                    user_hash: vec![],
                });
            }
        }
    } else if !profile.imports.is_empty() {
        bail!("imports do not work in embedded profiles")
    } else if profile.overrides.is_some() {
        bail!("overrides do not work in embedded profiles")
    }

    for kind in SourceKind::ARRAY {
        if profile.map(*kind).is_empty() {
            continue;
        }

        error |= get_source_downloadables(src_path, *kind, to_download, profile, &filters).await?;
    }

    Ok(error)
}

fn sanitize_path(path: &Path) -> bool {
    if !path.is_relative() {
        eprintln!(
            "Only relative paths are allowed in overrides, {}",
            path.display()
        );
    }

    for comp in path.components() {
        match comp {
            Component::Prefix(prefix) => {
                eprintln!(
                    "{}",
                    format!(
                        "File prefix {:?} is not allowed in override paths, {}",
                        prefix.as_os_str(),
                        path.display()
                    )
                    .bright_red()
                );
                return false;
            }
            Component::RootDir => {
                eprintln!(
                    "{}",
                    format!(
                        "Root directory is not allowed in override paths, {}",
                        path.display()
                    )
                    .bright_red()
                );
                return false;
            }
            Component::ParentDir => {
                eprintln!(
                    "{}",
                    format!(
                        "Parent directory \"..\" is not allowed in override paths, {}",
                        path.display()
                    )
                    .bright_red()
                );
                return false;
            }
            Component::CurDir | Component::Normal(_) => {}
        }
    }
    true
}

async fn get_source_downloadables(
    src_path: Option<&Path>,
    kind: SourceKind,
    to_download: &mut Vec<DownloadData>,
    profile: &Profile,
    filters: &Filters,
) -> Result<bool> {
    let progress_bar = Arc::new(Mutex::new(ProgressBar::new(0).with_style(STYLE_NO.clone())));
    let mut tasks = JoinSet::new();
    let mut done_sources = Vec::new();
    let client = reqwest::Client::new();
    let (mod_sender, mod_rcvr) = mpsc::channel();

    // Wrap it again in an Arc so that I can count the references to it,
    // because I cannot drop the main thread's sender due to the recursion
    let mod_sender = Arc::new(mod_sender);

    println!("{}\n", "Determining the Latest Compatible Versions".bold());
    progress_bar
        .lock()
        .enable_steady_tick(Duration::from_millis(100));
    let sources = profile.map(kind);
    let pad_len = sources
        .keys()
        .map(String::len)
        .max()
        .unwrap_or(20)
        .clamp(20, 50);

    for (name, source) in sources {
        mod_sender.send((name.to_owned(), source.clone()))?;
    }

    let mut initial = true;

    // A race condition exists where if the last task drops its sender before this thread receives the message,
    // that particular message will get ignored. I used the ostrich algorithm to solve this.

    // `initial` accounts for the edge case where at first,
    // no tasks have been spawned yet but there are messages in the channel
    while Arc::strong_count(&mod_sender) > 1 || initial {
        if let Ok((name, source)) = mod_rcvr.try_recv() {
            initial = false;

            if done_sources.contains(&name) {
                continue;
            }

            done_sources.push(name.clone());
            progress_bar.lock().inc_length(1);

            let filters = filters.clone();
            let dep_sender = Arc::clone(&mod_sender);
            let progress_bar = Arc::clone(&progress_bar);
            let client = client.clone();
            let src_path = src_path.map(ToOwned::to_owned);

            tasks.spawn(async move {
                let permit = SEMAPHORE.get_or_init(default_semaphore).acquire().await?;

                let result = source
                    .fetch_download_file(src_path.as_deref(), kind, vec![&filters])
                    .await;

                drop(permit);

                progress_bar.lock().inc(1);
                match result {
                    Ok(mut download_file) => {
                        progress_bar.lock().println(format!(
                            "{} {name:pad_len$}  {}",
                            TICK.clone(),
                            download_file.filename().dimmed()
                        ));
                        for dep in take(&mut download_file.dependencies) {
                            let id = format!(
                                "Dependency of {name}: {}",
                                match &dep {
                                    SourceId::Curseforge(id) => id.to_string(),
                                    SourceId::Modrinth(id) | SourceId::PinnedModrinth(id, _) =>
                                        id.to_owned(),
                                    _ => unreachable!(),
                                }
                            );
                            let source = Source::from_id(dep, Filters::empty());
                            dep_sender.send((id, source))?;
                        }
                        if let SourceKind::Modpacks = kind {
                            let install_overrides = source
                                .filters()
                                .and_then(|filters| filters.install_overrides)
                                .unwrap_or(true);

                            let mut to_download = vec![];
                            download_modpack(
                                &mut to_download,
                                client,
                                download_file,
                                install_overrides,
                            )
                            .await?;
                            Ok(Some(to_download))
                        } else {
                            Ok(Some(vec![download_file]))
                        }
                    }
                    Err(err) => {
                        if let mod_downloadable::Error::ModrinthError(
                            ferinth::Error::RateLimitExceeded(_),
                        ) = err
                        {
                            // Immediately fail if the rate limit has been exceeded
                            progress_bar.lock().finish_and_clear();
                            bail!(err);
                        }
                        progress_bar.lock().println(format!(
                            "{}",
                            format!("{CROSS} {name:pad_len$}  {err}").red()
                        ));
                        Ok(None)
                    }
                }
            });
        }
    }

    Arc::try_unwrap(progress_bar)
        .map_err(|_| anyhow!("Failed to run threads to completion"))?
        .into_inner()
        .finish_and_clear();

    let tasks = tasks
        .join_all()
        .await
        .into_iter()
        .collect::<Result<Vec<_>>>()?;

    let error = tasks.iter().any(Option::is_none);
    for new_to_download in tasks.into_iter().flatten() {
        for downloadable in new_to_download {
            to_download.push(downloadable);
        }
    }

    Ok(error)
}

async fn download_modpack(
    to_download: &mut Vec<DownloadData>,
    client: reqwest::Client,
    downloadable: DownloadData,
    install_overrides: bool,
) -> Result<()> {
    let tmp_dir = get_tmp_dir()?;
    let (_size, filename) = downloadable.download(client, tmp_dir, |_| {}).await?;
    let path = tmp_dir.join(filename);
    let res = download_modpack_inner(to_download, &path, install_overrides).await;
    fs::remove_file(path)?;
    res
}

async fn download_modpack_inner(
    to_download: &mut Vec<DownloadData>,
    path: &PathBuf,
    install_overrides: bool,
) -> Result<()> {
    let Some(kind) = SourceKindWithModpack::infer(path)? else {
        bail!("Couldn't infer the modpack type")
    };

    let modpack_file = File::open(path)?;

    match kind {
        SourceKindWithModpack::ModpacksCurseforge => {
            let manifest: curseforge::Manifest = serde_json::from_str(
                &read_file_from_zip(BufReader::new(modpack_file), "manifest.json")?
                    .context("Does not contain manifest")?,
            )?;

            let file_ids: Vec<i32> = manifest.files.iter().map(|file| file.file_id).collect();
            let files = CURSEFORGE_API.get_files(file_ids.clone()).await?;

            let mut tasks = JoinSet::new();
            let mut msg_shown = false;
            for (i, file) in files.into_iter().enumerate() {
                let Some(file) = file else {
                    bail!(
                        "couldn't find the curseforge file for the file id {}",
                        file_ids[i]
                    )
                };

                match try_from_cf_file(SourceKind::Modpacks, file, None) {
                    Ok((_metadata, mut downloadable)) => {
                        downloadable.output = PathBuf::from(
                            if Path::new(&downloadable.filename())
                                .extension()
                                .is_some_and(|ext| ext.eq_ignore_ascii_case(".zip"))
                            {
                                "resourcepacks"
                            } else {
                                "mods"
                            },
                        )
                        .join(downloadable.filename());
                        to_download.push(downloadable);
                    }
                    Err(DistributionDeniedError(mod_id, file_id)) => {
                        if !msg_shown {
                            println!("\n{}", "The following mod(s) have denied 3rd parties such as Ferium from downloading it".red().bold());
                        }
                        msg_shown = true;
                        tasks.spawn(async move {
                            let project = CURSEFORGE_API.get_mod(mod_id).await?;
                            eprintln!(
                                "- {}
                           \r  {}",
                                project.name.bold(),
                                format!("{}/download/{file_id}", project.links.website_url)
                                    .blue()
                                    .underline(),
                            );
                            Ok::<(), furse::Error>(())
                        });
                    }
                }
            }

            if install_overrides {
                let tmp_dir = get_tmp_dir()?.join(manifest.name);
                zip_extract(path, &tmp_dir)?;
                read_overrides(to_download, &tmp_dir.join(manifest.overrides))?;
            }
        }
        SourceKindWithModpack::ModpacksModrinth => {
            let metadata: modrinth::Metadata = serde_json::from_str(
                &read_file_from_zip(BufReader::new(modpack_file), "modrinth.index.json")?
                    .context("Does not contain metadata file")?,
            )?;

            for file in metadata.files {
                to_download.push(from_modpack_file(file));
            }

            if install_overrides {
                let tmp_dir = get_tmp_dir()?.join(metadata.name);
                zip_extract(path, &tmp_dir)?;
                read_overrides(to_download, &tmp_dir.join("overrides"))?;
            }
        }
        _ => bail!("That is not a modpack!"),
    }

    Ok(())
}

/// Warn if a filter is potentially not strict enough.
fn check_unstrict_filter(filters: &Filters) {
    if let Some(mod_loaders) = &filters.mod_loaders {
        check_unstrict_mod_loaders(mod_loaders);
    }

    if let Some(versions) = &filters.versions {
        check_unstrict_versions(versions);
    }
}

fn check_unstrict_versions(versions: &Vec<Version>) {
    for version in versions {
        if version.is_strict() {
            return;
        }
    }

    warn!("specified unstrict version requirements");
}

fn check_unstrict_mod_loaders(mod_loaders: &Vec<ModLoader>) {
    let mut is_err = false;
    let mut loader = None;

    for mod_loader in mod_loaders {
        match mod_loader {
            ModLoader::Fabric | ModLoader::Quilt => {
                if loader.is_some() && loader != Some(ModLoader::Fabric) {
                    is_err = true;
                    break;
                }

                loader = Some(ModLoader::Fabric);
            }
            ModLoader::Forge => {
                if loader.is_some() && loader != Some(ModLoader::Forge) {
                    is_err = true;
                    break;
                }

                loader = Some(ModLoader::Fabric);
            }
            ModLoader::NeoForge => {
                if loader.is_some() && loader != Some(ModLoader::NeoForge) {
                    is_err = true;
                    break;
                }

                loader = Some(ModLoader::NeoForge);
            }
        }
    }

    if is_err {
        warn!("specified multiple possible mod loaders");
    }
}
