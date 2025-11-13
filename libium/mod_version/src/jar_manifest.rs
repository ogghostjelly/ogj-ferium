use std::collections::HashMap;

use crate::forge::{self, version::ForgeVersion};

pub fn parse(s: &str) -> Result<HashMap<&str, &str>, Error> {
    let mut map = HashMap::new();

    for line in s.lines() {
        let Some((k, v)) = line.split_once(':') else {
            return Err(Error::MissingDelimeter);
        };

        map.insert(k, v.trim());
    }

    Ok(map)
}

pub fn extract_implementation_version(s: &str) -> Result<ForgeVersion, Error> {
    match parse(s)?.get("Implementation-Version") {
        Some(s) => Ok(ForgeVersion::parse(s)?),
        None => Err(Error::MissingImplementationVersion),
    }
}

#[derive(thiserror::Error, Debug)]
pub enum Error {
    #[error("missing ':' property delimeter")]
    MissingDelimeter,
    #[error("META-INF/MANIFEST.MF missing Implementation-Version")]
    MissingImplementationVersion,
    #[error("Implementation-Version: {0}")]
    Forge(#[from] forge::version::Error),
}
