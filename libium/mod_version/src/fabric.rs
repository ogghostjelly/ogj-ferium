use std::{collections::HashMap, fmt, path::PathBuf};

use serde::{
    Deserialize,
    de::{self, Error as _},
};

pub mod version;

use version::FabricVersion;

use crate::fabric::version::FabricVersionRange;

/// A `fabric.mod.json` file.
/// Contains metadata about a fabric mod.
#[derive(Deserialize, PartialEq, Eq, Debug)]
#[serde(rename_all = "camelCase")]
pub struct ModJson {
    pub schema_version: SchemaVersion,
    pub id: ModId,
    pub version: FabricVersion,
    #[serde(default)]
    pub provides: Vec<ModId>,
    #[serde(default)]
    pub environment: Environment,
    #[serde(default)]
    pub jars: Vec<JarPath>,

    #[serde(default)]
    pub depends: HashMap<ModId, FabricVersionRange>,
    #[serde(default)]
    pub recommends: HashMap<ModId, FabricVersionRange>,
    #[serde(default)]
    pub suggests: HashMap<ModId, FabricVersionRange>,
    #[serde(default)]
    pub breaks: HashMap<ModId, FabricVersionRange>,
    #[serde(default)]
    pub conflicts: HashMap<ModId, FabricVersionRange>,
}

#[derive(Deserialize, PartialEq, Eq, Debug)]
pub struct JarPath {
    pub file: PathBuf,
}

#[derive(Deserialize, PartialEq, Eq, Debug, Default)]
#[serde(rename_all = "lowercase")]
pub enum Environment {
    Client,
    Server,
    #[serde(alias = "*")]
    #[default]
    Any,
}

#[derive(Deserialize, PartialEq, Eq, Debug)]
#[serde(try_from = "u64")]
pub struct SchemaVersion;

impl TryFrom<u64> for SchemaVersion {
    type Error = Error;

    fn try_from(value: u64) -> Result<Self, Self::Error> {
        if value == 1 {
            Ok(Self)
        } else {
            Err(Error::InvalidSchemaVersion)
        }
    }
}

#[derive(Deserialize, PartialEq, Eq, Debug, Hash, Clone)]
#[serde(try_from = "String")]
pub struct ModId(pub String);

impl TryFrom<String> for ModId {
    type Error = Error;

    fn try_from(value: String) -> Result<Self, Self::Error> {
        if (2..=64).contains(&value.len()) {
            Ok(ModId(value.to_string()))
        } else {
            Err(Error::ModIdTooLong)
        }
    }
}

impl fmt::Display for ModId {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.0)
    }
}

impl<'de> Deserialize<'de> for FabricVersionRange {
    fn deserialize<D>(deserializer: D) -> Result<Self, D::Error>
    where
        D: de::Deserializer<'de>,
    {
        struct V;

        impl<'de> de::Visitor<'de> for V {
            type Value = FabricVersionRange;

            fn expecting(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
                write!(f, "a version string or list of versions")
            }

            fn visit_str<E>(self, v: &str) -> Result<Self::Value, E>
            where
                E: de::Error,
            {
                FabricVersionRange::parse_single(v).map_err(E::custom)
            }

            fn visit_seq<A>(self, mut seq: A) -> Result<Self::Value, A::Error>
            where
                A: de::SeqAccess<'de>,
            {
                let mut ls: Vec<String> = Vec::with_capacity(seq.size_hint().unwrap_or(0));
                while let Some(value) = seq.next_element()? {
                    ls.push(value);
                }
                FabricVersionRange::parse_many(ls.into_iter()).map_err(A::Error::custom)
            }
        }

        deserializer.deserialize_any(V)
    }
}

impl<'de> Deserialize<'de> for FabricVersion {
    fn deserialize<D>(deserializer: D) -> Result<Self, D::Error>
    where
        D: serde::Deserializer<'de>,
    {
        struct V;

        impl<'de> de::Visitor<'de> for V {
            type Value = FabricVersion;

            fn expecting(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
                write!(f, "a fabric version string")
            }

            fn visit_str<E>(self, v: &str) -> Result<Self::Value, E>
            where
                E: de::Error,
            {
                FabricVersion::parse(v, false).map_err(E::custom)
            }
        }

        deserializer.deserialize_str(V)
    }
}

#[derive(thiserror::Error, Debug)]
pub enum Error {
    #[error("mod id must be between 2-64 characters")]
    ModIdTooLong,
    #[error("invalid schema version is not 1")]
    InvalidSchemaVersion,
}

#[cfg(test)]
mod test {
    use super::*;

    #[test]
    fn parse_mod_json() {
        let s = r#"{
            "schemaVersion": 1,
            "id": "examplemod",
            "version": "1.0.0-pre+build",
            "name": "Example Mod",
            "description": "Lorem ipsum dolor sit amet.",
            "environment": "client",
            "depends": {
                "minecraft": [
                    "1.21",
                    "1.20.1"
                ]
            },
            "jars": [
                {
                    "file": "META-INF/jars/example.jar"
                }
            ]
        }"#;
        let v: ModJson = match serde_json::from_str(s) {
            Ok(value) => value,
            Err(e) => panic!("{e}"),
        };
        assert_eq!(
            v,
            ModJson {
                schema_version: SchemaVersion,
                id: ModId("examplemod".into()),
                version: FabricVersion::parse("1.0.0-pre+build", false).unwrap(),
                provides: vec![],
                environment: Environment::Client,
                jars: vec![JarPath {
                    file: "META-INF/jars/example.jar".into()
                }],
                depends: HashMap::from([(
                    ModId("minecraft".into()),
                    FabricVersionRange::parse_many(["1.21", "1.20.1"].iter()).unwrap()
                )]),
                recommends: Default::default(),
                suggests: Default::default(),
                breaks: Default::default(),
                conflicts: Default::default(),
            }
        )
    }
}
