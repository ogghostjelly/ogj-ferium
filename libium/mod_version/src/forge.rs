use std::collections::HashMap;

use serde::{Deserialize, de};

use crate::forge::version::{ForgeVersion, ForgeVersionRange};

pub mod version;

pub type ModId = String;

/// A `META-INF/mods.toml` file.
/// Contains metadata about a forge mod.
#[derive(Deserialize, PartialEq, Debug)]
#[serde(rename_all = "camelCase")]
pub struct ModsToml {
    pub mods: Vec<Mod>,
    pub dependencies: HashMap<ModId, Vec<Dependency>>,
}

#[derive(Deserialize, PartialEq, Eq, Debug)]
#[serde(rename_all = "camelCase")]
pub struct Mod {
    pub mod_id: ModId,
    /// Defaults to "1"
    pub version: Option<MaybeForgeVersion>,
}

/// Can be a forge version or if the string "${file.jarVersion}" is used,
/// forge will replace the string with the 'Implementation Version' specified in the jar manifest.
#[derive(PartialEq, Eq, Debug)]
pub enum MaybeForgeVersion {
    ForgeVersion(ForgeVersion),
    ImplementationVersion,
}

#[derive(Deserialize, PartialEq, Eq, Debug)]
#[serde(rename_all = "camelCase")]
pub struct Dependency {
    pub mod_id: ModId,
    pub mandatory: bool,
    pub version_range: ForgeVersionRange,
}

impl<'de> Deserialize<'de> for MaybeForgeVersion {
    fn deserialize<D>(deserializer: D) -> Result<Self, D::Error>
    where
        D: de::Deserializer<'de>,
    {
        struct V;

        impl<'de> de::Visitor<'de> for V {
            type Value = MaybeForgeVersion;

            fn expecting(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
                write!(f, "a forge version")
            }

            fn visit_str<E>(self, v: &str) -> Result<Self::Value, E>
            where
                E: de::Error,
            {
                if v == "${file.jarVersion}" {
                    return Ok(MaybeForgeVersion::ImplementationVersion);
                }

                ForgeVersion::parse(v)
                    .map(MaybeForgeVersion::ForgeVersion)
                    .map_err(E::custom)
            }
        }

        deserializer.deserialize_str(V)
    }
}

impl<'de> Deserialize<'de> for ForgeVersionRange {
    fn deserialize<D>(deserializer: D) -> Result<Self, D::Error>
    where
        D: serde::Deserializer<'de>,
    {
        struct V;

        impl<'de> de::Visitor<'de> for V {
            type Value = ForgeVersionRange;

            fn expecting(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
                write!(f, "a forge version range")
            }

            fn visit_str<E>(self, v: &str) -> Result<Self::Value, E>
            where
                E: de::Error,
            {
                ForgeVersionRange::parse(v).map_err(E::custom)
            }
        }

        deserializer.deserialize_str(V)
    }
}

#[cfg(test)]
mod test {
    use super::*;

    #[test]
    fn parse_mods_toml() {
        let s = r#"
        modLoader="javafml"

        [[mods]]
        modId="examplemod"
        version="1.0.0.0"

        [[mods]]
        modId="othermod"
        version="${file.jarVersion}"

        [[dependencies.examplemod]]
            modId="forge"
            mandatory=true
            versionRange="[41,)"

        [[dependencies.examplemod]]
            modId="minecraft"
            mandatory=true
            versionRange="[1.19,1.20)""#;

        let v: ModsToml = match toml::from_str(s) {
            Ok(value) => value,
            Err(e) => panic!("{e}"),
        };
        assert_eq!(
            v,
            ModsToml {
                mods: vec![
                    Mod {
                        mod_id: "examplemod".into(),
                        version: Some(MaybeForgeVersion::ForgeVersion(
                            ForgeVersion::parse("1.0.0.0").unwrap()
                        )),
                    },
                    Mod {
                        mod_id: "othermod".into(),
                        version: Some(MaybeForgeVersion::ImplementationVersion),
                    }
                ],
                dependencies: HashMap::from([(
                    "examplemod".into(),
                    vec![
                        Dependency {
                            mod_id: "forge".into(),
                            mandatory: true,
                            version_range: ForgeVersionRange::parse("[41,)").unwrap(),
                        },
                        Dependency {
                            mod_id: "minecraft".into(),
                            mandatory: true,
                            version_range: ForgeVersionRange::parse("[1.19,1.20)").unwrap(),
                        }
                    ]
                ),]),
            }
        )
    }
}
