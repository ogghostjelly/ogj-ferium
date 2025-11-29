use std::{collections::HashMap, io, str::FromStr as _};

mod keybind;
mod keycode;

pub use keybind::KeyKey;
pub use keycode::Keycode;
use serde::{Deserialize, Serialize};
use thiserror::Error;

/// Struct for processing the `.minecraft/options.txt` file.
/// Use the [Options::read] and [Options::write] methods for serialization and deserialization.
#[derive(Default)]
pub struct Options {
    fields: HashMap<String, String>,
}

impl Options {
    /// Is the `options.txt` from Minecraft 1.14 onwards,
    /// will be `false` for 1.13 and below.
    pub fn is_post_1_13(&self) -> bool {
        self.fields
            .get("version")
            .and_then(|ver| ver.parse().ok())
            .map(|ver: u64| ver > 1631)
            .unwrap_or(false)
    }

    /// Set a keybind in `options.txt`
    ///
    /// # Examples
    /// ```
    /// # use ogj_libium::config::options::{Keycode, KeyKey, Options};
    /// # let mut options = Options::default();
    /// // Post-1.13 `key_key.attack:key.keyboard.w`
    /// // Pre-1.13  `key_key.attack:17`
    /// options.set_keybind(KeyKey::Attack, Keycode::W);
    /// ```
    pub fn set_keybind(&mut self, keybind: impl ToString, keycode: Keycode) {
        self.fields.insert(
            keybind.to_string(),
            if self.is_post_1_13() {
                keycode.id_post1_13().to_string()
            } else {
                keycode.id_pre1_13().to_string()
            },
        );
    }

    /// Set a field in `options.txt`
    ///
    /// # Examples
    /// ```
    /// # use ogj_libium::config::options::{Keycode, KeyKey, Options};
    /// # let mut options = Options::default();
    /// options.set_field("fancyGraphics", true);
    /// ```
    pub fn set_field(&mut self, key: impl ToString, value: impl ToString) {
        self.fields.insert(key.to_string(), value.to_string());
    }
}

impl Options {
    /// Write the `options.txt` file to a writer.
    pub fn write<W: io::Write>(
        &self,
        write: &mut W,
        mut _err_handler: impl FnMut(WriteError) + Clone,
    ) -> Result<(), io::Error> {
        let mut iter: Vec<(&String, &String)> = self.fields.iter().collect();

        iter.sort();

        for (key, value) in iter {
            // Remove string quotes since that breaks the options file for some reason.
            let value = if value.starts_with('"') && value.ends_with('"') {
                &value[1..value.len() - 1]
            } else {
                value
            };

            writeln!(write, "{key}:{value}")?;
        }
        Ok(())
    }

    /// Read the `options.txt` file from a reader.
    pub fn read<R: io::BufRead>(
        reader: R,
        mut err_handler: impl FnMut(ReadError) + Clone,
    ) -> Result<Self, io::Error> {
        let mut options = HashMap::new();

        for (lineno, line) in reader.lines().enumerate() {
            let mut line = line?;

            let Some(index) = line.find(':') else {
                err_handler(ReadError::MissingDelimiter(lineno + 1, line));
                continue;
            };

            let mut value = line.split_off(index);
            value.remove(0);
            let key = line;

            options.insert(key, value);
        }

        Ok(Options { fields: options })
    }
}

/// Overrides defined in the profile that can be applied to `options.txt`. See [Options::apply].
#[derive(Serialize, Deserialize, Default, Debug, Clone)]
#[serde(rename_all = "kebab-case")]
pub struct OptionsOverrides {
    #[serde(default, skip_serializing_if = "toml::Table::is_empty")]
    keybinds: toml::Table,
    #[serde(flatten)]
    fields: toml::Table,
}

impl OptionsOverrides {
    pub fn is_empty(&self) -> bool {
        self.keybinds.is_empty() && self.fields.is_empty()
    }

    pub fn join(&mut self, other: &Self) {
        for (k, v) in &other.keybinds {
            self.keybinds.insert(k.clone(), v.clone());
        }

        for (k, v) in &other.fields {
            self.fields.insert(k.clone(), v.clone());
        }
    }
}

impl Options {
    /// Apply overrides to the options.
    pub fn apply(
        &mut self,
        overrides: OptionsOverrides,
        mut err_handler: impl FnMut(OverrideError) + Clone,
    ) {
        for (key, value) in overrides.keybinds {
            self.apply_keybinds(&key, value, err_handler.clone());
        }

        for (key, value) in overrides.fields {
            if key.starts_with("key_") {
                err_handler(OverrideError::BadFieldPrefix(key.clone()));
            }

            self.set_field(key, value);
        }
    }

    /// Apply a TOML value of keybinds to the options.
    fn apply_keybinds(
        &mut self,
        key: &str,
        value: toml::Value,
        mut err_handler: impl FnMut(OverrideError) + Clone,
    ) {
        match value {
            toml::Value::String(string) => match string_to_keycode(string) {
                Ok(value) => self.set_keybind_with_err(key, value, err_handler),
                Err(e) => err_handler(e),
            },
            toml::Value::Integer(num) => match i64_to_keycode(num) {
                Ok(value) => self.set_keybind_with_err(key, value, err_handler),
                Err(e) => err_handler(e),
            },
            toml::Value::Table(table) => {
                for (next_key, value) in table {
                    let key = format!("{key}.{next_key}");
                    self.apply_keybinds(&key, value, err_handler.clone());
                }
            }
            v @ toml::Value::Float(_)
            | v @ toml::Value::Boolean(_)
            | v @ toml::Value::Datetime(_)
            | v @ toml::Value::Array(_) => {
                err_handler(OverrideError::InvalidKeycodeType(v.type_str()))
            }
        }
    }

    fn set_keybind_with_err(
        &mut self,
        keybind: &str,
        keycode: Keycode,
        mut err_handler: impl FnMut(OverrideError) + Clone,
    ) {
        if !keybind.starts_with("key_") {
            err_handler(OverrideError::BadKeybindPrefix(keybind.to_string()))
        }

        self.set_keybind(keybind, keycode);
    }
}

/// Convert a String to a Minecraft keycode with error handling.
fn string_to_keycode(string: String) -> Result<Keycode, OverrideError> {
    match Keycode::from_str(&string) {
        Ok(keycode) => Ok(keycode),
        Err(_) => Err(OverrideError::InvalidKeycodeId(string)),
    }
}

/// Convert an i64 to a Minecraft keycode with error handling.
fn i64_to_keycode(num: i64) -> Result<Keycode, OverrideError> {
    match num.try_into() {
        Ok(v) => match Keycode::from_repr(v) {
            Some(keycode) => Ok(keycode),
            None => Err(OverrideError::InvalidKeycodeNumber(v)),
        },
        Err(_) => Err(OverrideError::UnderflowKeycodeNumber(num)),
    }
}

#[derive(Error, Debug)]
pub enum OverrideError {
    #[error("invalid keycode type: {0}")]
    InvalidKeycodeType(&'static str),
    #[error("invalid keycode number: negative keycodes dont exist")]
    UnderflowKeycodeNumber(i64),
    #[error("invalid keycode number: keycode {0} not found")]
    InvalidKeycodeNumber(usize),
    #[error("invalid keycode id: keycode {0} not found")]
    InvalidKeycodeId(String),
    #[error("keybind '{0}' doesn't start with 'key_', are you sure this is a keybind?")]
    BadKeybindPrefix(String),
    #[error(
        "option '{0}' starts with 'key_', if this is a keybind you should put it in [options.keybinds]"
    )]
    BadFieldPrefix(String),
}

#[derive(Error, Debug)]
pub enum ReadError {
    #[error("option with no ':' delimiter found at lineno {0}, skipping\n  line: {1:?}")]
    MissingDelimiter(usize, String),
}

#[derive(Error, Debug)]
pub enum WriteError {}
