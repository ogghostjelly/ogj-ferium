# Mod Version

Library to parse version and dependency data from a Fabric or Forge mod, using the `fabric.mod.json` and `META-INF/mods.toml` files respectively.

# Features

- Parser for the [`fabric.mod.json`](https://wiki.fabricmc.net/documentation:fabric_mod_json_spec) and [`META-INF/mods.toml`](https://docs.minecraftforge.net/en/latest/gettingstarted/modfiles/) files
- Parser for the extended semantic version and version ranges used by Fabric described in the specification [here](https://wiki.fabricmc.net/documentation:fabric_mod_json_spec#versionrange)
- Parser for Maven versions and version ranges used by Forge described in the specification [here](https://cwiki.apache.org/confluence/display/MAVENOLD/Versioning) and [here](https://maven.apache.org/enforcer/enforcer-rules/versionRanges.html).

# Limitations

Currently, the parser for the Forge `META-INF/mods.toml` does not take into account string substitution via the `properties` field.
