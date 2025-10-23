use std::{cmp, fmt, num::ParseIntError};

/// A list of predicates with an `OR` relationship,
/// e.g a range that is 1 `OR` 2 is `["1", "2"]`
#[derive(PartialEq, Eq, Debug, Clone)]
pub struct FabricVersionRange(Vec<FabricVersionPredicate>);

impl FabricVersionRange {
    pub fn matches(&self, other: &FabricVersion) -> bool {
        self.0.iter().any(|pred| pred.matches(other))
    }

    pub fn empty() -> Self {
        Self(vec![])
    }

    pub fn only(version: FabricVersion) -> Self {
        Self(vec![FabricVersionPredicate(
            FabricVersionPredicateIn::Terms(vec![FabricVersionTerm::Equal(version)]),
        )])
    }
}

impl FabricVersionRange {
    /// Parse a single predicate as a version range,
    /// e.g `>=1.2.3 <2-alpha`
    pub fn parse_single(s: &str) -> Result<Self> {
        Ok(Self(vec![FabricVersionPredicate::parse(s)?]))
    }

    /// Parse a list of predicates as a version range,
    /// e.g `[">1.0.0 <2-alpha", "=5", ">=8"]`
    pub fn parse_many<I: Iterator>(iter: I) -> Result<Self>
    where
        I::Item: AsRef<str>,
    {
        let res: Result<Vec<_>> = iter
            .map(|s| FabricVersionPredicate::parse(s.as_ref()))
            .collect();

        Ok(Self(res?))
    }
}

impl fmt::Display for FabricVersionRange {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let mut iter = self.0.iter();
        if let Some(x) = iter.next() {
            write!(f, "{x}")?;
        }
        for x in iter {
            write!(f, "|{x}")?;
        }
        Ok(())
    }
}

/// Space separated terms with an `AND` relationship,
/// e.g a predicate that is greater than or equal to 1 `AND` less than 2 is `>=1 <2`
#[derive(PartialEq, Eq, Debug, Clone)]
pub struct FabricVersionPredicate(FabricVersionPredicateIn);

#[derive(PartialEq, Eq, Debug, Clone)]
enum FabricVersionPredicateIn {
    Terms(Vec<FabricVersionTerm>),
    Any,
}

impl FabricVersionPredicate {
    pub fn matches(&self, other: &FabricVersion) -> bool {
        match &self.0 {
            FabricVersionPredicateIn::Terms(terms) => terms.iter().all(|term| term.matches(other)),
            FabricVersionPredicateIn::Any => true,
        }
    }
}

impl FabricVersionPredicate {
    pub fn parse(s: &str) -> Result<FabricVersionPredicate> {
        let s = s.trim();

        if s == "*" {
            return Ok(Self(FabricVersionPredicateIn::Any));
        }

        let mut terms = vec![];

        for s in s.split(' ') {
            terms.push(FabricVersionTerm::parse(s)?);
        }

        Ok(Self(FabricVersionPredicateIn::Terms(terms)))
    }
}

impl fmt::Display for FabricVersionPredicate {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match &self.0 {
            FabricVersionPredicateIn::Terms(terms) => {
                let mut iter = terms.iter();
                if let Some(x) = iter.next() {
                    write!(f, "{x}")?;
                }
                for x in iter {
                    write!(f, " {x}")?;
                }
                Ok(())
            }
            FabricVersionPredicateIn::Any => write!(f, "*"),
        }
    }
}

/// A single term in a fabric version range.
/// Such as `>=1.2.3` or `<4.0.0`
#[derive(PartialEq, Eq, Debug, Clone)]
enum FabricVersionTerm {
    Equal(FabricVersion),
    Greater(SemVer),
    GreaterOrEqual(SemVer),
    Less(SemVer),
    LessOrEqual(SemVer),
    Caret(SemVer),
    Tilde(SemVer),
}

impl FabricVersionTerm {
    pub fn matches(&self, version: &FabricVersion) -> bool {
        if let (
            FabricVersionTerm::Equal(FabricVersion(FabricVersionIn::String(range))),
            FabricVersionIn::String(version),
        ) = (self, &version.0)
        {
            return range == version;
        }

        let FabricVersion(FabricVersionIn::SemVer(version)) = version else {
            return false;
        };

        match self {
            FabricVersionTerm::Equal(FabricVersion(FabricVersionIn::SemVer(range))) => {
                version.cmp(range).is_eq()
            }
            FabricVersionTerm::Greater(range) => version.cmp(range).is_gt(),
            FabricVersionTerm::GreaterOrEqual(range) => version.cmp(range).is_ge(),
            FabricVersionTerm::Less(range) => version.cmp(range).is_lt(),
            FabricVersionTerm::LessOrEqual(range) => version.cmp(range).is_le(),
            FabricVersionTerm::Caret(range) => {
                if !version.cmp(range).is_ge() {
                    return false;
                }

                let mut range = range.clone();
                version.cmp(range.inc_major().prerelease()).is_lt()
            }
            FabricVersionTerm::Tilde(range) => {
                if !version.cmp(range).is_ge() {
                    return false;
                }

                let mut range = range.clone();
                version.cmp(range.inc_minor().prerelease()).is_lt()
            }
            _ => false,
        }
    }
}

impl FabricVersionTerm {
    fn parse(s: &str) -> Result<FabricVersionTerm> {
        if let Some(value) = Self::parse_eq(s, "=", FabricVersionTerm::Equal)? {
            return Ok(value);
        }
        if let Some(value) = Self::parse_op(s, ">=", FabricVersionTerm::GreaterOrEqual)? {
            return Ok(value);
        }
        if let Some(value) = Self::parse_op(s, ">", FabricVersionTerm::Greater)? {
            return Ok(value);
        }
        if let Some(value) = Self::parse_op(s, "<=", FabricVersionTerm::LessOrEqual)? {
            return Ok(value);
        }
        if let Some(value) = Self::parse_op(s, "<", FabricVersionTerm::Less)? {
            return Ok(value);
        }
        if let Some(value) = Self::parse_op(s, "^", FabricVersionTerm::Caret)? {
            return Ok(value);
        }
        if let Some(value) = Self::parse_op(s, "~", FabricVersionTerm::Tilde)? {
            return Ok(value);
        }

        Ok(FabricVersionTerm::Equal(FabricVersion::parse(s, true)?))
    }

    fn parse_eq(
        s: &str,
        symbol: &str,
        op: impl Fn(FabricVersion) -> FabricVersionTerm,
    ) -> Result<Option<FabricVersionTerm>> {
        match s.strip_prefix(symbol) {
            Some(s) => Ok(Some(op(FabricVersion::parse(s, true)?))),
            None => Ok(None),
        }
    }

    fn parse_op(
        s: &str,
        symbol: &str,
        op: impl Fn(SemVer) -> FabricVersionTerm,
    ) -> Result<Option<FabricVersionTerm>> {
        match s.strip_prefix(symbol) {
            Some(s) => Ok(Some(op(SemVer::parse(s, true)?))),
            None => Ok(None),
        }
    }
}

impl fmt::Display for FabricVersionTerm {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            FabricVersionTerm::Equal(version) => write!(f, "={version}"),
            FabricVersionTerm::Greater(ver) => write!(f, ">{ver}"),
            FabricVersionTerm::GreaterOrEqual(ver) => write!(f, ">={ver}"),
            FabricVersionTerm::Less(ver) => write!(f, "<{ver}"),
            FabricVersionTerm::LessOrEqual(ver) => write!(f, "<={ver}"),
            FabricVersionTerm::Caret(ver) => write!(f, "^{ver}"),
            FabricVersionTerm::Tilde(ver) => write!(f, "~{ver}"),
        }
    }
}

/// A version used by Fabric mods.
#[derive(PartialEq, Eq, Debug, Clone)]
pub struct FabricVersion(FabricVersionIn);

#[derive(PartialEq, Eq, Debug, Clone)]
enum FabricVersionIn {
    SemVer(SemVer),
    String(String),
}

impl FabricVersion {
    /// Try parse a string as [`SemVer`] or fallback to a string.
    /// `allow_wildcards` allows forms such as `1.x` or `1.*` to be used.
    pub fn parse(s: &str, allow_wildcards: bool) -> Result<Self> {
        match SemVer::parse(s, allow_wildcards) {
            Ok(value) => Ok(Self(FabricVersionIn::SemVer(value))),
            Err(Error::Empty | Error::ParseInt(_)) => {
                Ok(Self(FabricVersionIn::String(s.to_string())))
            }
            Err(e) => Err(e),
        }
    }
}

impl cmp::Ord for FabricVersion {
    fn cmp(&self, other: &Self) -> cmp::Ordering {
        match (&self.0, &other.0) {
            (FabricVersionIn::SemVer(a), FabricVersionIn::SemVer(b)) => a.cmp(b),
            (FabricVersionIn::String(a), FabricVersionIn::String(b)) => a.cmp(b),
            (FabricVersionIn::SemVer(_), FabricVersionIn::String(_)) => cmp::Ordering::Greater,
            (FabricVersionIn::String(_), FabricVersionIn::SemVer(_)) => cmp::Ordering::Less,
        }
    }
}

impl cmp::PartialOrd for FabricVersion {
    fn partial_cmp(&self, other: &Self) -> Option<cmp::Ordering> {
        Some(self.cmp(other))
    }
}

impl fmt::Display for FabricVersion {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match &self.0 {
            FabricVersionIn::SemVer(version) => write!(f, "{version}"),
            FabricVersionIn::String(s) => write!(f, "{s}"),
        }
    }
}

/// An extended version of SemVer.
/// See also [`FabricVersion`].
#[derive(PartialEq, Eq, Clone)]
pub struct SemVer {
    components: Vec<u64>,
    prerelease: Option<Vec<PrereleaseComponent>>,
    build: Option<String>,
    has_wildcard: bool,
}

impl fmt::Debug for SemVer {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "SemVer({self})")
    }
}

impl fmt::Display for SemVer {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        for (i, x) in self.components.iter().enumerate() {
            if i != 0 {
                write!(f, ".")?;
            }
            write!(f, "{x}")?;
        }
        if self.has_wildcard {
            write!(f, ".*")?;
        }
        if let Some(prerelease) = &self.prerelease {
            write!(f, "-")?;
            for (i, component) in prerelease.iter().enumerate() {
                if i != 0 {
                    write!(f, ".")?;
                }
                match component {
                    PrereleaseComponent::Number(x) => write!(f, "{x}")?,
                    PrereleaseComponent::String(x) => write!(f, "{x}")?,
                }
            }
        }
        if let Some(build) = &self.build {
            write!(f, "+{build}")?;
        }
        Ok(())
    }
}

impl SemVer {
    /// Parse a string as a version.
    /// `allow_wildcards` allows forms such as `1.x` or `1.*` to be used.
    pub fn parse(s: &str, allow_wildcards: bool) -> Result<Self> {
        let core = s.trim();

        // Parse the build metadata.
        let (core, build) = match core.split_once('+') {
            Some((core, build)) => (core, Some(build.to_string())),
            None => (core, None),
        };

        // Parse the pre-release version.
        let (core, prerelease) = match core.split_once('-') {
            Some((core, prerelease)) => (
                core,
                Some(
                    prerelease
                        .split('.')
                        .filter(|x| !x.is_empty())
                        .map(PrereleaseComponent::parse)
                        .collect(),
                ),
            ),
            None => (core, None),
        };

        if core.is_empty() {
            return Err(Error::Empty);
        }

        // Parse the components of the version, e.g 1.2.x
        let parts = core.split('.');
        let mut components = vec![];
        let mut has_wildcard = false;

        for part in parts {
            let is_wildcard = ["x", "X", "*"].contains(&part);

            if is_wildcard {
                if !allow_wildcards {
                    return Err(Error::WildcardsDisallowed);
                }
                if prerelease.is_some() {
                    // Pre-release versions cannot have wildcards
                    return Err(Error::WildcardsInPrerelease);
                }
                if components.is_empty() {
                    // Versions cannot start with a wildcard
                    return Err(Error::StartsWithWildcard);
                }
                has_wildcard = true;
            } else if has_wildcard {
                return Err(Error::InterjacentWildcards);
            } else {
                components.push(part.parse()?);
            }
        }

        Ok(Self {
            components,
            prerelease,
            build,
            has_wildcard,
        })
    }

    fn inc_major(&mut self) -> &mut Self {
        self.extend_to(1);
        self.components[0] += 1;
        self
    }

    fn inc_minor(&mut self) -> &mut Self {
        self.extend_to(2);
        self.components[1] += 1;
        self
    }

    fn extend_to(&mut self, size: usize) {
        for _ in 0..(size.saturating_sub(self.components.len())) {
            self.components.push(0);
        }
    }

    fn prerelease(&mut self) -> &mut Self {
        self.prerelease = Some(vec![]);
        self
    }
}

#[derive(PartialEq, Eq, Clone, Debug)]
enum PrereleaseComponent {
    Number(u64),
    String(String),
}

impl PrereleaseComponent {
    fn parse(s: &str) -> Self {
        match s.parse() {
            Ok(value) => Self::Number(value),
            Err(_) => Self::String(s.to_string()),
        }
    }
}

impl SemVer {
    /// Get a component from a version.
    ///
    /// # Examples
    ///
    /// ```ignore
    /// let ver = SemVer::parse("1.2.3").unwrap();
    /// assert_eq!(ver.get(1), Component::Component(1));
    /// assert_eq!(ver.get(2), Component::Component(2));
    /// assert_eq!(ver.get(3), Component::Component(3));
    /// assert_eq!(ver.get(4), Component::Component(0));
    ///
    /// let ver = SemVer::parse("1.2.x").unwrap();
    /// assert_eq!(ver.get(1), Component::Component(1));
    /// assert_eq!(ver.get(2), Component::Component(2));
    /// assert_eq!(ver.get(3), Component::Wildcard);
    /// assert_eq!(ver.get(4), Component::Wildcard);
    /// ```
    fn get(&self, index: usize) -> Component {
        match self.components.get(index) {
            Some(value) => Component::Component(*value),
            None => {
                if self.has_wildcard {
                    Component::Wildcard
                } else {
                    Component::Component(0)
                }
            }
        }
    }
}

#[derive(PartialEq, Eq, Debug)]
enum Component {
    Component(u64),
    Wildcard,
}

impl cmp::Ord for SemVer {
    fn cmp(&self, other: &Self) -> cmp::Ordering {
        let len = self.components.len().max(other.components.len()) + 1;

        for i in 0..len {
            match (self.get(i), other.get(i)) {
                (Component::Wildcard, _) | (_, Component::Wildcard) => return cmp::Ordering::Equal,
                (Component::Component(this), Component::Component(other)) => {
                    match this.cmp(&other) {
                        cmp::Ordering::Equal => {}
                        ord => return ord,
                    }
                }
            }
        }

        let (this, other) = match (&self.prerelease, &other.prerelease) {
            (None, None) => return cmp::Ordering::Equal,
            (None, Some(_)) => return cmp::Ordering::Greater,
            (Some(_), None) => return cmp::Ordering::Less,
            (Some(this), Some(other)) => (this, other),
        };

        for (this, other) in this.iter().zip(other.iter()) {
            let ord = match (this, other) {
                (PrereleaseComponent::Number(a), PrereleaseComponent::Number(b)) => a.cmp(b),
                (PrereleaseComponent::String(a), PrereleaseComponent::String(b)) => a.cmp(b),
                (PrereleaseComponent::Number(_), PrereleaseComponent::String(_)) => {
                    cmp::Ordering::Less
                }
                (PrereleaseComponent::String(_), PrereleaseComponent::Number(_)) => {
                    cmp::Ordering::Greater
                }
            };

            match ord {
                cmp::Ordering::Equal => {}
                ord => return ord,
            }
        }

        this.len().cmp(&other.len())
    }
}

impl cmp::PartialOrd for SemVer {
    fn partial_cmp(&self, other: &Self) -> Option<cmp::Ordering> {
        Some(self.cmp(other))
    }
}

pub type Result<T> = std::result::Result<T, Error>;

#[derive(thiserror::Error, Debug)]
pub enum Error {
    #[error("pre-release versions are not allowed to use wildcards")]
    WildcardsInPrerelease,
    #[error("wildcards are disallowed in this context")]
    WildcardsDisallowed,
    #[error("interjacent wildcards (e.g., 1.x.2) are disallowed")]
    InterjacentWildcards,
    #[error("version cannot start with a wildcard")]
    StartsWithWildcard,
    #[error(transparent)]
    ParseInt(#[from] ParseIntError),
    #[error("empty")]
    Empty,
}

#[cfg(test)]
mod test {
    use super::*;

    #[test]
    fn version_range() {
        let range = FabricVersionRange::parse_single("1.0.0").unwrap();
        assert!(range.matches(&FabricVersion::parse("1", true).unwrap()));
        let range = FabricVersionRange::parse_single("1").unwrap();
        assert!(range.matches(&FabricVersion::parse("1.0.0", true).unwrap()));

        let range = FabricVersionRange::parse_single("1.x").unwrap();
        assert!(range.matches(&FabricVersion::parse("1", true).unwrap()));
        assert!(range.matches(&FabricVersion::parse("1-", true).unwrap()));
        assert!(!range.matches(&FabricVersion::parse("0", true).unwrap()));

        let range = FabricVersionRange::parse_single("<1").unwrap();
        assert!(!range.matches(&FabricVersion::parse("1", true).unwrap()));
        assert!(range.matches(&FabricVersion::parse("1-", true).unwrap()));
        assert!(range.matches(&FabricVersion::parse("0", true).unwrap()));

        let range = FabricVersionRange::parse_single("=abc").unwrap();
        assert!(!range.matches(&FabricVersion::parse("1", true).unwrap()));
        assert!(!range.matches(&FabricVersion::parse("hello", true).unwrap()));
        assert!(range.matches(&FabricVersion::parse("abc", true).unwrap()));
    }

    #[test]
    fn ord_semver() {
        let mut vers = [
            SemVer::parse("2.0.0", false).unwrap(),
            SemVer::parse("2-beta", false).unwrap(),
            SemVer::parse("2-alpha.beta", false).unwrap(),
            SemVer::parse("2-alpha", false).unwrap(),
            SemVer::parse("2-2", false).unwrap(),
            SemVer::parse("2-1", false).unwrap(),
            SemVer::parse("2-", false).unwrap(),
            SemVer::parse("1.0", false).unwrap(),
        ];

        let expected = {
            let mut value = vers.clone();
            value.reverse();
            value
        };

        vers.sort();

        assert_eq!(vers, expected);
    }

    #[test]
    fn parse_term() {
        assert_eq!(
            FabricVersionTerm::parse(">=1").unwrap(),
            FabricVersionTerm::GreaterOrEqual(SemVer {
                components: vec![1],
                prerelease: None,
                build: None,
                has_wildcard: false,
            })
        );
        assert_eq!(
            FabricVersionTerm::parse("<1").unwrap(),
            FabricVersionTerm::Less(SemVer {
                components: vec![1],
                prerelease: None,
                build: None,
                has_wildcard: false,
            })
        );
        assert_eq!(
            FabricVersionTerm::parse("=1").unwrap(),
            FabricVersionTerm::Equal(FabricVersion(FabricVersionIn::SemVer(SemVer {
                components: vec![1],
                prerelease: None,
                build: None,
                has_wildcard: false,
            })))
        );
        assert_eq!(
            FabricVersionTerm::parse("=abc").unwrap(),
            FabricVersionTerm::Equal(FabricVersion(FabricVersionIn::String("abc".into())))
        );
    }

    #[test]
    fn parse_semver() {
        assert_eq!(
            SemVer::parse("1.0-alpha.1+mc1.20.1", true).unwrap(),
            SemVer {
                components: vec![1, 0],
                prerelease: Some(vec![
                    PrereleaseComponent::String("alpha".into()),
                    PrereleaseComponent::Number(1)
                ]),
                build: Some("mc1.20.1".into()),
                has_wildcard: false,
            }
        );
        assert_eq!(
            SemVer::parse("1.2.x.*.X", true).unwrap(),
            SemVer {
                components: vec![1, 2],
                prerelease: None,
                build: None,
                has_wildcard: true,
            }
        );
        assert!(matches!(
            SemVer::parse("1.x.1", true).expect_err("interjacent wildcards"),
            Error::InterjacentWildcards,
        ));
        assert!(matches!(
            SemVer::parse("1.x-alpha", true).expect_err("wildcards in prerelease"),
            Error::WildcardsInPrerelease,
        ));
        assert!(matches!(
            SemVer::parse("", true).expect_err("empty"),
            Error::Empty,
        ));
        assert!(matches!(
            SemVer::parse("*", true).expect_err("starts with wildcard"),
            Error::StartsWithWildcard,
        ));
        assert!(matches!(
            SemVer::parse("1.*", false).expect_err("wildcards disallowed"),
            Error::WildcardsDisallowed,
        ));
        assert!(matches!(
            SemVer::parse("abc", true).expect_err("not a number"),
            Error::ParseInt(_),
        ));
    }
}
