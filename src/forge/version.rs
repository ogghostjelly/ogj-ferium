use std::{cmp, fmt, mem};

/// A Maven version range used by Forge mods.
///
/// # Examples
/// `(a,b)` exclusive
/// `[a,b]` inclusive
/// `[1.0]` exact match
/// `1.0`   same as [1.0,) or x >= 1.0
/// `(,1.0],[1.2,)` x <= 1.0 OR x >= 1.2
#[derive(PartialEq, Eq, Debug, Clone)]
pub struct ForgeVersionRange(Vec<ForgeVersionRangeIn>);

#[derive(PartialEq, Eq, Debug, Clone)]
enum ForgeVersionRangeIn {
    Equal(ForgeVersion),
    Range(Bound, Bound),
}

#[derive(PartialEq, Eq, Debug, Clone)]
enum Bound {
    Inclusive(Option<ForgeVersion>),
    Exclusive(Option<ForgeVersion>),
}

impl ForgeVersionRange {
    pub fn matches(&self, version: &ForgeVersion) -> bool {
        self.0.iter().any(|range| range.matches(version))
    }

    pub fn empty() -> Self {
        Self(vec![])
    }

    pub fn only(version: ForgeVersion) -> Self {
        Self(vec![ForgeVersionRangeIn::Equal(version)])
    }
}

impl ForgeVersionRangeIn {
    pub fn matches(&self, version: &ForgeVersion) -> bool {
        let (lower, upper) = match self {
            ForgeVersionRangeIn::Equal(range) => return range.cmp(version).is_eq(),
            ForgeVersionRangeIn::Range(lower, upper) => (lower, upper),
        };

        lower.matches(version, cmp::Ordering::Greater)
            && upper.matches(version, cmp::Ordering::Less)
    }
}

impl Bound {
    fn matches(&self, version: &ForgeVersion, order: cmp::Ordering) -> bool {
        match self {
            Bound::Inclusive(Some(range)) => {
                let ord = version.cmp(range);
                ord == order || ord == cmp::Ordering::Equal
            }
            Bound::Exclusive(Some(range)) => version.cmp(range) == order,
            Bound::Inclusive(None) | Bound::Exclusive(None) => true,
        }
    }
}

impl ForgeVersionRange {
    pub fn parse(mut s: &str) -> Result<Self> {
        let mut terms = vec![];

        loop {
            s = s.trim();
            let Some((version, rest)) = Self::parse_in(s)? else {
                break;
            };
            terms.push(version);
            s = rest.trim();

            if s.is_empty() {
                break;
            }

            let Some(rest) = take_ch(s, ',') else {
                return Err(Error::Expected("','"));
            };
            s = rest;
        }

        Ok(Self(terms))
    }

    fn parse_in(input: &str) -> Result<Option<(ForgeVersionRangeIn, &str)>> {
        let input = input.trim();

        if let (Some(version), input) = Self::parse_version(input)? {
            return Ok(Some((
                ForgeVersionRangeIn::Range(Bound::Inclusive(Some(version)), Bound::Exclusive(None)),
                input,
            )));
        }

        if let Some(res) = Self::parse_list(input)? {
            return Ok(Some(res));
        }

        Ok(None)
    }

    fn parse_list(input: &str) -> Result<Option<(ForgeVersionRangeIn, &str)>> {
        let Some((open_inclusive, input)) = Self::parse_open(input) else {
            return Ok(None);
        };

        let (lower, input) = Self::parse_version(input)?;

        if let Some((close_inclusive, input)) = Self::parse_close(input) {
            return match lower {
                Some(lower) => {
                    if open_inclusive && close_inclusive {
                        Ok(Some((ForgeVersionRangeIn::Equal(lower), input)))
                    } else {
                        Err(Error::BadParensOneItem)
                    }
                }
                None => Err(Error::EmptyList),
            };
        }

        let Some(input) = take_ch(input, ',') else {
            return Err(Error::Expected("','"));
        };

        let (upper, input) = Self::parse_version(input)?;
        let Some((close_inclusive, input)) = Self::parse_close(input) else {
            return Err(Error::Expected("']' or ')'"));
        };

        let lower = Bound::from_bool(open_inclusive, lower);
        let upper = Bound::from_bool(close_inclusive, upper);

        Ok(Some((ForgeVersionRangeIn::Range(lower, upper), input)))
    }

    #[inline]
    fn parse_open(input: &str) -> Option<(bool, &str)> {
        Self::parse_bracks(input, '(', '[')
    }

    #[inline]
    fn parse_close(input: &str) -> Option<(bool, &str)> {
        Self::parse_bracks(input, ')', ']')
    }

    fn parse_bracks(input: &str, exclusive: char, inclusive: char) -> Option<(bool, &str)> {
        match take_ch(input, exclusive) {
            Some(s) => Some((false, s)),
            None => match take_ch(input, inclusive) {
                Some(s) => Some((true, s)),
                None => None,
            },
        }
    }

    fn parse_version(input: &str) -> Result<(Option<ForgeVersion>, &str)> {
        let (version, rest) = match input.find(|ch| "[](),".contains(ch)) {
            Some(i) => input.split_at(i),
            None => (input, ""),
        };

        let version = version.trim();

        if version.is_empty() {
            return Ok((None, rest));
        }

        Ok((Some(ForgeVersion::parse(version)?), rest))
    }
}

fn take_ch(s: &str, ch: char) -> Option<&str> {
    if s.starts_with(ch) {
        Some(&s[ch.len_utf8()..])
    } else {
        None
    }
}

impl Bound {
    pub fn from_bool(is_inclusive: bool, version: Option<ForgeVersion>) -> Self {
        if is_inclusive {
            Self::Inclusive(version)
        } else {
            Self::Exclusive(version)
        }
    }
}

impl fmt::Display for ForgeVersionRange {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let mut iter = self.0.iter();
        if let Some(range) = iter.next() {
            write!(f, "{range}")?;
        }
        for range in iter {
            write!(f, "{range}")?;
        }
        Ok(())
    }
}

impl fmt::Display for ForgeVersionRangeIn {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            ForgeVersionRangeIn::Equal(version) => write!(f, "[{version}]"),
            ForgeVersionRangeIn::Range(lower, upper) => write!(f, "{lower},{upper:#}"),
        }
    }
}

impl fmt::Display for Bound {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        fn display(
            f: &mut fmt::Formatter<'_>,
            (open, close): (char, char),
            version: &Option<ForgeVersion>,
        ) -> fmt::Result {
            if f.alternate() {
                if let Some(version) = version {
                    write!(f, "{version}")?;
                }
                write!(f, "{close}")
            } else {
                write!(f, "{open}")?;
                if let Some(version) = version {
                    write!(f, "{version}")?;
                }
                Ok(())
            }
        }

        match self {
            Bound::Inclusive(version) => display(f, ('[', ']'), version),
            Bound::Exclusive(version) => display(f, ('(', ')'), version),
        }
    }
}

/// A ComparableVersion from Maven used by Forge mods.
/// Based off of the spec described [here](https://cwiki.apache.org/confluence/display/MAVENOLD/Versioning) and the source code provided [here](https://maven.apache.org/ref/3.0/maven-artifact/xref/org/apache/maven/artifact/versioning/ComparableVersion.html).
#[derive(PartialEq, Eq, Clone)]
pub struct ForgeVersion(Vec<Item>);

impl fmt::Debug for ForgeVersion {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "ForgeVersion({self})")
    }
}

impl fmt::Display for ForgeVersion {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        for (i, item) in self.0.iter().enumerate() {
            if i != 0 {
                write!(f, ".")?;
            }
            match item {
                Item::String(x) => write!(f, "{x}")?,
                Item::Integer(x) => write!(f, "{x}")?,
                Item::Dash => write!(f, "-")?,
                Item::Null => write!(f, "<null>")?,
            }
        }
        Ok(())
    }
}

impl ForgeVersion {
    pub fn parse(s: &str) -> Result<Self> {
        Self::parse_from_chars(s.chars())
    }

    pub fn parse_from_chars<I: Iterator<Item = char>>(iter: I) -> Result<Self> {
        let mut parser = Parser::new();
        for ch in iter {
            parser.eat(ch)?;
        }
        Ok(Self(parser.consume()?))
    }
}

impl Ord for ForgeVersion {
    fn cmp(&self, other: &Self) -> cmp::Ordering {
        let iter = 0..self.0.len().max(other.0.len());

        let mut iter = iter.map(|i| {
            let a = self.0.get(i).unwrap_or(&Item::Null);
            let b = other.0.get(i).unwrap_or(&Item::Null);
            (a, b)
        });

        let Some((a, b)) = iter.next() else {
            return cmp::Ordering::Equal;
        };

        a.cmp_list(b, iter)
    }
}

impl PartialOrd for ForgeVersion {
    fn partial_cmp(&self, other: &Self) -> Option<cmp::Ordering> {
        Some(self.cmp(other))
    }
}

#[derive(PartialEq, Eq, Debug, Clone)]
enum Item {
    String(String),
    Integer(u64),
    Dash,
    Null,
}

const QUALIFIERS: &[&str] = &["alpha", "beta", "milestone", "rc", "snapshot", "", "sp"];
const ALIASES: &[(&str, &str)] = &[("ga", ""), ("final", ""), ("cr", "rc")];

impl Item {
    fn cmp_list<'a>(
        &self,
        other: &Self,
        mut iter: impl Iterator<Item = (&'a Item, &'a Item)>,
    ) -> cmp::Ordering {
        match self.cmp_item(other) {
            ItemOrdering::Ordering(cmp::Ordering::Equal) => {
                let Some((a, b)) = iter.next() else {
                    return cmp::Ordering::Equal;
                };

                Item::cmp_list(a, b, iter)
            }
            ItemOrdering::Ordering(cmp::Ordering::Less) => cmp::Ordering::Less,
            ItemOrdering::Ordering(cmp::Ordering::Greater) => cmp::Ordering::Greater,
            ItemOrdering::RecurseLeft => {
                let Some((left, _)) = iter.next() else {
                    return cmp::Ordering::Equal;
                };

                Item::cmp_list(left, &Item::Null, iter)
            }
            ItemOrdering::RecurseRight => {
                let Some((_, right)) = iter.next() else {
                    return cmp::Ordering::Equal;
                };

                Item::cmp_list(&Item::Null, right, iter)
            }
        }
    }

    fn cmp_item(&self, other: &Self) -> ItemOrdering {
        ItemOrdering::Ordering(match (self, other) {
            (Item::String(this), Item::String(other)) => cmp_string_string(this, other),
            (Item::Integer(this), Item::Integer(other)) => this.cmp(other),
            (Item::Null, Item::Null) => cmp::Ordering::Equal,
            (Item::Dash, Item::Dash) => cmp::Ordering::Equal,

            // Integer is newer when comparing for string and dash.
            (Item::String(_), Item::Integer(_)) => cmp::Ordering::Less,
            (Item::Integer(_), Item::String(_)) => cmp::Ordering::Greater,
            (Item::Dash, Item::Integer(_)) => cmp::Ordering::Less,
            (Item::Integer(_), Item::Dash) => cmp::Ordering::Greater,

            // Integer is newer when comparing null, or equal if the integer is 0
            (Item::Integer(0), Item::Null) | (Item::Null, Item::Integer(0)) => cmp::Ordering::Equal,
            (Item::Null, Item::Integer(_)) => cmp::Ordering::Less,
            (Item::Integer(_), Item::Null) => cmp::Ordering::Greater,

            // List is newer when comparing string.
            (Item::String(_), Item::Dash) => cmp::Ordering::Less,
            (Item::Dash, Item::String(_)) => cmp::Ordering::Greater,

            // String is newer when comparing null.
            (Item::Null, Item::String(s)) => cmp_string_string("", s),
            (Item::String(s), Item::Null) => cmp_string_string(s, ""),

            (Item::Dash, Item::Null) => return ItemOrdering::RecurseLeft,
            (Item::Null, Item::Dash) => return ItemOrdering::RecurseRight,
        })
    }
}

enum ItemOrdering {
    Ordering(cmp::Ordering),
    RecurseLeft,
    RecurseRight,
}

/// Compare to strings, handle special qualifiers like `alpha` or `rc`
fn cmp_string_string(this: &str, other: &str) -> cmp::Ordering {
    fn to_alias(s: &str) -> &str {
        match ALIASES.iter().find(|(alias, _)| *alias == s) {
            Some((_, s)) => s,
            None => s,
        }
    }

    let (this, other) = (to_alias(this), to_alias(other));

    let this_idx = QUALIFIERS.iter().position(|x| *x == this);
    let other_idx = QUALIFIERS.iter().position(|x| *x == other);

    match (this_idx, other_idx) {
        // Qualifier is less than no qualifier
        (Some(_), None) => return cmp::Ordering::Less,
        (None, Some(_)) => return cmp::Ordering::Greater,
        // Compare indicies
        (Some(a), Some(b)) => match a.cmp(&b) {
            cmp::Ordering::Equal => {}
            ord => return ord,
        },
        (None, None) => {}
    };

    this.cmp(other)
}

struct Parser {
    buf: String,
    last_char: Option<char>,
    data: Vec<Item>,
}

impl Parser {
    pub fn new() -> Self {
        Self {
            buf: String::new(),
            last_char: None,
            data: vec![],
        }
    }

    pub fn eat(&mut self, ch: char) -> Result<()> {
        match ch {
            '.' => self.push()?,
            '-' => {
                self.push()?;
                self.data.push(Item::Dash);
            }
            ch => {
                if self.last_char.is_some_and(|ch| ch.is_numeric()) == ch.is_numeric() {
                    self.buf.push(ch);
                } else {
                    self.push()?;
                    self.buf.push(ch);
                }
            }
        };
        self.last_char = Some(ch);
        Ok(())
    }

    fn consume(mut self) -> Result<Vec<Item>> {
        self.push()?;
        if self.data.is_empty() {
            return Err(Error::Empty);
        }
        Ok(self.data)
    }

    fn push(&mut self) -> Result<()> {
        let buf = mem::take(&mut self.buf);

        match buf.parse() {
            Ok(value) => {
                self.data.push(Item::Integer(value));
                return Ok(());
            }
            Err(e) => match e.kind() {
                std::num::IntErrorKind::PosOverflow => return Err(Error::IntPosOverflow(buf)),
                std::num::IntErrorKind::NegOverflow => return Err(Error::IntNegOverflow(buf)),
                _ => {}
            },
        }

        let buf = buf.trim().to_string();

        if !buf.is_empty() {
            self.data.push(Item::String(buf));
        }

        Ok(())
    }
}

pub type Result<T> = std::result::Result<T, Error>;

#[derive(thiserror::Error, Debug)]
pub enum Error {
    #[error("number too big: {0}")]
    IntPosOverflow(String),
    #[error("number too small: {0}")]
    IntNegOverflow(String),
    #[error("input is empty")]
    Empty,
    #[error("empty list is disallowed in version range")]
    EmptyList,
    #[error("lists with one item must use '[]' as parenthesis")]
    BadParensOneItem,
    #[error("expected {0}")]
    Expected(&'static str),
}

#[cfg(test)]
mod test {
    use super::*;

    #[test]
    fn version_range() {
        // x <= 1.0 or x >= 1.2
        let range = ForgeVersionRange::parse("(,1.0],[1.2,)").unwrap();
        assert!(range.matches(&ForgeVersion::parse("1.0").unwrap()));
        assert!(!range.matches(&ForgeVersion::parse("1.1").unwrap()));
        assert!(range.matches(&ForgeVersion::parse("1.2").unwrap()));

        // x >= 1.0
        let range = ForgeVersionRange::parse("1.0").unwrap();
        assert!(range.matches(&ForgeVersion::parse("1.0").unwrap()));
        assert!(range.matches(&ForgeVersion::parse("1.1").unwrap()));
        assert!(range.matches(&ForgeVersion::parse("1.2").unwrap()));

        // x == 1.0
        let range = ForgeVersionRange::parse("[1.0]").unwrap();
        assert!(range.matches(&ForgeVersion::parse("1.0").unwrap()));
        assert!(!range.matches(&ForgeVersion::parse("1.1").unwrap()));
        assert!(!range.matches(&ForgeVersion::parse("1.2").unwrap()));

        // x != 1.1
        let range = ForgeVersionRange::parse("(,1.1),(1.1,)").unwrap();
        assert!(range.matches(&ForgeVersion::parse("1.0").unwrap()));
        assert!(!range.matches(&ForgeVersion::parse("1.1").unwrap()));
        assert!(range.matches(&ForgeVersion::parse("1.2").unwrap()));

        // x != 1.1
        let range = ForgeVersionRange::parse("(,1.1),(1.1,)").unwrap();
        assert!(range.matches(&ForgeVersion::parse("1.0").unwrap()));
        assert!(!range.matches(&ForgeVersion::parse("1.1").unwrap()));
        assert!(range.matches(&ForgeVersion::parse("1.2").unwrap()));

        // x is between 1.0 and 1.1 inclusive
        let range = ForgeVersionRange::parse("[1.0, 1.1]").unwrap();
        assert!(range.matches(&ForgeVersion::parse("1.0").unwrap()));
        assert!(range.matches(&ForgeVersion::parse("1.1").unwrap()));
        assert!(!range.matches(&ForgeVersion::parse("1.2").unwrap()));

        // x is between 1.0 and 1.2 exclusive
        let range = ForgeVersionRange::parse("(1.0, 1.2)").unwrap();
        assert!(!range.matches(&ForgeVersion::parse("1.0").unwrap()));
        assert!(range.matches(&ForgeVersion::parse("1.1").unwrap()));
        assert!(!range.matches(&ForgeVersion::parse("1.2").unwrap()));
    }

    #[test]
    fn parse_version_range() {
        assert_eq!(
            ForgeVersionRange::parse("[1.0]").unwrap(),
            ForgeVersionRange(vec![ForgeVersionRangeIn::Equal(
                ForgeVersion::parse("1.0").unwrap()
            )])
        );
        assert_eq!(
            ForgeVersionRange::parse("1.0").unwrap(),
            ForgeVersionRange(vec![ForgeVersionRangeIn::Range(
                Bound::Inclusive(Some(ForgeVersion::parse("1.0").unwrap())),
                Bound::Exclusive(None),
            )])
        );
        assert_eq!(
            ForgeVersionRange::parse("[1.0,)").unwrap(),
            ForgeVersionRange(vec![ForgeVersionRangeIn::Range(
                Bound::Inclusive(Some(ForgeVersion::parse("1.0").unwrap())),
                Bound::Exclusive(None),
            )])
        );
        assert_eq!(
            ForgeVersionRange::parse("(,1.0]").unwrap(),
            ForgeVersionRange(vec![ForgeVersionRangeIn::Range(
                Bound::Exclusive(None),
                Bound::Inclusive(Some(ForgeVersion::parse("1.0").unwrap())),
            )])
        );
        assert_eq!(
            ForgeVersionRange::parse(" ( , 1.0] , [ 1.2 , ) ").unwrap(),
            ForgeVersionRange(vec![
                ForgeVersionRangeIn::Range(
                    Bound::Exclusive(None),
                    Bound::Inclusive(Some(ForgeVersion::parse("1.0").unwrap())),
                ),
                ForgeVersionRangeIn::Range(
                    Bound::Inclusive(Some(ForgeVersion::parse("1.2").unwrap())),
                    Bound::Exclusive(None),
                )
            ])
        );
        assert_eq!(
            ForgeVersionRange::parse("  ").unwrap(),
            ForgeVersionRange(vec![])
        );
        assert!(matches!(
            ForgeVersionRange::parse(" [ ] ").unwrap_err(),
            Error::EmptyList
        ));
        assert!(matches!(
            ForgeVersionRange::parse("[1.0)").unwrap_err(),
            Error::BadParensOneItem
        ));
    }

    #[test]
    fn compare_comparable_version() {
        cmp_str("1.0", "1.0-alpha", cmp::Ordering::Greater);
        cmp_str("1", "1.0", cmp::Ordering::Equal);
        cmp_str("1-beta", "1-xyz", cmp::Ordering::Less);
        cmp_str("1-beta", "1-abc", cmp::Ordering::Less);
        cmp_str("1.0", "1.0-abc", cmp::Ordering::Less);
        cmp_str("1.0-alpha-10", "1.0-alpha-2", cmp::Ordering::Greater);
        cmp_str("1.0-alpha-1.0", "1.0-alpha-1", cmp::Ordering::Equal);
        cmp_str("1.0-alpha-1.2", "1.0-alpha-2", cmp::Ordering::Less);
    }

    #[test]
    fn parse_comparable_version() {
        let items = parse_str("1.0");
        assert_eq!(items, vec![Item::Integer(1), Item::Integer(0)]);
        let items = parse_str("1.0.1");
        assert_eq!(
            items,
            vec![Item::Integer(1), Item::Integer(0), Item::Integer(1)]
        );
        let items = parse_str("1-SNAPSHOT");
        assert_eq!(
            items,
            vec![
                Item::Integer(1),
                Item::Dash,
                Item::String("SNAPSHOT".into())
            ]
        );
        let items = parse_str("1-alpha10-SNAPSHOT");
        assert_eq!(
            items,
            vec![
                Item::Integer(1),
                Item::Dash,
                Item::String("alpha".into()),
                Item::Integer(10),
                Item::Dash,
                Item::String("SNAPSHOT".into()),
            ]
        );
    }

    fn parse_str(s: &str) -> Vec<Item> {
        let mut parser = Parser::new();
        for ch in s.chars() {
            parser.eat(ch).unwrap();
        }
        parser.consume().unwrap()
    }

    fn cmp_str(a: &str, b: &str, ord: cmp::Ordering) {
        let ord_a = ForgeVersion(parse_str(a)).cmp(&ForgeVersion(parse_str(b)));
        let ord_b = ForgeVersion(parse_str(b)).cmp(&ForgeVersion(parse_str(a)));
        assert_eq!(ord_a, ord_b.reverse());
        assert_eq!(ord_a, ord);
    }
}
