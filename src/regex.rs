use std::fmt::Debug;
use std::ops::Index;
use std::sync::{Arc};
use super::string::{SvStringMap, SvString, AnySvStringType};
use super::character_validation::{is_whitespace, is_decimal_digit, hex_digit_mv};
use enumflags2::{bitflags, BitFlags};

enum Expression {
    Empty,
    SingleCharacter(char),
    Text(SvString),
    /// Multiple expressions.
    Sequence(Vec<Arc<Expression>>),
    /// `\k<Name>`
    BackReference(SvString),
    /// `x|y`
    Disjunction(Arc<Expression>, Arc<Expression>),
    /// `^`
    StartAssertion,
    /// `$`
    EndAssertion,
    /// `\b`
    WordBoundaryAssertion,
    /// `\B`
    NonWordBoundaryAssertion,
    /// `[]`
    CharacterClass(Vec<CharacterClassItem>),
    /// `[^]`
    NegatedCharacterClass(Vec<CharacterClassItem>),
    /// `(?=x)`
    Lookahead(Arc<Expression>),
    /// `(?!x)`
    LookaheadNot(Arc<Expression>),
    /// `(?<=x)`
    Lookbehind(Arc<Expression>),
    /// `(?<!x)`
    LookbehindNot(Arc<Expression>),
    /// `.`
    AnyCharacter,
    /// `\d`
    AnyDigit,
    /// `\D`
    AnyNonDigit,
    /// `\w`
    AnyBasicLatinAlphanumeric,
    /// `\W`
    AnyNonBasicLatinAlphanumeric,
    /// `\s`
    AnyWhitespace,
    /// `\S`
    AnyNonWhitespace,
    /// `x*`
    ZeroOrMore {
        expression: Arc<Expression>,
        greedy: bool,
    },
    /// `x+`
    OneOrMore {
        expression: Arc<Expression>,
        greedy: bool,
    },
    /// `x?`
    ZeroOrOne {
        expression: Arc<Expression>,
        greedy: bool,
    },
    /// `x{n}`
    NTimes {
        expression: Arc<Expression>,
        greedy: bool,
        num: i32,
    },
    /// `x{n,}`
    AtLeastNTimes {
        expression: Arc<Expression>,
        greedy: bool,
        num: i32,
    },
    /// `x{n,m}`
    FromNToMTimes {
        expression: Arc<Expression>,
        greedy: bool,
        from: i32,
        to: i32,
    },
    /// `(x)`
    CaptureGroup(Arc<Expression>),
    /// `(?:x)`
    NonCaptureGroup(Arc<Expression>),
    /// `(?<Name>x)`
    NamedCaptureGroup{
        name: SvString,
        expression: Arc<Expression>,
    },
}

enum CharacterClassItem {
    SingleCharacter(char),
    /// From-to range (inclusive).
    FromTo(char, char),
    /// `\d`
    AnyDigit,
    /// `\D`
    AnyNonDigit,
    /// `\w`
    AnyBasicLatinAlphanumeric,
    /// `\W`
    AnyNonBasicLatinAlphanumeric,
    /// `\s`
    AnyWhitespace,
    /// `\S`
    AnyNonWhitespace,
}

struct Parser {
    ignores_whitespace: bool,
    source: SvString,
    pos: i32,
    len: i32,
}

#[derive(Clone, Debug)]
pub enum RegexSyntaxError {
    UnexpectedEndOfPattern,
    UnexpectedCharacter { index: i32, },
    UnknownEscapeCharacter { character: SvString, index: i32, },
    InvalidCharacter { index: i32, },
    RangeBetweenInvalidAtoms { index: i32, },
    InvalidFlags,
}

impl Parser {
    fn new(source: SvString, ignores_whitespace: bool) -> Parser {
        Parser {
            source: source.clone(),
            pos: 0,
            len: source.len(),
            ignores_whitespace,
        }
    }

    fn reached_end(&self) -> bool {
        self.pos >= self.len
    }

    fn construct_unexpected_error(&self) -> RegexSyntaxError {
        if self.reached_end() {
            return RegexSyntaxError::UnexpectedEndOfPattern;
        }
        RegexSyntaxError::UnexpectedCharacter { index: self.pos }
    }

    fn lookahead(&self, i: i32) -> char {
        self.source.char_at(self.pos + i)
    }

    fn skip_whitespace(&mut self) {
        if !self.ignores_whitespace {
            return;
        }
        while is_whitespace(self.lookahead(0)) {
            self.pos += 1;
        }
    }

    fn consume(&mut self, ch: char) -> bool {
        if self.lookahead(0) == ch {
            self.pos += 1;
            return true;
        }
        false
    }

    fn parse_decimal_digits(&mut self) -> Result<i32, RegexSyntaxError> {
        let i = self.pos;
        if !is_decimal_digit(self.lookahead(0)) {
            return Err(self.construct_unexpected_error());
        }
        self.pos += 1;
        while is_decimal_digit(self.lookahead(0)) {
            self.pos += 1;
        }
        Ok(i32::from_str_radix(self.source.substr(i..self.pos).to_standard_string().as_ref(), 10).unwrap_or(0))
    }

    fn parse_hex_digit(&mut self) -> Result<i32, RegexSyntaxError> {
        let v = hex_digit_mv(self.lookahead(0));
        if v.is_none() {
            return Err(self.construct_unexpected_error());
        }
        self.pos += 1;
        Ok(v.unwrap())
    }

    fn parse_pattern(&mut self) -> Result<Arc<Expression>, RegexSyntaxError> {
        let r = self.parse_disjunction()?;
        self.skip_whitespace();
        if !self.reached_end() {
            return Err(self.construct_unexpected_error());
        }
        Ok(r)
    }

    fn parse_disjunction(&mut self) -> Result<Arc<Expression>, RegexSyntaxError> {
        if self.reached_end() {
            return Ok(Arc::new(Expression::Empty));
        }
        let mut r = self.parse_term_or_seq()?;
        self.skip_whitespace();
        while self.lookahead(0) == '|' {
            self.pos += 1;
            let r2 = r.clone();
            r = Arc::new(Expression::Disjunction(r, self.parse_term_or_seq()?));
            self.skip_whitespace();
        }
        Ok(r)
    }

    fn parse_term_or_seq(&mut self) -> Result<Arc<Expression>, RegexSyntaxError> {
        let r = self.parse_term()?;
        // parse sequence
        self.skip_whitespace();
        if self.lookahead(0) != '|' && self.lookahead(0) != ')' {
            let mut seq: Vec<Arc<Expression>> = vec![r];
            while !self.reached_end() && self.lookahead(0) != '|' && self.lookahead(0) != ')' {
                seq.push(self.parse_term()?);
                self.skip_whitespace();
            }
            return Ok(Arc::new(Expression::Sequence(seq)));
        }
        Ok(r)
    }

    fn parse_term(&mut self) -> Result<Arc<Expression>, RegexSyntaxError> {
        if self.reached_end() {
            return Ok(Arc::new(Expression::Empty));
        }
        self.skip_whitespace();
        let mut ch = self.lookahead(0);
        let mut r: Option<Arc<Expression>> = None;
        if ch == '^' {
            r = Some(Arc::new(Expression::StartAssertion));
            self.pos += 1;
        } else if ch == '$' {
            r = Some(Arc::new(Expression::EndAssertion));
            self.pos += 1;
        } else if ch == '\\' && self.lookahead(1) == 'b' {
            r = Some(Arc::new(Expression::WordBoundaryAssertion));
            self.pos += 2;
        } else if ch == '\\' && self.lookahead(1) == 'B' {
            r = Some(Arc::new(Expression::NonWordBoundaryAssertion));
            self.pos += 2;
        } else {
            // atom
            r = Some(self.parse_atom()?);

            // parse atom quantifier
            self.skip_whitespace();
            ch = self.lookahead(0);
            if ch == '*' {
                self.pos += 1;
                r = Some(Arc::new(Expression::ZeroOrMore {
                    expression: r.unwrap(), greedy: !self.consume('?'),
                }));
            } else if ch == '+' {
                self.pos += 1;
                r = Some(Arc::new(Expression::OneOrMore {
                    expression: r.unwrap(), greedy: !self.consume('?'),
                }));
            } else if ch == '?' {
                self.pos += 1;
                r = Some(Arc::new(Expression::ZeroOrOne {
                    expression: r.unwrap(), greedy: !self.consume('?'),
                }));
            } else if ch == '{' {
                self.pos += 1;
                let n = self.parse_decimal_digits()?;
                if self.lookahead(0) == ',' && self.lookahead(1) == '}' {
                    self.pos += 2;
                    r = Some(Arc::new(Expression::AtLeastNTimes {
                        expression: r.unwrap(),
                        greedy: !self.consume('?'),
                        num: n,
                    }));
                } else if self.consume(',') {
                    let m = self.parse_decimal_digits()?;
                    if !self.consume('}') {
                        return Err(self.construct_unexpected_error());
                    }
                    r = Some(Arc::new(Expression::FromNToMTimes {
                        expression: r.unwrap(),
                        greedy: !self.consume('?'),
                        from: n,
                        to: m,
                    }));
                } else {
                    if !self.consume('}') {
                        return Err(self.construct_unexpected_error());
                    }
                    r = Some(Arc::new(Expression::NTimes {
                        expression: r.unwrap(),
                        greedy: !self.consume('?'),
                        num: n,
                    }));
                }
            }
        }
        Ok(r.unwrap())
    }

    fn parse_atom(&mut self) -> Result<Arc<Expression>, RegexSyntaxError> {
        let mut ch = self.lookahead(0);

        if ch == '.' {
            self.pos += 1;
            Ok(Arc::new(Expression::AnyCharacter))
        } else if ch == '\\' {
            let mut ch2 = self.lookahead(1);
            if ch2 == '0' {
                self.pos += 2;
                Ok(Arc::new(Expression::SingleCharacter('\x00')))
            } else if ch2 == 'f' {
                self.pos += 2;
                Ok(Arc::new(Expression::SingleCharacter('\x0C')))
            } else if ch2 == 'n' {
                self.pos += 2;
                Ok(Arc::new(Expression::SingleCharacter('\x0A')))
            } else if ch2 == 'r' {
                self.pos += 2;
                Ok(Arc::new(Expression::SingleCharacter('\x0D')))
            } else if ch2 == 't' {
                self.pos += 2;
                Ok(Arc::new(Expression::SingleCharacter('\x09')))
            } else if ch2 == 'v' {
                self.pos += 2;
                Ok(Arc::new(Expression::SingleCharacter('\x0B')))
            } else if ch2 == 'x' {
                self.pos += 2;
                Ok(Arc::new(Expression::SingleCharacter(
                    char::from_u32((
                        (self.parse_hex_digit()?) << 4 |
                        self.parse_hex_digit()?
                    ) as u32).unwrap_or('\x00')
                )))
            } else if ch2 == 'u' && self.lookahead(2) == '{' {
                let start = self.pos;
                self.pos += 3;
                let mut v: i32 = self.parse_hex_digit()?;
                loop {
                    let v2 = hex_digit_mv(self.lookahead(0));
                    if v2.is_none() {
                        break;
                    }
                    self.pos += 1;
                    v = (v << 4) | v2.unwrap();
                }
                if !self.consume('}') {
                    return Err(self.construct_unexpected_error());
                }
                let v = char::from_u32(v as u32);
                if v.is_none() {
                    return Err(RegexSyntaxError::InvalidCharacter {
                        index: start,
                    });
                }
                Ok(Arc::new(Expression::SingleCharacter(v.unwrap())))
            } else if ch2 == 'u' {
                self.pos += 2;
                let v = char::from_u32((
                    (self.parse_hex_digit()?) << 12 |
                    (self.parse_hex_digit()?) << 8 |
                    (self.parse_hex_digit()?) << 4 |
                    self.parse_hex_digit()?
                ) as u32);
                if v.is_none() {
                    return Err(RegexSyntaxError::InvalidCharacter {
                        index: self.pos - 6,
                    });
                }
                Ok(Arc::new(Expression::SingleCharacter(v.unwrap())))
            } else if is_meta_character(ch2) {
                self.pos += 2;
                Ok(Arc::new(Expression::SingleCharacter(ch2)))
            } else if ch2 == 'd' {
                self.pos += 2;
                Ok(Arc::new(Expression::AnyDigit))
            } else if ch2 == 'D' {
                self.pos += 2;
                Ok(Arc::new(Expression::AnyNonDigit))
            } else if ch2 == 's' {
                self.pos += 2;
                Ok(Arc::new(Expression::AnyWhitespace))
            } else if ch2 == 'S' {
                self.pos += 2;
                Ok(Arc::new(Expression::AnyNonWhitespace))
            } else if ch2 == 'w' {
                self.pos += 2;
                Ok(Arc::new(Expression::AnyBasicLatinAlphanumeric))
            } else if ch2 == 'W' {
                self.pos += 2;
                Ok(Arc::new(Expression::AnyNonBasicLatinAlphanumeric))
            } else if ch2 == 'k' {
                self.pos += 2;
                if !self.consume('<') {
                    return Err(self.construct_unexpected_error());
                }
                let mut s: Vec<char> = vec![];
                while self.lookahead(0) != '>' {
                    if self.reached_end() {
                        return Err(self.construct_unexpected_error());
                    }
                    s.push(self.lookahead(0));
                    self.pos += 1;
                }
                if !self.consume('>') {
                    return Err(self.construct_unexpected_error());
                }
                Ok(Arc::new(Expression::BackReference(SvString::from(s))))
            } else {
                self.pos += 1;
                if self.reached_end() {
                    return Err(self.construct_unexpected_error());
                }
                Err(RegexSyntaxError::UnknownEscapeCharacter {
                    index: self.pos,
                    character: SvString::from(ch2),
                })
            }
        } else if ch == '(' {
            let mut ch2 = self.lookahead(1);
            if ch2 == '?' && self.lookahead(2) == ':' {
                self.pos += 3;
                let d = self.parse_disjunction()?;
                if !self.consume(')') {
                    return Err(self.construct_unexpected_error());
                }
                Ok(Arc::new(Expression::NonCaptureGroup(d)))
            } else if ch2 == '?' && self.lookahead(2) == '<' && self.lookahead(3) == '=' {
                self.pos += 4;
                let d = self.parse_disjunction()?;
                if !self.consume(')') {
                    return Err(self.construct_unexpected_error());
                }
                Ok(Arc::new(Expression::Lookbehind(d)))
            } else if ch2 == '?' && self.lookahead(2) == '<' && self.lookahead(3) == '!' {
                self.pos += 4;
                let d = self.parse_disjunction()?;
                if !self.consume(')') {
                    return Err(self.construct_unexpected_error());
                }
                Ok(Arc::new(Expression::LookbehindNot(d)))
            } else if ch2 == '?' && self.lookahead(2) == '<' {
                self.pos += 3;
                let mut name: Vec<char> = vec![];
                loop {
                    let ch = self.lookahead(0);
                    if ch == '>' {
                        self.pos += 1;
                        break;
                    }
                    if self.reached_end() {
                        return Err(self.construct_unexpected_error());
                    }
                    name.push(ch);
                    self.pos += 1;
                }
                let name = SvString::from(name);
                let d = self.parse_disjunction()?;
                if !self.consume(')') {
                    return Err(self.construct_unexpected_error());
                }
                Ok(Arc::new(Expression::NamedCaptureGroup {name, expression: d}))
            } else if ch2 == '?' && self.lookahead(2) == '=' {
                self.pos += 3;
                let d = self.parse_disjunction()?;
                if !self.consume(')') {
                    return Err(self.construct_unexpected_error());
                }
                Ok(Arc::new(Expression::Lookahead(d)))
            } else if ch2 == '?' && self.lookahead(2) == '!' {
                self.pos += 3;
                let d = self.parse_disjunction()?;
                if !self.consume(')') {
                    return Err(self.construct_unexpected_error());
                }
                Ok(Arc::new(Expression::LookaheadNot(d)))
            } else if ch2 == '?' {
                self.pos += 1;
                Err(self.construct_unexpected_error())
            } else {
                self.pos += 1;
                let d = self.parse_disjunction()?;
                if !self.consume(')') {
                    return Err(self.construct_unexpected_error());
                }
                Ok(Arc::new(Expression::CaptureGroup(d)))
            }
        } else if ch == '[' {
            self.parse_character_class()
        } else if ch == ')' {
            Ok(Arc::new(Expression::Empty))
        } else if ch == '^' || ch == '$' || ch == '.' || ch == '*' || ch == '+' || ch == '?'
            || ch == ']' || ch == '{' || ch == '}'
        {
            return Err(self.construct_unexpected_error());
        } else {
            self.pos += 1;
            Ok(Arc::new(Expression::SingleCharacter(ch)))
        }
    }

    fn parse_character_class(&mut self) -> Result<Arc<Expression>, RegexSyntaxError> {
        self.pos += 1;
        let mut items: Vec<CharacterClassItem> = vec![];
        self.skip_whitespace();
        let neg = self.consume('^');
        self.skip_whitespace();
        loop {
            let ch = self.lookahead(0);
            if ch == ']' {
                self.pos += 1;
                break;
            }
            if self.reached_end() {
                return Err(self.construct_unexpected_error());
            }
            let range_start = self.pos;
            let mut atom = self.parse_class_atom()?;
            if self.lookahead(0) == '-' && self.lookahead(1) != ']' {
                self.pos += 1;
                let r_atom = self.parse_class_atom()?;
                let mut range: Option<CharacterClassItem> = None;
                if let CharacterClassItem::SingleCharacter(from) = atom {
                    if let CharacterClassItem::SingleCharacter(to) = r_atom {
                        range = Some(CharacterClassItem::FromTo(from, to));
                    }
                }
                if range.is_none() {
                    return Err(RegexSyntaxError::RangeBetweenInvalidAtoms { index: range_start });
                }
                atom = range.unwrap();
            }
            items.push(atom);
            self.skip_whitespace();
        }
        Ok(Arc::new(if neg {
            Expression::NegatedCharacterClass(items)
        } else {
            Expression::CharacterClass(items)
        }))
    }

    fn parse_class_atom(&mut self) -> Result<CharacterClassItem, RegexSyntaxError> {
        let ch = self.lookahead(0);
        if ch == '\\' {
            let ch2 = self.lookahead(1);
            if ch2 == '0' {
                self.pos += 2;
                Ok(CharacterClassItem::SingleCharacter('\x00'))
            } else if ch2 == ']' {
                self.pos += 2;
                Ok(CharacterClassItem::SingleCharacter(']'))
            } else if ch2 == '-' {
                self.pos += 2;
                Ok(CharacterClassItem::SingleCharacter('-'))
            } else if ch2 == 'f' {
                self.pos += 2;
                Ok(CharacterClassItem::SingleCharacter('\x0C'))
            } else if ch2 == 'n' {
                self.pos += 2;
                Ok(CharacterClassItem::SingleCharacter('\x0A'))
            } else if ch2 == 'r' {
                self.pos += 2;
                Ok(CharacterClassItem::SingleCharacter('\x0D'))
            } else if ch2 == 't' {
                self.pos += 2;
                Ok(CharacterClassItem::SingleCharacter('\x09'))
            } else if ch2 == 'v' {
                self.pos += 2;
                Ok(CharacterClassItem::SingleCharacter('\x0B'))
            } else if ch2 == 'x' {
                self.pos += 2;
                Ok(CharacterClassItem::SingleCharacter(
                    char::from_u32((
                        (self.parse_hex_digit()?) << 4 |
                        self.parse_hex_digit()?
                    ) as u32).unwrap_or('\x00')
                ))
            } else if ch2 == 'u' && self.lookahead(2) == '{' {
                let start = self.pos;
                self.pos += 3;
                let mut v: i32 = self.parse_hex_digit()?;
                loop {
                    let v2 = hex_digit_mv(self.lookahead(0));
                    if v2.is_none() {
                        break;
                    }
                    self.pos += 1;
                    v = (v << 4) | v2.unwrap();
                }
                if !self.consume('}') {
                    return Err(self.construct_unexpected_error());
                }
                let v = char::from_u32(v as u32);
                if v.is_none() {
                    return Err(RegexSyntaxError::InvalidCharacter {
                        index: start,
                    });
                }
                Ok(CharacterClassItem::SingleCharacter(v.unwrap()))
            } else if ch2 == 'u' {
                self.pos += 2;
                let v = char::from_u32((
                    (self.parse_hex_digit()?) << 12 |
                    (self.parse_hex_digit()?) << 8 |
                    (self.parse_hex_digit()?) << 4 |
                    self.parse_hex_digit()?
                ) as u32);
                if v.is_none() {
                    return Err(RegexSyntaxError::InvalidCharacter {
                        index: self.pos - 6,
                    });
                }
                Ok(CharacterClassItem::SingleCharacter(v.unwrap()))
            } else if is_meta_character(ch2) {
                self.pos += 2;
                Ok(CharacterClassItem::SingleCharacter('\\'))
            } else if ch2 == 'd' {
                self.pos += 2;
                Ok(CharacterClassItem::AnyDigit)
            } else if ch2 == 'D' {
                self.pos += 2;
                Ok(CharacterClassItem::AnyNonDigit)
            } else if ch2 == 's' {
                self.pos += 2;
                Ok(CharacterClassItem::AnyWhitespace)
            } else if ch2 == 'S' {
                self.pos += 2;
                Ok(CharacterClassItem::AnyNonWhitespace)
            } else if ch2 == 'w' {
                self.pos += 2;
                Ok(CharacterClassItem::AnyBasicLatinAlphanumeric)
            } else if ch2 == 'W' {
                self.pos += 2;
                Ok(CharacterClassItem::AnyNonBasicLatinAlphanumeric)
            } else {
                self.pos += 1;
                if self.reached_end() {
                    return Err(self.construct_unexpected_error());
                }
                Err(RegexSyntaxError::UnknownEscapeCharacter {
                    index: self.pos,
                    character: SvString::from(ch2),
                })
            }
        } else {
            Ok(CharacterClassItem::SingleCharacter(ch))
        }
    }
}

/// Regular expression. `Regex` is similiar to EcmaScript built-in object `RegExp`.
#[derive(Clone)]
pub struct Regex {
    m_source: SvString,
    m_flags: BitFlags<RegexFlags>,
    m_compiled: Arc<Expression>,
}

impl Debug for Regex {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.source())
    }
}

impl Regex {
    /// Constructs a new `Regex` object.
    pub fn new(source: impl AnySvStringType) -> Regex {
        Regex::try_new(source).unwrap()
    }

    /// Constructs a new `Regex` object.
    pub fn try_new(source: impl AnySvStringType) -> Result<Regex, RegexSyntaxError> {
        let source = source.convert();
        let c = Parser::new(source.clone(), false).parse_pattern()?;
        Ok(Regex {
            m_source: source.clone(),
            m_flags: BitFlags::empty(),
            m_compiled: c,
        })
    }

    /// Constructs a new `Regex` object with given flags.
    pub fn with_flags(source: impl AnySvStringType, flags: impl AnySvStringType) -> Regex {
        Regex::try_with_flags(source, flags).unwrap()
    }

    /// Constructs a new `Regex` object with given flags.
    pub fn try_with_flags(source: impl AnySvStringType, flags: impl AnySvStringType) -> Result<Regex, RegexSyntaxError> {
        let source = source.convert();
        let mut flags_obj = BitFlags::empty();
        for ch in flags.convert().chars() {
            if ch == 'g' {
                if flags_obj.contains(RegexFlags::Global) {
                    return Err(RegexSyntaxError::InvalidFlags);
                }
                flags_obj |= RegexFlags::Global;
            } else if ch == 'i' {
                if flags_obj.contains(RegexFlags::IgnoreCase) {
                    return Err(RegexSyntaxError::InvalidFlags);
                }
                flags_obj |= RegexFlags::IgnoreCase;
            } else if ch == 'x' {
                if flags_obj.contains(RegexFlags::IgnoreWhitespace) {
                    return Err(RegexSyntaxError::InvalidFlags);
                }
                flags_obj |= RegexFlags::IgnoreWhitespace;
            } else if ch == 'm' {
                if flags_obj.contains(RegexFlags::Multiline) {
                    return Err(RegexSyntaxError::InvalidFlags);
                }
                flags_obj |= RegexFlags::Multiline;
            } else {
                return Err(RegexSyntaxError::InvalidFlags);
            }
        }
        let c = Parser::new(source.clone(), flags_obj.contains(RegexFlags::IgnoreWhitespace)).parse_pattern()?;
        Ok(Regex {
            m_source: source.clone(),
            m_flags: flags_obj,
            m_compiled: c,
        })
    }

    /// Escapes meta characters from given string.
    pub fn escape(str: impl AnySvStringType) -> SvString {
        let mut r: Vec<char> = vec![];
        for ch in str.convert().chars() {
            if is_meta_character(ch) {
                r.push('\\');
            }
            r.push(ch);
        }
        SvString::from(r)
    }

    /// SvString containing the source text of the regular expression.
    pub fn source(&self) -> SvString {
        self.m_source.clone()
    }

    /// Represents the flags of the current regular expression object.
    pub fn flags(&self) -> SvString {
        let mut r: Vec<char> = vec![];
        if self.global() {
            r.push('g');
        }
        if self.ignore_case() {
            r.push('i');
        }
        if self.ignore_whitespace() {
            r.push('x');
        }
        if self.multiline() {
            r.push('m');
        }
        SvString::from(r)
    }

    /// Indicates whether or not the `g` flag is used with the
    /// regular expression.
    pub fn global(&self) -> bool {
        self.m_flags.contains(RegexFlags::Global)
    }

    /// Indicates whether or not the `i` flag is used with the
    /// regular expression.
    pub fn ignore_case(&self) -> bool {
        self.m_flags.contains(RegexFlags::IgnoreCase)
    }

    /// Indicates whether or not the `x` flag is used with the
    /// regular expression.
    pub fn ignore_whitespace(&self) -> bool {
        self.m_flags.contains(RegexFlags::IgnoreWhitespace)
    }

    /// Indicates whether or not the `m` flag is used with the
    /// regular expression.
    pub fn multiline(&self) -> bool {
        self.m_flags.contains(RegexFlags::Multiline)
    }

    /// Tests whether or not the regular expression matches the
    /// given string.
    pub fn test(&self, str: impl AnySvStringType) -> bool {
        self.exec(str).is_some()
    }

    pub fn exec(&self, str: impl AnySvStringType) -> Option<RegexMatch> {
        let str = str.convert();
    }
}

#[derive(Clone)]
struct State {
    end_index: i32,
    captures: Vec<SvString>,
    captures_by_name: SvStringMap<SvString>,
}

struct Matcher {
    input: SvString,
    input_len: i32,
    ignore_case: bool,
    multiline: bool,
}

impl Matcher {
}

/// Represents the result of executing a regular expression against
/// a string.
#[derive(Clone, Debug)]
pub struct RegexMatch {
    m_index: i32,
    m_captures: SvStringMap<SvString>,
    m_captures_list: Vec<SvString>,
}

impl RegexMatch {
    /// The index relative to the full capture.
    pub fn index(&self) -> i32 {
        self.m_index
    }

    /// The full matched substring.
    pub fn full_capture(&self) -> SvString {
        self.m_captures_list[0].clone()
    }

    pub fn get_capture(&self, name: impl AnySvStringType) -> SvString {
        self.m_captures.get(&name.convert()).unwrap_or(&SvString::from("")).clone()
    }

    pub fn has_capture(&self, name: impl AnySvStringType) -> bool {
        self.m_captures.contains_key(&name.convert())
    }

    pub fn get_capture_by_index(&self, index: i32) -> SvString {
        if index < 0 && (index as usize) < self.m_captures_list.len() { self.m_captures_list[index as usize].clone() } else { SvString::from("") }
    }

    pub fn has_capture_by_index(&self, index: i32) -> bool {
        index < 0 && (index as usize) < self.m_captures_list.len()
    }
}

impl Index<i32> for RegexMatch {
    type Output = SvString;
    fn index(&self, index: i32) -> &Self::Output {
        if index < 0 && (index as usize) < self.m_captures_list.len() {
            &self.m_captures_list[index as usize]
        } else {
            panic!("Index out of bounds in RegexMatch.");
        }
    }
}

/// Allows functions to accept either a regular expression or string
/// as argument.
pub trait RegexOrString {
    fn convert(&self) -> Regex;
}

impl RegexOrString for Regex {
    fn convert(&self) -> Regex {
        self.clone()
    }
}

impl RegexOrString for SvString {
    fn convert(&self) -> Regex {
        Regex {
            m_source: SvString::from(""),
            m_compiled: Arc::new(Expression::Text(self.clone())),
            m_flags: BitFlags::empty(),
        }
    }
}

impl RegexOrString for &str {
    fn convert(&self) -> Regex {
        Regex {
            m_source: SvString::from(""),
            m_compiled: Arc::new(Expression::Text(SvString::from(*self))),
            m_flags: BitFlags::empty(),
        }
    }
}

impl RegexOrString for std::string::String {
    fn convert(&self) -> Regex {
        Regex {
            m_source: SvString::from(""),
            m_compiled: Arc::new(Expression::Text(SvString::from(self.as_ref()))),
            m_flags: BitFlags::empty(),
        }
    }
}

#[bitflags]
#[repr(u8)]
#[derive(Copy, Clone, Debug, PartialEq)]
enum RegexFlags {
    Global = 0b1,
    IgnoreCase = 0b10,
    IgnoreWhitespace = 0b100,
    Multiline = 0b1000,
}

fn is_meta_character(ch: char) -> bool {
    ch == '\\' || ch == '[' || ch == ']' || ch == '(' || ch == ')' ||
    ch == '{' || ch == '}' || ch == '^' || ch == '$' || ch == '.' ||
    ch == '*' || ch == '+' || ch == '?' || ch == '|'
}