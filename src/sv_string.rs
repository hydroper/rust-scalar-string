use std::{
    cmp::Ordering,
    collections::{HashMap},
    sync::{Arc, Weak, Mutex},
    ops::{
        Range,
        RangeFull,
        RangeInclusive,
        RangeFrom,
        RangeTo,
        RangeToInclusive, Add,
    }, fmt::{Debug, Display},
};
use once_cell::sync::Lazy;
use super::character_validation::is_whitespace;

pub type SvStringMap<V> = std::collections::BTreeMap<SvString, V>;
pub type SvStringSet = std::collections::BTreeSet<SvString>;

static INTERN_LIMIT: i32 = 512;

static INTERNED: Lazy<Mutex<HashMap<i32, Vec<Weak<StringRepr0>>>>> = Lazy::new(|| {
    Mutex::new(HashMap::new())
});

fn intern(s: Arc<StringRepr0>) -> Arc<StringRepr0> {
    if s.m_len > INTERN_LIMIT {
        return s.clone();
    }
    let p1 = &mut *INTERNED.lock().unwrap();
    let mut p2 = p1.get_mut(&s.m_len);
    let v = vec![];
    if p2.is_none() {
        p1.insert(s.m_len, v);
        p2 = p1.get_mut(&s.m_len);
    }
    let p2 = p2.unwrap();
    for s2 in p2.iter() {
        let s2 = s2.upgrade().unwrap();
        if s == s2 {
            return s2.clone();
        }
    }
    p2.push(Arc::downgrade(&s));
    s.clone()
}

/// SvString data type. This string type consists of Unicode Scalar Value.
/// The `len()` method returns the number of characters in
/// Unicode Scalar Value.
#[derive(Clone, Ord)]
pub struct SvString {
    m_repr: Arc<StringRepr0>,
}

impl Eq for SvString {
}

#[derive(Clone)]
struct StringRepr0 {
    m_len: i32,
    m_repr: StringRepr1,
}

impl Ord for StringRepr0 {
    fn cmp(&self, other: &Self) -> Ordering {
        self.partial_cmp(other).unwrap_or(Ordering::Equal)
    }
}

impl Eq for StringRepr0 {
}

impl Drop for StringRepr0 {
    fn drop(&mut self) {
        if self.m_len > INTERN_LIMIT {
            return;
        }
        let p1 = &mut *INTERNED.lock().unwrap();
        let p2 = p1.get_mut(&self.m_len);
        if p2.is_none() {
            return;
        }
        let p2 = p2.unwrap();
        let mut i: usize = 0;
        let mut f: Vec<usize> = vec![];
        for s2 in p2.iter() {
            if s2.upgrade().is_none() {
                f.push(i);
            }
            i += 1;
        }
        for i in f {
            p2.remove(i);
        }
        if p2.len() == 0 {
            p1.remove(&self.m_len);
        }
    }
}

#[derive(Clone)]
enum StringRepr1 {
    Reference(Arc<StringRepr2>),
    Slice(Slice),
}

impl StringRepr1 {
    fn char_at(&self, index: usize) -> char {
        match self {
            StringRepr1::Reference(r) => r.char_at(index),
            StringRepr1::Slice(s) => s.container.char_at(s.start + index),
        }
    }
}

#[derive(Clone)]
struct Slice {
    container: Arc<StringRepr2>,
    start: usize,
    end: usize,
}

impl PartialEq for Slice {
    fn eq(&self, other: &Self) -> bool {
        if (self.end - self.start) != (other.end - other.start) {
            return false;
        }
        let mut l_i = self.start;
        let mut r_i = other.start;
        match self.container.as_ref() {
            StringRepr2::Latin1(l) => {
                match other.container.as_ref() {
                    StringRepr2::Latin1(r) => {
                        while l_i < self.end {
                            if l[l_i] != r[r_i] {
                                return false;
                            }
                            l_i += 1;
                            r_i += 1;
                        }
                        true
                    },
                    StringRepr2::Ucs2(r) => {
                        while l_i < self.end {
                            if (l[l_i] as u16) != r[r_i] {
                                return false;
                            }
                            l_i += 1;
                            r_i += 1;
                        }
                        true
                    },
                    StringRepr2::Ucs4(r) => {
                        while l_i < self.end {
                            if (l[l_i] as u32) != r[r_i] {
                                return false;
                            }
                            l_i += 1;
                            r_i += 1;
                        }
                        true
                    },
                }
            },
            StringRepr2::Ucs2(l) => {
                match other.container.as_ref() {
                    StringRepr2::Latin1(r) => {
                        while l_i < self.end {
                            if l[l_i] != (r[r_i] as u16) {
                                return false;
                            }
                            l_i += 1;
                            r_i += 1;
                        }
                        true
                    },
                    StringRepr2::Ucs2(r) => {
                        while l_i < self.end {
                            if l[l_i] != r[r_i] {
                                return false;
                            }
                            l_i += 1;
                            r_i += 1;
                        }
                        true
                    },
                    StringRepr2::Ucs4(r) => {
                        while l_i < self.end {
                            if (l[l_i] as u32) != r[r_i] {
                                return false;
                            }
                            l_i += 1;
                            r_i += 1;
                        }
                        true
                    },
                }
            },
            StringRepr2::Ucs4(l) => {
                match other.container.as_ref() {
                    StringRepr2::Latin1(r) => {
                        while l_i < self.end {
                            if l[l_i] != (r[r_i] as u32) {
                                return false;
                            }
                            l_i += 1;
                            r_i += 1;
                        }
                        true
                    },
                    StringRepr2::Ucs2(r) => {
                        while l_i < self.end {
                            if l[l_i] != (r[r_i] as u32) {
                                return false;
                            }
                            l_i += 1;
                            r_i += 1;
                        }
                        true
                    },
                    StringRepr2::Ucs4(r) => {
                        while l_i < self.end {
                            if l[l_i] != r[r_i] {
                                return false;
                            }
                            l_i += 1;
                            r_i += 1;
                        }
                        true
                    },
                }
            },
        }
    }
}

#[derive(Clone)]
enum StringRepr2 {
    Latin1(Vec<u8>),
    Ucs2(Vec<u16>),
    Ucs4(Vec<u32>),
}

impl StringRepr2 {
    fn len(&self) -> usize {
        match self {
            StringRepr2::Latin1(v) => v.len(),
            StringRepr2::Ucs2(v) => v.len(),
            StringRepr2::Ucs4(v) => v.len(),
        }
    }

    fn char_at(&self, index: usize) -> char {
        match self {
            StringRepr2::Latin1(v) => v[index] as char,
            StringRepr2::Ucs2(v) => char::from_u32(v[index] as u32).unwrap_or('\x00'),
            StringRepr2::Ucs4(v) => char::from_u32(v[index]).unwrap_or('\x00'),
        }
    }
}

impl PartialEq for StringRepr2 {
    fn eq(&self, other: &Self) -> bool {
        match self {
            StringRepr2::Latin1(l) => {
                match other {
                    StringRepr2::Latin1(r) => {
                        l.eq(r)
                    },
                    StringRepr2::Ucs2(r) => {
                        l.iter().map(|&e| e as u16).collect::<Vec<u16>>().eq(r)
                    },
                    StringRepr2::Ucs4(r) => {
                        l.iter().map(|&e| e as u32).collect::<Vec<u32>>().eq(r)
                    },
                }
            },
            StringRepr2::Ucs2(l) => {
                match other {
                    StringRepr2::Latin1(r) => {
                        r.iter().map(|&e| e as u16).collect::<Vec<u16>>().eq(l)
                    },
                    StringRepr2::Ucs2(r) => {
                        l.eq(r)
                    },
                    StringRepr2::Ucs4(r) => {
                        l.iter().map(|&e| e as u32).collect::<Vec<u32>>().eq(r)
                    },
                }
            },
            StringRepr2::Ucs4(l) => {
                match other {
                    StringRepr2::Latin1(r) => {
                        r.iter().map(|&e| e as u32).collect::<Vec<u32>>().eq(l)
                    },
                    StringRepr2::Ucs2(r) => {
                        r.iter().map(|&e| e as u32).collect::<Vec<u32>>().eq(l)
                    },
                    StringRepr2::Ucs4(r) => {
                        l.eq(r)
                    },
                }
            },
        }
    }
}

impl PartialEq for SvString {
    fn eq(&self, other: &Self) -> bool {
        self.m_repr == other.m_repr
    }
}

impl PartialEq for StringRepr0 {
    fn eq(&self, other: &Self) -> bool {
        match &self.m_repr {
            StringRepr1::Reference(l) => {
                match &other.m_repr {
                    StringRepr1::Reference(r) => {
                        Arc::ptr_eq(l, r) || l == r
                    },
                    StringRepr1::Slice(r) => {
                        are_reference_and_slice_eq(l, r)
                    },
                }
            },
            StringRepr1::Slice(l) => {
                match &other.m_repr {
                    StringRepr1::Reference(r) => {
                        are_reference_and_slice_eq(r, l)
                    },
                    StringRepr1::Slice(r) => {
                        l == r
                    },
                }
            },
        }
    }
}

fn are_reference_and_slice_eq(l: &Arc<StringRepr2>, r: &Slice) -> bool {
    let l_l = l.len();
    if l_l != (r.end - r.start) {
        return false;
    }
    let mut l_i = 0;
    let mut r_i = r.start;
    match l.as_ref() {
        StringRepr2::Latin1(l) => {
            match r.container.as_ref() {
                StringRepr2::Latin1(r) => {
                    while l_i < l_l {
                        if l[l_i] != r[r_i] {
                            return false;
                        }
                        l_i += 1;
                        r_i += 1;
                    }
                    true
                },
                StringRepr2::Ucs2(r) => {
                    while l_i < l_l {
                        if (l[l_i] as u16) != r[r_i] {
                            return false;
                        }
                        l_i += 1;
                        r_i += 1;
                    }
                    true
                },
                StringRepr2::Ucs4(r) => {
                    while l_i < l_l {
                        if (l[l_i] as u32) != r[r_i] {
                            return false;
                        }
                        l_i += 1;
                        r_i += 1;
                    }
                    true
                },
            }
        },
        StringRepr2::Ucs2(l) => {
            match r.container.as_ref() {
                StringRepr2::Latin1(r) => {
                    while l_i < l_l {
                        if l[l_i] != (r[r_i] as u16) {
                            return false;
                        }
                        l_i += 1;
                        r_i += 1;
                    }
                    true
                },
                StringRepr2::Ucs2(r) => {
                    while l_i < l_l {
                        if l[l_i] != r[r_i] {
                            return false;
                        }
                        l_i += 1;
                        r_i += 1;
                    }
                    true
                },
                StringRepr2::Ucs4(r) => {
                    while l_i < l_l {
                        if (l[l_i] as u32) != r[r_i] {
                            return false;
                        }
                        l_i += 1;
                        r_i += 1;
                    }
                    true
                },
            }
        },
        StringRepr2::Ucs4(l) => {
            match r.container.as_ref() {
                StringRepr2::Latin1(r) => {
                    while l_i < l_l {
                        if l[l_i] != (r[r_i] as u32) {
                            return false;
                        }
                        l_i += 1;
                        r_i += 1;
                    }
                    true
                },
                StringRepr2::Ucs2(r) => {
                    while l_i < l_l {
                        if l[l_i] != (r[r_i] as u32) {
                            return false;
                        }
                        l_i += 1;
                        r_i += 1;
                    }
                    true
                },
                StringRepr2::Ucs4(r) => {
                    while l_i < l_l {
                        if l[l_i] != r[r_i] {
                            return false;
                        }
                        l_i += 1;
                        r_i += 1;
                    }
                    true
                },
            }
        },
    }
}

impl PartialOrd for SvString {
    fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
        self.m_repr.partial_cmp(&other.m_repr)
    }
}

impl PartialOrd for StringRepr0 {
    fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
        let mut l: Option<&StringRepr2> = None;
        let mut l_i: usize = 0;
        let mut l_l: usize = 0;
        let mut r: Option<&StringRepr2> = None;
        let mut r_i: usize = 0;
        let mut r_l: usize = 0;
        match &self.m_repr {
            StringRepr1::Reference(v) => {
                l_l = v.len();
                l = Some(v.as_ref());
            },
            StringRepr1::Slice(s) => {
                l = Some(s.container.as_ref());
                l_i = s.start;
                l_l = s.end;
            },
        }
        match &other.m_repr {
            StringRepr1::Reference(v) => {
                r_l = v.len();
                r = Some(v.as_ref());
            },
            StringRepr1::Slice(s) => {
                r = Some(s.container.as_ref());
                r_i = s.start;
                r_l = s.end;
            },
        }
        let l = l.unwrap();
        let r = r.unwrap();
        loop {
            if l_i >= l_l {
                return Some(Ordering::Less);
            }
            if r_i >= r_l {
                return Some(Ordering::Greater);
            }
            let l_char = l.char_at(l_i);
            let r_char = r.char_at(r_i);
            if l_char < r_char {
                return Some(Ordering::Less);
            }
            if l_char > r_char {
                return Some(Ordering::Greater);
            }
            l_i += 1;
            r_i += 1;
        }
        #[allow(unreachable_code)]
        Some(Ordering::Equal)
    }
}

impl From<char> for SvString {
    fn from(value: char) -> Self {
        SvString::from(value.to_string())
    }
}

impl From<String> for SvString {
    fn from(value: String) -> Self {
        SvString::from(value.as_ref())
    }
}

impl From<&str> for SvString {
    fn from(value: &str) -> Self {
        let mut r: Vec<char> = vec![];
        // largest Unicode ordinal
        let mut max_ordinal: char = '\x00';
        for c in value.chars() {
            r.push(c);
            max_ordinal = char::max(c, max_ordinal);
        }
        SvString {
            m_repr: intern(Arc::new(if max_ordinal < '\u{100}' {
                StringRepr0 {
                    m_len: r.len() as i32,
                    m_repr: StringRepr1::Reference(
                        Arc::new(StringRepr2::Latin1(r.iter().map(|&c| c as u8).collect()))
                    ),
                }
            } else if max_ordinal < '\u{10000}' {
                StringRepr0 {
                    m_len: r.len() as i32,
                    m_repr: StringRepr1::Reference(
                        Arc::new(StringRepr2::Ucs2(r.iter().map(|&c| c as u16).collect()))
                    ),
                }
            } else {
                StringRepr0 {
                    m_len: r.len() as i32,
                    m_repr: StringRepr1::Reference(
                        Arc::new(StringRepr2::Ucs4(r.iter().map(|&c| c as u32).collect()))
                    ),
                }
            })),
        }
    }
}

impl From<Vec<char>> for SvString {
    fn from(r: Vec<char>) -> Self {
        // largest Unicode ordinal
        let mut max_ordinal: char = '\x00';
        for &c in r.iter() {
            max_ordinal = char::max(c, max_ordinal);
        }
        SvString {
            m_repr: intern(Arc::new(if max_ordinal < '\u{100}' {
                StringRepr0 {
                    m_len: r.len() as i32,
                    m_repr: StringRepr1::Reference(
                        Arc::new(StringRepr2::Latin1(r.iter().map(|&c| c as u8).collect()))
                    ),
                }
            } else if max_ordinal < '\u{10000}' {
                StringRepr0 {
                    m_len: r.len() as i32,
                    m_repr: StringRepr1::Reference(
                        Arc::new(StringRepr2::Ucs2(r.iter().map(|&c| c as u16).collect()))
                    ),
                }
            } else {
                StringRepr0 {
                    m_len: r.len() as i32,
                    m_repr: StringRepr1::Reference(
                        Arc::new(StringRepr2::Ucs4(r.iter().map(|&c| c as u32).collect()))
                    ),
                }
            })),
        }
    }
}

impl SvString {
    /// Empty string.
    pub fn empty() -> Self {
        Self {
            m_repr: intern(Arc::new(StringRepr0 {
                m_len: 0,
                m_repr: StringRepr1::Reference(Arc::new(StringRepr2::Latin1(vec![]))),
            })),
        }
    }

    /// Length in Unicode Scalar Value.
    pub fn len(&self) -> i32 {
        self.m_repr.m_len
    }

    /// Converts this string into `String` type.
    pub fn to_standard_string(&self) -> String {
        self.chars().collect()
    }

    /// Obtains character at given position. If the index is out of bounds,
    /// this method returns `'\x00'`.
    pub fn char_at(&self, index: i32) -> char {
        if index >= 0 && index < self.m_repr.m_len { self.m_repr.m_repr.char_at(index as usize) } else { '\x00' }
    }

    /// Iterates over each character.
    pub fn chars(&self) -> impl Iterator<Item = char> + '_ {
        match &&self.m_repr.m_repr {
            StringRepr1::Reference(r) => {
                CharIterator {
                    container: r.as_ref(),
                    index: 0,
                    lim: r.len(),
                }
            },
            StringRepr1::Slice(s) => {
                CharIterator {
                    container: s.container.as_ref(),
                    index: s.start,
                    lim: s.end,
                }
            },
        }
    }

    /// Concatenates two strings.
    pub fn concat(&self, other: impl AnySvStringType) -> SvString {
        let other = other.convert();
        let mut r: Vec<char> = vec![];
        // largest Unicode ordinal
        let mut max_ordinal: char = '\x00';
        for ch in self.chars() {
            r.push(ch);
            max_ordinal = char::max(ch, max_ordinal);
        }
        for ch in other.chars() {
            r.push(ch);
            max_ordinal = char::max(ch, max_ordinal);
        }
        SvString {
            m_repr: intern(Arc::new(if max_ordinal < '\u{100}' {
                StringRepr0 {
                    m_len: r.len() as i32,
                    m_repr: StringRepr1::Reference(
                        Arc::new(StringRepr2::Latin1(r.iter().map(|&c| c as u8).collect()))
                    ),
                }
            } else if max_ordinal < '\u{10000}' {
                StringRepr0 {
                    m_len: r.len() as i32,
                    m_repr: StringRepr1::Reference(
                        Arc::new(StringRepr2::Ucs2(r.iter().map(|&c| c as u16).collect()))
                    ),
                }
            } else {
                StringRepr0 {
                    m_len: r.len() as i32,
                    m_repr: StringRepr1::Reference(
                        Arc::new(StringRepr2::Ucs4(r.iter().map(|&c| c as u32).collect()))
                    ),
                }
            })),
        }
    }

    /// Determines whether the string contains another string.
    pub fn contains(&self, other: impl AnySvStringType) -> bool {
        self.index_of(other) != -1
    }

    /// Determines whether the string starts with another string.
    pub fn starts_with(&self, other: impl AnySvStringType) -> bool {
        let other = other.convert();
        for i in 0..other.len() {
            if i >= self.len() || self.char_at(i) != other.char_at(i) {
                return false;
            }
        }
        true
    }

    /// Determines whether the string ends with another string.
    pub fn ends_with(&self, other: impl AnySvStringType) -> bool {
        let other = other.convert();
        let mut i = other.len() - 1;
        let mut j = self.len() - 1;
        while i >= 0 {
            if j < 0 || self.char_at(j) != other.char_at(i) {
                return false;
            }
            i -= 1;
            j -= 1;
        }
        true
    }

    /// Finds the index of the given string. If the argument is not found,
    /// this method returns `-1`.
    pub fn index_of(&self, other: impl AnySvStringType) -> i32 {
        let other = other.convert();
        let mut remaining = other.chars();
        let mut i: i32 = 0;
        let mut j: i32 = -1;
        for ch in self.chars() {
            if j == -1 {
                j = i;
            }
            let remaining_ch = remaining.next();
            if remaining_ch.is_none() {
                break;
            }
            if ch != remaining_ch.unwrap() {
                remaining = other.chars();
                j = -1;
            }
            i += 1;
        }
        if remaining.next().is_none() { j } else { -1 }
    }

    /// Finds the last index of the given string. If the argument is not found,
    /// this method returns `-1`.
    pub fn last_index_of(&self, other: impl AnySvStringType) -> i32 {
        let other = other.convert();
        if other.m_repr.m_len == 0 {
            return self.m_repr.m_len;
        }
        let mut remaining = other.chars();
        let mut i: i32 = 0;
        let mut j: i32 = -1;
        let mut r: Vec<i32> = vec![];
        for ch in self.chars() {
            if j == -1 {
                j = i;
            }
            let remaining_ch = remaining.next();
            if remaining_ch.is_none() {
                r.push(j);
                remaining = other.chars();
                j = -1;
            }
            if ch != remaining_ch.unwrap() {
                remaining = other.chars();
                j = -1;
            }
            i += 1;
        }
        if r.len() > 0 { r[r.len() - 1] } else { -1 }
    }

    /// Determines whether the string is empty.
    pub fn is_empty(&self) -> bool {
        self.m_repr.m_len == 0
    }

    /// Repeats the string _n_ times.
    pub fn repeat(&self, n: i32) -> SvString {
        if n < 1 {
            return SvString::empty();
        }
        let v = self.chars().collect::<Vec<char>>();
        let mut r: Vec<char> = vec![];
        // largest Unicode ordinal
        let mut max_ordinal: char = '\x00';
        for _ in 0..n {
            for &ch in v.iter() {
                r.push(ch);
                max_ordinal = char::max(ch, max_ordinal);
            }
        }
        SvString {
            m_repr: intern(Arc::new(if max_ordinal < '\u{100}' {
                StringRepr0 {
                    m_len: r.len() as i32,
                    m_repr: StringRepr1::Reference(
                        Arc::new(StringRepr2::Latin1(r.iter().map(|&c| c as u8).collect()))
                    ),
                }
            } else if max_ordinal < '\u{10000}' {
                StringRepr0 {
                    m_len: r.len() as i32,
                    m_repr: StringRepr1::Reference(
                        Arc::new(StringRepr2::Ucs2(r.iter().map(|&c| c as u16).collect()))
                    ),
                }
            } else {
                StringRepr0 {
                    m_len: r.len() as i32,
                    m_repr: StringRepr1::Reference(
                        Arc::new(StringRepr2::Ucs4(r.iter().map(|&c| c as u32).collect()))
                    ),
                }
            })),
        }
    }

    /// Converts string to lowercase.
    pub fn to_lowercase(&self) -> SvString {
        SvString::from(self.to_standard_string().to_lowercase())
    }

    /// Converts string to uppercase.
    pub fn to_uppercase(&self) -> SvString {
        SvString::from(self.to_standard_string().to_uppercase())
    }

    /// Extracts a substring.
    pub fn substr(&self, index: impl SvSubstringIndex) -> SvString {
        self.covered_substring(index.from_index(), index.to_index())
    }

    /// Extracts a substring.
    pub fn substr_with_length(&self, mut from: i32, mut len: i32) -> SvString {
        from = i32::max(0, from);
        from = i32::min(from, self.m_repr.m_len);
        len = i32::max(0, len);
        if from + len >= self.m_repr.m_len {
            self.substr(from..)
        } else {
            self.substr(from..(from + len))
        }
    }

    fn covered_substring(&self, mut from: i32, mut to: i32) -> SvString {
        from = i32::max(0, from);
        from = i32::min(from, self.m_repr.m_len);
        to = i32::max(0, to);
        to = i32::min(to, self.m_repr.m_len);
        if to < from {
            let k = from;
            from = to;
            to = k;
        }
        match &self.m_repr.m_repr {
            StringRepr1::Reference(r) => {
                SvString {
                    m_repr: intern(Arc::new(StringRepr0 {
                        m_len: to - from,
                        m_repr: StringRepr1::Slice(Slice {
                            container: r.clone(),
                            start: from as usize,
                            end: to as usize,
                        }),
                    }))
                }
            },
            StringRepr1::Slice(slice) => {
                SvString {
                    m_repr: intern(Arc::new(StringRepr0 {
                        m_len: to - from,
                        m_repr: StringRepr1::Slice(Slice {
                            container: slice.container.clone(),
                            start: slice.start + (from as usize),
                            end: slice.start + (to as usize),
                        }),
                    }))
                }
            },
        }
    }

    /// Removes leading and trailing whitespace.
    pub fn trim(&self) -> SvString {
        let mut i: i32 = 0;
        while is_whitespace(self.char_at(i)) {
            i += 1;
        }
        let mut j: i32 = self.len();
        while is_whitespace(self.char_at(j - 1)) {
            j -= 1;
        }
        self.substr(i..j)
    }

    /// Removes leading whitespace.
    pub fn trim_left(&self) -> SvString {
        let mut i: i32 = 0;
        while is_whitespace(self.char_at(i)) {
            i += 1;
        }
        self.substr(i..)
    }

    /// Removes trailing whitespace.
    pub fn trim_right(&self) -> SvString {
        let mut i: i32 = self.len();
        while is_whitespace(self.char_at(i - 1)) {
            i -= 1;
        }
        self.substr(..i)
    }

    /// Concatenates a list of strings with given separator.
    pub fn join(vector: Vec<SvString>, sep: impl AnySvStringType) -> SvString {
        let sep = sep.convert();
        let mut r: Vec<char> = vec![];
        let mut max_ordinal: char = '\x00';
        let mut add_sep = false;
        for s in vector {
            if add_sep {
                for ch in sep.chars() {
                    r.push(ch);
                    max_ordinal = char::max(ch, max_ordinal);
                }
            }
            for ch in s.chars() {
                r.push(ch);
                max_ordinal = char::max(ch, max_ordinal);
            }
            add_sep = true;
        }
        SvString {
            m_repr: intern(Arc::new(if max_ordinal < '\u{100}' {
                StringRepr0 {
                    m_len: r.len() as i32,
                    m_repr: StringRepr1::Reference(
                        Arc::new(StringRepr2::Latin1(r.iter().map(|&c| c as u8).collect()))
                    ),
                }
            } else if max_ordinal < '\u{10000}' {
                StringRepr0 {
                    m_len: r.len() as i32,
                    m_repr: StringRepr1::Reference(
                        Arc::new(StringRepr2::Ucs2(r.iter().map(|&c| c as u16).collect()))
                    ),
                }
            } else {
                StringRepr0 {
                    m_len: r.len() as i32,
                    m_repr: StringRepr1::Reference(
                        Arc::new(StringRepr2::Ucs4(r.iter().map(|&c| c as u32).collect()))
                    ),
                }
            })),
        }
    }
}

struct CharIterator<'a> {
    container: &'a StringRepr2,
    index: usize,
    lim: usize,
}

impl<'a> Iterator for CharIterator<'a> {
    type Item = char;
    fn next(&mut self) -> Option<Self::Item> {
        if self.index >= self.lim {
            return None;
        }
        let r = self.container.char_at(self.index);
        self.index += 1;
        Some(r)
    }
}

/// Allows taking any string type in certain
/// methods.
pub trait AnySvStringType {
    fn convert(&self) -> SvString;
}

impl AnySvStringType for SvString {
    fn convert(&self) -> SvString {
        self.clone()
    }
}

impl<'a> AnySvStringType for &'a str {
    fn convert(&self) -> SvString {
        SvString::from(*self)
    }
}

impl<'a> AnySvStringType for char {
    fn convert(&self) -> SvString {
        SvString::from(*self)
    }
}

/// Allows taking range expressions in certain methods.
pub trait SvSubstringIndex {
    fn from_index(&self) -> i32;
    fn to_index(&self) -> i32;
}

impl SvSubstringIndex for Range<i32> {
    fn from_index(&self) -> i32 {
        self.start
    }
    fn to_index(&self) -> i32 {
        self.end
    }
}

impl SvSubstringIndex for RangeFrom<i32> {
    fn from_index(&self) -> i32 {
        self.start
    }
    fn to_index(&self) -> i32 {
        i32::MAX
    }
}

impl SvSubstringIndex for RangeFull {
    fn from_index(&self) -> i32 {
        0
    }
    fn to_index(&self) -> i32 {
        i32::MAX
    }
}

impl SvSubstringIndex for RangeInclusive<i32> {
    fn from_index(&self) -> i32 {
        *self.start()
    }
    fn to_index(&self) -> i32 {
        self.end() + 1
    }
}

impl SvSubstringIndex for RangeTo<i32> {
    fn from_index(&self) -> i32 {
        0
    }
    fn to_index(&self) -> i32 {
        self.end
    }
}

impl SvSubstringIndex for RangeToInclusive<i32> {
    fn from_index(&self) -> i32 {
        0
    }
    fn to_index(&self) -> i32 {
        self.end + 1
    }
}

impl Debug for SvString {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.to_standard_string())
    }
}

impl Display for SvString {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.to_standard_string())
    }
}

impl Add for SvString {
    type Output = SvString;
    fn add(self, rhs: Self) -> Self::Output {
        self.concat(rhs)
    }
}