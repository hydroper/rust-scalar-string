pub fn is_whitespace(ch: char) -> bool {
    (ch >= '\x09' && ch <= '\x0D') ||
    ch == '\x20' ||
    ch == '\u{85}' ||
    ch == '\u{A0}' ||
    ch == '\u{1680}' ||
    (ch >= '\u{2000}' && ch <= '\u{200A}') ||
    ch == '\u{2028}' ||
    ch == '\u{2029}' ||
    ch == '\u{202F}' ||
    ch == '\u{205F}' ||
    ch == '\u{3000}'
}

pub fn is_decimal_digit(ch: char) -> bool {
    ch >= '\x30' && ch <= '\x39'
}

pub fn hex_digit_mv(ch: char) -> Option<i32> {
    if is_decimal_digit(ch) {
        Some((ch as i32) - 0x30)
    } else if ch >= 'a' && ch <= 'f' {
        Some((ch as i32) - ('a' as i32) + 10)
    } else if ch >= 'A' && ch <= 'F' {
        Some((ch as i32) - ('A' as i32) + 10)
    } else {
        None
    }
}