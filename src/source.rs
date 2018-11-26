use std::iter::Peekable;
use std::str::FromStr;
use std::{f64, u32};
use unicode_xid::UnicodeXID;

/// A range in a text document expressed as (zero-based) start and end positions. A range is
/// comparable to a selection in an editor. Therefore the end position is exclusive.
#[derive(Clone, Copy, Eq, PartialEq)]
pub struct Range {
    /// The range’s start position.
    start: Position,
    /// The range’s end position.
    end: Position,
}

impl Range {
    /// Creates a new range. The end position must be greater than or equal to the end position.
    pub fn new(start: Position, end: Position) -> Self {
        debug_assert!(end >= start);
        Range { start, end }
    }

    /// Creates a new range where both start and end are the provided position.
    pub fn position(position: Position) -> Self {
        Range {
            start: position,
            end: position,
        }
    }

    /// The initial range. The same as `Range::new(Position::initial(), Position::initial())`.
    pub fn initial() -> Self {
        INITIAL_RANGE
    }

    /// The range’s start position.
    pub fn start(&self) -> Position {
        self.start
    }

    /// The range’s end position.
    pub fn end(&self) -> Position {
        self.end
    }
}

/// Position in a text document expressed as zero-based line and zero-based character offset.
///
/// The offsets are based on a UTF-16 string representation. The sequences which represent a newline
/// are `\n`, `\r\n`, and `\r`.
///
/// We use 32 bit unsigned integers for the line and character properties. Together that gives the
/// position struct a size of 64 bits.
#[derive(Clone, Copy, Eq, Ord, PartialEq, PartialOrd)]
pub struct Position {
    /// Line position in a document (zero-based).
    line: u32,
    /// Character offset on a line in a document (zero-based).
    character: u32,
}

impl Position {
    /// Create a new position.
    pub fn new(line: u32, character: u32) -> Self {
        Position { line, character }
    }

    /// The initial position.
    pub fn initial() -> Self {
        INITIAL_POSITION
    }
}

const INITIAL_POSITION: Position = Position {
    line: 0,
    character: 0,
};

const INITIAL_RANGE: Range = Range {
    start: INITIAL_POSITION,
    end: INITIAL_POSITION,
};

/// Peekable iterator of source characters which keeps track of the current `Position`.
pub struct Chars<I>
where
    I: Iterator<Item = char>,
{
    position: Position,
    iter: Peekable<I>,
}

impl<I> Chars<I>
where
    I: Iterator<Item = char>,
{
    /// Create a new iterator starting at the provided position.
    pub fn new(position: Position, iter: I) -> Self {
        Chars {
            position,
            iter: iter.peekable(),
        }
    }

    /// Take a peek at the next character without advancing the iterator.
    pub fn peek(&mut self) -> Option<char> {
        self.iter.peek().cloned()
    }

    /// Get the current character position. Remember that the position is based on the UTF-16
    /// string representation.
    pub fn position(&self) -> Position {
        self.position
    }
}

impl<I> Iterator for Chars<I>
where
    I: Iterator<Item = char>,
{
    type Item = char;

    /// Advances the iterator updating the current position in the process.
    fn next(&mut self) -> Option<char> {
        let c = self.iter.next();
        if let Some(c) = c {
            // Newline sequences `\n`, `\r\n`, and `\r` move the position to the next line and reset
            // the character. If our sequence is `\r\n` then we skip the `\r` not counting it
            // against the position and then increment the line after `\n`.
            if c == '\n' {
                self.position.line += 1;
                self.position.character = 0;
            } else if c == '\r' {
                if self.peek() == Some('\n') {
                    // noop...
                } else {
                    self.position.line += 1;
                    self.position.character = 0;
                }
            } else {
                // Increment the character based on the UTF-16 length.
                self.position.character += c.len_utf16() as u32;
            }
        }
        c
    }
}

/// A name written in a Brite program. Brite identifiers follow the [Unicode Identifier
/// Specification][1] including the optional underscore (`_`) character.
///
/// Some strings which are valid identifier syntax are reserved as keywords to enable other syntax.
/// We try to reserve the minimum number of keywords possible.
///
/// We could only have keywords in certain positions. For instance, only have the keyword `fun` when
/// in an expression context. However, this introduces a potentially confusing rule. It also means,
/// in this example, code transformations could not easily make expression identifiers out of
/// pattern identifiers.
///
/// [1]: http://www.unicode.org/reports/tr31
#[derive(Debug, PartialEq)]
pub struct Identifier(String);

/// A keyword reserved in the identifier syntax.
#[derive(Debug, PartialEq)]
pub enum Keyword {
    /// `_`
    Hole,
    /// `true`
    True,
    /// `false`
    False,
}

impl Identifier {
    /// Parses an identifier from the source characters iterator.
    ///
    /// - `Some(Ok())` if we parsed a valid identifier.
    /// - `Some(Err())` if we parsed a keyword.
    /// - `None` if we could not parse an identifier. We consumed no characters from the iterator
    ///   in this case.
    pub fn parse<I>(iter: &mut Chars<I>) -> Option<Result<Identifier, Keyword>>
    where
        I: Iterator<Item = char>,
    {
        let mut s = String::new();

        match iter.peek() {
            None => return None,
            Some(c) => if Identifier::is_start(c) {
                s.push(c);
                iter.next();
            } else {
                return None;
            },
        };

        loop {
            match iter.peek() {
                None => break,
                Some(c) => if Identifier::is_continue(c) {
                    s.push(c);
                    iter.next();
                } else {
                    break;
                },
            };
        }

        s.shrink_to_fit();

        match s.as_ref() {
            "_" => Some(Err(Keyword::Hole)),
            "true" => Some(Err(Keyword::True)),
            "false" => Some(Err(Keyword::False)),
            _ => Some(Ok(Identifier(s))),
        }
    }

    fn is_start(c: char) -> bool {
        match c {
            // Optimization: Quickly detect ASCII Latin characters.
            'a'...'z' => true,
            'A'...'Z' => true,
            // Include the optional underscore character.
            '_' => true,
            // Delegate to `UnicodeXID::is_xid_start` for everything else.
            c => UnicodeXID::is_xid_start(c),
        }
    }

    fn is_continue(c: char) -> bool {
        match c {
            // Optimization: Quickly detect ASCII Latin characters and numbers.
            'a'...'z' => true,
            'A'...'Z' => true,
            '0'...'9' => true,
            // Include the optional underscore character.
            '_' => true,
            // Delegate to `UnicodeXID::is_xid_continue` for everything else.
            c => UnicodeXID::is_xid_continue(c),
        }
    }
}

/// An `Identifier` which comes with a `Range`.
pub struct Name {
    pub range: Range,
    pub identifier: Identifier,
}

/// Some number that we parsed from a source document. We only parse positive numbers. Negative
/// numbers may be created with the negative unary operator. All numbers are represented as 64-bit
/// floats according to the [IEEE 754-2008][1] standard. This means the largest integer we can
/// support is 32 bits. The syntax we accept for numbers includes:
///
/// - Integers: 0, 1, 42
/// - Decimals: 3.1415
/// - Exponential: 1e2, 3.14e2, 1e-2
/// - Hexadecimal: 0xFFF
/// - Binary: 0b101
///
/// [1]: https://en.wikipedia.org/wiki/Double-precision_floating-point_format
#[derive(Debug, PartialEq)]
pub struct Number {
    raw: Option<String>,
    value: f64,
}

impl Number {
    /// Parses a number from a character iterator.
    ///
    /// - `Some(Ok())` if we parsed a valid number.
    /// - `Some(Err())` if we failed to parse a number. The string is the raw number we tried
    ///   parsing. We consumed characters from the iterator in this case.
    /// - `None` if we could not parse a number. We consumed no characters from the iterator
    ///   in this case.
    ///
    /// NOTE: The caller of this function should probably check to see if there is an identifier or
    /// number immediately following the number. Since numbers include letter characters like `x`,
    /// `b`, and `e` we don’t want arbitrary identifiers following numbers as that might
    /// be confusing.
    pub fn parse<I>(iter: &mut Chars<I>) -> Option<Result<Number, String>>
    where
        I: Iterator<Item = char>,
    {
        let mut raw = String::new();

        // If the first number we parse is 0 then we may have a binary or hexadecimal number on
        // our hands.
        match iter.peek() {
            Some('0') => {
                raw.push(iter.next().unwrap());
                match iter.peek() {
                    // Parse a binary number.
                    Some('b') | Some('B') => {
                        raw.push(iter.next().unwrap());
                        loop {
                            match iter.peek() {
                                Some('0') | Some('1') => raw.push(iter.next().unwrap()),
                                _ => break,
                            }
                        }
                        let (_, binary) = raw.split_at(2);
                        return match u32::from_str_radix(binary, 2) {
                            Ok(value) => {
                                raw.shrink_to_fit();
                                let raw = Some(raw);
                                let value = f64::from(value);
                                Some(Ok(Number { raw, value }))
                            }
                            Err(_) => Some(Err(raw)),
                        };
                    }
                    // Parse a hexadecimal number.
                    Some('x') | Some('X') => {
                        raw.push(iter.next().unwrap());
                        loop {
                            match iter.peek() {
                                Some('0'...'9') | Some('a'...'f') | Some('A'...'F') => {
                                    raw.push(iter.next().unwrap())
                                }
                                _ => break,
                            }
                        }
                        let (_, hexadecimal) = raw.split_at(2);
                        return match u32::from_str_radix(hexadecimal, 16) {
                            Ok(value) => {
                                raw.shrink_to_fit();
                                let raw = Some(raw);
                                let value = f64::from(value);
                                Some(Ok(Number { raw, value }))
                            }
                            Err(_) => Some(Err(raw)),
                        };
                    }
                    _ => {}
                }
            }
            _ => {}
        }

        // Get all the digits in the whole part of the number.
        loop {
            match iter.peek() {
                Some('0'...'9') => raw.push(iter.next().unwrap()),
                _ => break,
            }
        }

        // Get all the digits in the fractional part of the number.
        if let Some('.') = iter.peek() {
            raw.push(iter.next().unwrap());
            loop {
                match iter.peek() {
                    Some('0'...'9') => raw.push(iter.next().unwrap()),
                    _ => break,
                }
            }
        }

        // If we parsed no characters then we don’t have a number to parse.
        if raw.is_empty() {
            None
        } else {
            // Get all the digits in the exponential part of the number.
            match iter.peek() {
                Some('e') | Some('E') => {
                    raw.push(iter.next().unwrap());
                    match iter.peek() {
                        Some('+') | Some('-') => raw.push(iter.next().unwrap()),
                        _ => {}
                    }
                    loop {
                        match iter.peek() {
                            Some('0'...'9') => raw.push(iter.next().unwrap()),
                            _ => break,
                        }
                    }
                }
                _ => {}
            }

            match f64::from_str(&raw) {
                Ok(value) => {
                    raw.shrink_to_fit();
                    let raw = Some(raw);
                    Some(Ok(Number { raw, value }))
                }
                Err(_) => Some(Err(raw)),
            }
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn parse_identifier() {
        const OK: Result<(), Keyword> = Ok(());

        let cases: Vec<(&'static str, Option<Result<(), Keyword>>)> = vec![
            ("", None),
            ("%", None),
            ("4", None),
            ("x", Some(OK)),
            ("x4", Some(OK)),
            ("Θ", Some(OK)),
            ("Θ2", Some(OK)),
            ("_x", Some(OK)),
            ("x_", Some(OK)),
            ("a_b", Some(OK)),
            ("_0", Some(OK)),
            ("'", None),
            ("🙂", None),
            ("_", Some(Err(Keyword::Hole))),
            ("true", Some(Err(Keyword::True))),
            ("false", Some(Err(Keyword::False))),
        ];

        for (source, expected) in cases {
            let mut chars = Chars::new(Position::initial(), source.chars());
            let actual = Identifier::parse(&mut chars);
            let expected =
                expected.map(|expected| expected.map(|()| Identifier(String::from(source))));
            assert_eq!(actual, expected);
        }
    }

    #[test]
    fn parse_number() {
        const ERR: Result<f64, ()> = Err(());

        let cases: Vec<(&'static str, Option<Result<f64, ()>>)> = vec![
            ("", None),
            ("x", None),
            ("e", None),
            ("E", None),
            ("e2", None),
            ("E2", None),
            ("0", Some(Ok(0.))),
            ("1", Some(Ok(1.))),
            ("3.14", Some(Ok(3.14))),
            ("-1", None),
            ("12", Some(Ok(12.))),
            ("01", Some(Ok(1.))),
            ("012", Some(Ok(12.))),
            ("001", Some(Ok(1.))),
            ("0012", Some(Ok(12.))),
            ("0b", Some(ERR)),
            ("0b0", Some(Ok(0.))),
            ("0b1", Some(Ok(1.))),
            ("0b10", Some(Ok(2.))),
            ("0b11", Some(Ok(3.))),
            ("0b10", Some(Ok(0b10.into()))),
            ("0b11", Some(Ok(0b11.into()))),
            ("0B", Some(ERR)),
            ("0B0", Some(Ok(0.))),
            ("0B1", Some(Ok(1.))),
            ("0B10", Some(Ok(2.))),
            ("0B11", Some(Ok(3.))),
            ("0x", Some(ERR)),
            ("0x0", Some(Ok(0.))),
            ("0x1", Some(Ok(1.))),
            ("0xb", Some(Ok(11.))),
            ("0xB", Some(Ok(11.))),
            ("0x10", Some(Ok(16.))),
            ("0x11", Some(Ok(17.))),
            ("0x1b", Some(Ok(27.))),
            ("0xb1", Some(Ok(177.))),
            ("0x1B", Some(Ok(27.))),
            ("0xB1", Some(Ok(177.))),
            ("0X", Some(ERR)),
            ("0X0", Some(Ok(0.))),
            ("0X1", Some(Ok(1.))),
            ("0Xb", Some(Ok(11.))),
            ("0XB", Some(Ok(11.))),
            ("0X10", Some(Ok(16.))),
            ("0X11", Some(Ok(17.))),
            ("0X1b", Some(Ok(27.))),
            ("0Xb1", Some(Ok(177.))),
            ("0X1B", Some(Ok(27.))),
            ("0XB1", Some(Ok(177.))),
            ("0b11111111111111111111111111111111", Some(Ok(4294967295.))),
            ("0b100000000000000000000000000000000", Some(ERR)),
            ("0xFFFFFFFF", Some(Ok(4294967295.))),
            ("0x100000000", Some(ERR)),
            ("1", Some(Ok(1.))),
            ("42", Some(Ok(42.))),
            ("4294967295", Some(Ok(4294967295.))),
            ("4294967296", Some(Ok(4294967296.))),
            ("9007199254740991", Some(Ok(9007199254740991.))),
            ("9007199254740992", Some(Ok(9007199254740992.))),
            ("9007199254740993", Some(Ok(9007199254740993.))),
            ("9007199254741000", Some(Ok(9007199254741000.))),
            ("12345678912345678", Some(Ok(12345678912345678.))),
            ("123456789123456789", Some(Ok(123456789123456789.))),
            ("1234567891234567891", Some(Ok(1234567891234567891.))),
            (".4294967295", Some(Ok(0.4294967295))),
            (".4294967296", Some(Ok(0.4294967296))),
            (".9007199254740991", Some(Ok(0.9007199254740991))),
            (".9007199254740992", Some(Ok(0.9007199254740992))),
            (".9007199254740993", Some(Ok(0.9007199254740993))),
            (".9007199254741000", Some(Ok(0.9007199254741000))),
            (".12345678912345678", Some(Ok(0.12345678912345678))),
            (".123456789123456789", Some(Ok(0.123456789123456789))),
            (".1234567891234567891", Some(Ok(0.1234567891234567891))),
            ("0.4294967295", Some(Ok(0.4294967295))),
            ("0.4294967296", Some(Ok(0.4294967296))),
            ("0.9007199254740991", Some(Ok(0.9007199254740991))),
            ("0.9007199254740992", Some(Ok(0.9007199254740992))),
            ("0.9007199254740993", Some(Ok(0.9007199254740993))),
            ("0.9007199254741000", Some(Ok(0.9007199254741000))),
            ("0.12345678912345678", Some(Ok(0.12345678912345678))),
            ("0.123456789123456789", Some(Ok(0.123456789123456789))),
            ("0.1234567891234567891", Some(Ok(0.1234567891234567891))),
            ("429.4967295", Some(Ok(429.4967295))),
            ("429.4967296", Some(Ok(429.4967296))),
            ("900.7199254740991", Some(Ok(900.7199254740991))),
            ("900.7199254740992", Some(Ok(900.7199254740992))),
            ("900.7199254740993", Some(Ok(900.7199254740993))),
            ("900.7199254741000", Some(Ok(900.7199254741000))),
            ("123.45678912345678", Some(Ok(123.45678912345678))),
            ("123.456789123456789", Some(Ok(123.456789123456789))),
            ("123.4567891234567891", Some(Ok(123.4567891234567891))),
            ("3.1415", Some(Ok(3.1415))),
            (".5", Some(Ok(0.5))),
            (".21", Some(Ok(0.21))),
            (".200", Some(Ok(0.2))),
            ("002", Some(Ok(2.))),
            ("200", Some(Ok(200.))),
            ("200.2", Some(Ok(200.2))),
            (".002", Some(Ok(0.002))),
            ("2.002", Some(Ok(2.002))),
            ("2e2", Some(Ok(200.0))),
            ("2e+2", Some(Ok(200.0))),
            ("2e-2", Some(Ok(0.02))),
            ("3.14e2", Some(Ok(314.0))),
            ("3.14e-1", Some(Ok(0.314))),
            ("31.4e-1", Some(Ok(3.14))),
            ("1e308", Some(Ok(1e308))),
            ("1e309", Some(Ok(f64::INFINITY))),
            ("1e-500", Some(Ok(0.))),
        ];

        for (source, expected) in cases {
            let mut chars = Chars::new(Position::initial(), source.chars());
            let actual = Number::parse(&mut chars);
            let expected = expected.map(|expected| {
                expected
                    .map(|value| {
                        let raw = Some(String::from(source));
                        Number { raw, value }
                    }).map_err(|()| String::from(source))
            });
            assert_eq!(actual, expected);
        }
    }
}
