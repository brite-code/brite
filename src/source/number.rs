use super::document::DocumentChars;
use std::str::FromStr;
use std::{f64, u32};

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
#[derive(Clone, Debug, PartialEq)]
pub struct Number {
    raw: String,
    value: f64,
}

impl Number {
    /// Gets the raw string representation of this number.
    pub fn raw(&self) -> &str {
        &self.raw
    }

    /// Converts the number into its raw representation.
    pub fn into_raw(self) -> String {
        self.raw
    }

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
    pub fn parse(chars: &mut DocumentChars) -> Option<Result<Number, String>> {
        let mut raw = String::new();

        // If the first number we parse is 0 then we may have a binary or hexadecimal number on
        // our hands.
        match chars.lookahead() {
            Some('0') => {
                raw.push(chars.advance().unwrap());
                match chars.lookahead() {
                    // Parse a binary number.
                    Some('b') | Some('B') => {
                        raw.push(chars.advance().unwrap());
                        loop {
                            match chars.lookahead() {
                                Some('0') | Some('1') => raw.push(chars.advance().unwrap()),
                                _ => break,
                            }
                        }
                        let (_, binary) = raw.split_at(2);
                        return match u32::from_str_radix(binary, 2) {
                            Ok(value) => {
                                raw.shrink_to_fit();
                                let value = f64::from(value);
                                Some(Ok(Number { raw, value }))
                            }
                            Err(_) => Some(Err(raw)),
                        };
                    }
                    // Parse a hexadecimal number.
                    Some('x') | Some('X') => {
                        raw.push(chars.advance().unwrap());
                        loop {
                            match chars.lookahead() {
                                Some('0'...'9') | Some('a'...'f') | Some('A'...'F') => {
                                    raw.push(chars.advance().unwrap())
                                }
                                _ => break,
                            }
                        }
                        let (_, hexadecimal) = raw.split_at(2);
                        return match u32::from_str_radix(hexadecimal, 16) {
                            Ok(value) => {
                                raw.shrink_to_fit();
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
            match chars.lookahead() {
                Some('0'...'9') => raw.push(chars.advance().unwrap()),
                _ => break,
            }
        }

        // Get all the digits in the fractional part of the number.
        if let Some('.') = chars.lookahead() {
            raw.push(chars.advance().unwrap());
            loop {
                match chars.lookahead() {
                    Some('0'...'9') => raw.push(chars.advance().unwrap()),
                    _ => break,
                }
            }
        }

        // If we parsed no characters then we don’t have a number to parse.
        if raw.is_empty() {
            None
        } else {
            // Get all the digits in the exponential part of the number.
            match chars.lookahead() {
                Some('e') | Some('E') => {
                    raw.push(chars.advance().unwrap());
                    match chars.lookahead() {
                        Some('+') | Some('-') => raw.push(chars.advance().unwrap()),
                        _ => {}
                    }
                    loop {
                        match chars.lookahead() {
                            Some('0'...'9') => raw.push(chars.advance().unwrap()),
                            _ => break,
                        }
                    }
                }
                _ => {}
            }

            match f64::from_str(&raw) {
                Ok(value) => {
                    raw.shrink_to_fit();
                    Some(Ok(Number { raw, value }))
                }
                Err(_) => Some(Err(raw)),
            }
        }
    }
}

#[cfg(test)]
mod tests {
    use super::super::document::Document;
    use super::*;

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
            let document = Document::new("/path/to/document.txt".into(), source.into());
            let actual = Number::parse(&mut document.chars());
            let expected = expected.map(|expected| {
                expected
                    .map(|value| {
                        let raw = String::from(source);
                        Number { raw, value }
                    }).map_err(|()| String::from(source))
            });
            assert_eq!(actual, expected);
        }
    }
}
