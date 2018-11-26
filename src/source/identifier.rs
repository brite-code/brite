use super::position::{Chars, Range};
use unicode_xid::UnicodeXID;

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

#[cfg(test)]
mod tests {
    use super::super::position::Position;
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
}
