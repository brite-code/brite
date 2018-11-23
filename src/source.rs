use std::iter::Peekable;

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

    /// The initial range. The same as `Range::new(Position::initial(), Position::initial())`.
    pub fn initial() -> Self {
        INITIAL_RANGE
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
pub struct Identifier(String);

/// A keyword reserved in the identifier syntax.
pub enum Keyword {
    /// `_`
    Hole,
    /// `true`
    True,
    /// `false`
    False,
}

/// An `Identifier` which comes with a `Range`.
pub struct Name {
    pub range: Range,
    pub identifier: Identifier,
}

/// Some number that we parsed from a source document. We only parse positive numbers. Negative
/// numbers may be created with the negative unary operator. All numbers are represented as 64-bit
/// floats. The syntax we accept for numbers includes:
///
/// - Integers: 0, 1, 42
/// - Decimals: 3.1415
/// - Exponential: 1e2, 3.14e2, 1e-2
/// - Hexadecimal: 0xFFF
/// - Binary: 0b101
pub struct Number {
    raw: String,
    value: f64,
}

/// A token in Brite source code is a range of text with some simple semantic meaning. When parsing
/// a source document we produce a list of tokens whose positions when added together should be the
/// full range of the document.
pub struct Token {
    /// The token’s full starting position. Including whitespace and comments.
    pub full_start: Position,
    /// The token’s start position.
    pub start: Position,
    /// The token’s end position.
    pub end: Position,
    /// The description of the token.
    pub description: TokenDescription,
}

pub enum TokenDescription {
    Glyph(Glyph),
    Identifier(Identifier),
    Number(Number),
    UnexpectedChar(char),
}

/// A glyph is some symbol which is a part of Brite source code.
pub enum Glyph {
    Keyword(Keyword),
}
