use std::iter::Peekable;

/// Position between two characters in a text document expressed as zero-based line and zero-based
/// character offset.
///
/// The offsets are based on a UTF-16 string representation. The sequences which represent a newline
/// are `\n`, `\r\n`, and `\r`.
///
/// We use 32 bit unsigned integers for the line and character properties. Together that gives the
/// position struct a size of 64 bits.
#[derive(Clone, Copy, Debug, Eq, Ord, PartialEq, PartialOrd)]
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

const INITIAL_POSITION: Position = Position {
    line: 0,
    character: 0,
};

const INITIAL_RANGE: Range = Range {
    start: INITIAL_POSITION,
    end: INITIAL_POSITION,
};

/// Peekable iterator of source characters which keeps track of the current `Position`. Remember
/// that `Position` is based on the UTF-16 representation of the characters so will be different
/// than Rust’s UTF-8 offsets. You won’t be able to index into a string using the `Position`!
///
/// A `Position` represents a location in between two characters. Like the cursor in a text editor.
/// Consider the string `abc`. There are three letters but four locations in between the letters. In
/// `|a|b|c|` each bar (`|`) shows a position.
///
/// We consider the current character to be the character returned by `Chars::peek()`. When you call
/// `Chars::position()` you get the position between the current character and the last character.
/// Consider the string `abcdef`:
///
/// ```
/// abc|def
///     ^
/// ```
///
/// In this string the caret (`^`) represents the current character returned by `Chars::peek()`. The
/// bar (`|`) represents the current position returned by `Chars::position()`.
pub struct Chars<I>
where
    I: Iterator<Item = char>,
{
    position: Position,
    chars: Peekable<I>,
}

impl<I> Chars<I>
where
    I: Iterator<Item = char>,
{
    /// Create a new iterator starting at the provided position.
    pub fn new(position: Position, chars: I) -> Self {
        Chars {
            position,
            chars: chars.peekable(),
        }
    }

    /// Take a peek at the next character without advancing the iterator.
    pub fn peek(&mut self) -> Option<char> {
        self.chars.peek().cloned()
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
        let c = self.chars.next();
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

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn chars_position() {
        let mut chars = Chars::new(Position::initial(), "abc\ndef\rghi\r\njkl".chars());
        assert_eq!(chars.position(), Position::new(0, 0));
        assert_eq!(chars.next(), Some('a'));
        assert_eq!(chars.position(), Position::new(0, 1));
        assert_eq!(chars.next(), Some('b'));
        assert_eq!(chars.position(), Position::new(0, 2));
        assert_eq!(chars.next(), Some('c'));
        assert_eq!(chars.position(), Position::new(0, 3));
        assert_eq!(chars.next(), Some('\n'));
        assert_eq!(chars.position(), Position::new(1, 0));
        assert_eq!(chars.next(), Some('d'));
        assert_eq!(chars.position(), Position::new(1, 1));
        assert_eq!(chars.next(), Some('e'));
        assert_eq!(chars.position(), Position::new(1, 2));
        assert_eq!(chars.next(), Some('f'));
        assert_eq!(chars.position(), Position::new(1, 3));
        assert_eq!(chars.next(), Some('\r'));
        assert_eq!(chars.position(), Position::new(2, 0));
        assert_eq!(chars.next(), Some('g'));
        assert_eq!(chars.position(), Position::new(2, 1));
        assert_eq!(chars.next(), Some('h'));
        assert_eq!(chars.position(), Position::new(2, 2));
        assert_eq!(chars.next(), Some('i'));
        assert_eq!(chars.position(), Position::new(2, 3));
        assert_eq!(chars.next(), Some('\r'));
        assert_eq!(chars.position(), Position::new(2, 3));
        assert_eq!(chars.next(), Some('\n'));
        assert_eq!(chars.position(), Position::new(3, 0));
        assert_eq!(chars.next(), Some('j'));
        assert_eq!(chars.position(), Position::new(3, 1));
        assert_eq!(chars.next(), Some('k'));
        assert_eq!(chars.position(), Position::new(3, 2));
        assert_eq!(chars.next(), Some('l'));
        assert_eq!(chars.position(), Position::new(3, 3));
        assert_eq!(chars.next(), None);
        assert_eq!(chars.position(), Position::new(3, 3));
        assert_eq!(chars.next(), None);
        assert_eq!(chars.position(), Position::new(3, 3));
    }
}
