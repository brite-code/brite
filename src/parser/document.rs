use std::cmp;
use std::rc::Rc;
use std::str::Chars;

/// A Brite source code document. Source code is represented as text and turned into an AST through
/// the process of lexing and parsing. This object contains metadata about a source document
/// including the document’s URI, the full document text, and information for determining the
/// placement of source positions.
pub struct Document {
    /// The document’s contents as a UTF-8 string. The [Language Server Protocol (LSP)][1]
    /// represents all positions in terms of UTF-16, though! So be careful when using UTF-8 indexes.
    ///
    /// The source is wrapped in an `Rc` so that we can share the document‘s source text.
    source: Rc<String>,
    /// Locations in our document where new lines begin. New lines are created by `\n`, `\r\n`,
    /// and `\r`.
    lines: Vec<u32>,
}

impl Document {
    /// Creates a new document.
    pub fn new(source: String) -> Self {
        // Calculate all the line boundaries in our source code.
        let lines = {
            let mut lines = Vec::new();
            let mut i = 0;
            let bytes = source.as_bytes();
            while i < bytes.len() {
                let byte = bytes[i];
                i += 1;
                if byte == b'\n' {
                    lines.push(i as u32);
                } else if byte == b'\r' {
                    // If the next byte is `\n` then skip it. We only want to add a single line for
                    // the sequence `\r\n`.
                    if i + 1 < bytes.len() && bytes[i] == b'\n' {
                        i += 1;
                    }
                    lines.push(i as u32);
                }
            }
            lines
        };
        println!("{:?}", source);
        println!("{:?}", lines);
        let source = Rc::new(source);
        // Return the created document.
        Document { source, lines }
    }

    /// Returns a reference to the document’s source string.
    pub fn source(&self) -> &str {
        &self.source
    }

    /// Returns a reference counted pointer ([`Rc`]) to the document’s source string.
    pub fn source_rc(&self) -> Rc<String> {
        Rc::clone(&self.source)
    }

    /// Gets an iterator of characters in the source document along with the position of
    /// that character.
    pub fn chars(&self) -> DocumentChars {
        DocumentChars {
            source_len: self.source.len(),
            chars: self.source.chars(),
            lookahead: DocumentCharsLookahead::None,
        }
    }
}

/// An iterator of characters in a Brite source document. We don’t implement the `Iterator` trait
/// for this struct since it forces composition on us. We don’t care about iterator composition in
/// our parser. We’d prefer domain specific iterators at every step along the way.
///
/// Let’s illustrate what the methods of `DocumentChars` do. In the following diagram, the caret
/// (`^`) represents the current character returned by `DocumentChars::lookahead()`. The bar (`|`)
/// represents the current position returned by  `DocumentChars::position()`.
///
/// ```txt
/// |abcdef
///  ^
/// ```
///
/// We start by pointing at the first character. Calling `DocumentChars::advance()` returns `a` and
/// advances our iterator to the next character.
///
/// ```txt
/// a|bcdef
///   ^
/// ```
///
/// This continues until we reach the last character.
///
/// ```txt
/// abcde|f
///       ^
/// ```
///
/// Calling `DocumentChars::advance()` here will return `f` and puts us in the following state.
///
/// ```txt
/// abcdef|
///        ^
/// ```
///
/// Now calling `DocumentChars::advance()` will return `None` and will continue to return `None`
/// every time it is called. The position returned by `DocumentChars::position()` is the final
/// position in our document.
pub struct DocumentChars<'a> {
    source_len: usize,
    chars: Chars<'a>,
    lookahead: DocumentCharsLookahead,
}

impl<'a> DocumentChars<'a> {
    /// Consumes a character and advances our iterator to the next character. To look at the next
    /// character without consuming it, call `DocumentChars::lookahead()`.
    ///
    /// When `None` is returned we’ve reached the end of our document’s characters. Calling
    /// `DocumentChars::advance()` will only return `None` now.
    #[inline(always)]
    pub fn advance(&mut self) -> Option<char> {
        match self.lookahead {
            DocumentCharsLookahead::None => self.chars.next(),
            DocumentCharsLookahead::Lookahead1(lookahead1) => {
                self.lookahead = DocumentCharsLookahead::None;
                lookahead1
            }
            DocumentCharsLookahead::Lookahead2(lookahead1, lookahead2) => {
                self.lookahead = DocumentCharsLookahead::Lookahead1(lookahead2);
                lookahead1
            }
        }
    }

    /// Looks at the next character without advancing the iterator.
    #[inline(always)]
    pub fn lookahead(&mut self) -> Option<char> {
        match self.lookahead {
            DocumentCharsLookahead::None => {
                let lookahead1 = self.chars.next();
                self.lookahead = DocumentCharsLookahead::Lookahead1(lookahead1);
                lookahead1
            }
            DocumentCharsLookahead::Lookahead1(lookahead1) => lookahead1,
            DocumentCharsLookahead::Lookahead2(lookahead1, _) => lookahead1,
        }
    }

    /// Looks two characters ahead without advancing the iterator.
    pub fn lookahead2(&mut self) -> Option<char> {
        match self.lookahead {
            DocumentCharsLookahead::None => {
                let lookahead1 = self.chars.next();
                let lookahead2 = self.chars.next();
                self.lookahead = DocumentCharsLookahead::Lookahead2(lookahead1, lookahead2);
                lookahead2
            }
            DocumentCharsLookahead::Lookahead1(lookahead1) => {
                let lookahead2 = self.chars.next();
                self.lookahead = DocumentCharsLookahead::Lookahead2(lookahead1, lookahead2);
                lookahead2
            }
            DocumentCharsLookahead::Lookahead2(_, lookahead2) => lookahead2,
        }
    }

    /// Returns the position between the previous character and the next character. See the
    /// documentation on `DocumentChars` for more information.
    pub fn position(&self) -> Position {
        let index = self.source_len - self.chars.as_str().len();
        let index = match self.lookahead {
            DocumentCharsLookahead::None => index,
            DocumentCharsLookahead::Lookahead1(c1) => index - c1.map_or(0, char::len_utf8),
            DocumentCharsLookahead::Lookahead2(c1, c2) => {
                index - c1.map_or(0, char::len_utf8) - c2.map_or(0, char::len_utf8)
            }
        };
        let index = index as u32;
        Position(index)
    }
}

/// Lookahead state for `DocumentChars`.
enum DocumentCharsLookahead {
    None,
    Lookahead1(Option<char>),
    Lookahead2(Option<char>, Option<char>),
}

/// A position between two characters in a Brite source code document. This is the same as a
/// position in the [Language Server Protocol (LSP)][1]. To get the line and character locations of
/// a position you need a `Document` object.
///
/// Some examples of positions where `|` represents a position:
///
/// - `|abcdef`: Here the position is 0 since it is at the very beginning of our string.
/// - `a|bcdef`: Here the position is 1 since it is between our first and second characters.
/// - `abc|def`: Here the position is 3 since it is between our third and fourth characters.
/// - `abcdef|`: Here the position is 6 since it is after our sixth character at the end.
///
/// We need to keep this small as an AST will contain a _lot_ of positions. Currently 32 bits.
///
/// [1]: https://microsoft.github.io/language-server-protocol/specification
#[derive(Copy, Clone, Debug, Eq, Ord, PartialEq, PartialOrd)]
pub struct Position(
    /// The UTF-8 index in our source document for this position.
    u32,
);

impl Position {
    /// Creates a new position with the provided index.
    pub fn new(index: u32) -> Self {
        Position(index)
    }

    /// The initial position.
    pub fn initial() -> Self {
        Position(0)
    }

    /// Gets the UTF-8 index for this position.
    #[inline(always)]
    pub fn index_utf8(self) -> u32 {
        self.0
    }

    /// The zero-based line number of this position in the provided document. A new line is
    /// created by `\n`, `\r\n`, or `\r`.
    pub fn line(&self, document: &Document) -> usize {
        match document.lines.binary_search(&self.0) {
            Ok(line) => line + 1,
            Err(line) => line,
        }
    }

    /// The zero-based character number of this position in the provided document. As per the
    /// [LSP][1] specification, character offsets are measured in UTF-16 code units.
    ///
    /// [1]: https://microsoft.github.io/language-server-protocol/specification
    pub fn character(self, document: &Document) -> usize {
        let bytes = document.source.as_bytes();
        let line = self.line(document);
        let start = if line == 0 {
            0
        } else {
            document.lines[line - 1] as usize
        };
        let end = self.0 as usize;
        if start >= bytes.len() {
            end - bytes.len()
        } else if end > bytes.len() {
            let line_bytes = &document.source.as_bytes()[start..];
            (end - bytes.len())
                + String::from_utf8_lossy(line_bytes)
                    .chars()
                    .map(|c| c.len_utf16())
                    .sum::<usize>()
        } else {
            let line_bytes = &document.source.as_bytes()[start..end];
            String::from_utf8_lossy(line_bytes)
                .chars()
                .map(|c| c.len_utf16())
                .sum()
        }
    }

    /// Displays the position as a string. We don’t use the [`Display`] trait because we need a
    /// [`Document`] to get an accurate line and column number.
    pub fn display(self, document: &Document) -> String {
        format!(
            "{}:{}",
            self.line(document) + 1,
            self.character(document) + 1
        )
    }
}

/// A range in a text document expressed as start and end positions. A range is comparable to a
/// selection in an editor. Therefore the end position is exclusive.
///
/// We need to keep this small as an AST will contain a _lot_ of ranges. Currently 64 bits.
#[derive(Clone, Copy, Debug, Eq, PartialEq)]
pub struct Range {
    /// The range’s start position.
    start: Position,
    /// The length of UTF-8 bytes covered by this range.
    length: u32,
}

impl Range {
    /// Creates a new range with a start and end position. If `end` is less than `start` we flip the
    /// arguments around.
    pub fn new(start: Position, length: u32) -> Self {
        Range { start, length }
    }

    /// Gets the range of a single [`char`] that starts at the provided position.
    ///
    /// This only measures the range of a single [`char`]! It is incorrect in respect to CRLF!
    pub fn single_char(start: Position, c: char) -> Self {
        Range {
            start,
            length: c.len_utf8() as u32,
        }
    }

    /// Create an empty range at the provided position.
    pub fn position(position: Position) -> Self {
        Self::new(position, 0)
    }

    /// Create an empty range at the initial position.
    pub fn initial() -> Self {
        Self::position(Position::initial())
    }

    /// Creates a new range between two positions.
    pub fn between(start: Position, end: Position) -> Self {
        if start <= end {
            Range {
                start,
                length: end.0 - start.0,
            }
        } else {
            Range {
                start: end,
                length: start.0 - end.0,
            }
        }
    }

    /// Creates a new range that covers both of the provided ranges.
    pub fn union(self, other: Range) -> Self {
        if self.start <= other.start {
            Range {
                start: self.start,
                length: cmp::max(self.length, (other.start.0 - self.start.0) + other.length),
            }
        } else {
            Range {
                start: other.start,
                length: cmp::max(other.length, (self.start.0 - other.start.0) + self.length),
            }
        }
    }

    /// Returns the start position of our range.
    #[inline(always)]
    pub fn start(self) -> Position {
        self.start
    }

    /// Returns the end position of our range. Will always be greater than or equal to the start
    /// position. Remember that range is not inclusive.
    #[inline(always)]
    pub fn end(self) -> Position {
        Position(self.start.0 + self.length)
    }

    /// Displays the range as a string. We don’t use the [`Display`] trait because we need a
    /// [`Document`] to get an accurate line and column number.
    pub fn display(self, document: &Document) -> String {
        format!(
            "{}-{}",
            self.start.display(document),
            self.end().display(document)
        )
    }

    /// Does this range intersect with the other range?
    pub fn intersects(self, other: Range) -> bool {
        !(self.end() < other.start || other.end() < self.start)
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn document_lines() {
        let document = Document::new("abc\ndef\rghi\r\njkl".into());
        assert_eq!(document.lines, vec![4, 8, 13]);
    }

    #[test]
    fn position_line() {
        let document = Document::new("abc\ndef\rghi\r\njkl".into());
        assert_eq!(document.lines, vec![4, 8, 13]);
        assert_eq!(Position(0).line(&document), 0);
        assert_eq!(Position(1).line(&document), 0);
        assert_eq!(Position(2).line(&document), 0);
        assert_eq!(Position(3).line(&document), 0);
        assert_eq!(Position(4).line(&document), 1);
        assert_eq!(Position(5).line(&document), 1);
        assert_eq!(Position(6).line(&document), 1);
        assert_eq!(Position(7).line(&document), 1);
        assert_eq!(Position(8).line(&document), 2);
        assert_eq!(Position(9).line(&document), 2);
        assert_eq!(Position(10).line(&document), 2);
        assert_eq!(Position(11).line(&document), 2);
        assert_eq!(Position(12).line(&document), 2);
        assert_eq!(Position(13).line(&document), 3);
        assert_eq!(Position(14).line(&document), 3);
        assert_eq!(Position(15).line(&document), 3);
        assert_eq!(Position(16).line(&document), 3);
        assert_eq!(Position(17).line(&document), 3);
        assert_eq!(Position(500).line(&document), 3);

        let document = Document::new("abc\n".into());
        assert_eq!(document.lines, vec![4]);
        assert_eq!(Position(0).line(&document), 0);
        assert_eq!(Position(1).line(&document), 0);
        assert_eq!(Position(2).line(&document), 0);
        assert_eq!(Position(3).line(&document), 0);
        assert_eq!(Position(4).line(&document), 1);
        assert_eq!(Position(5).line(&document), 1);
        assert_eq!(Position(6).line(&document), 1);
        assert_eq!(Position(500).line(&document), 1);

        let document = Document::new("\nabc".into());
        assert_eq!(document.lines, vec![1]);
        assert_eq!(Position(0).line(&document), 0);
        assert_eq!(Position(1).line(&document), 1);
        assert_eq!(Position(2).line(&document), 1);
        assert_eq!(Position(3).line(&document), 1);
        assert_eq!(Position(4).line(&document), 1);
        assert_eq!(Position(5).line(&document), 1);
        assert_eq!(Position(6).line(&document), 1);
        assert_eq!(Position(500).line(&document), 1);

        let document = Document::new("\nabc\n".into());
        assert_eq!(document.lines, vec![1, 5]);
        assert_eq!(Position(0).line(&document), 0);
        assert_eq!(Position(1).line(&document), 1);
        assert_eq!(Position(2).line(&document), 1);
        assert_eq!(Position(3).line(&document), 1);
        assert_eq!(Position(4).line(&document), 1);
        assert_eq!(Position(5).line(&document), 2);
        assert_eq!(Position(6).line(&document), 2);
        assert_eq!(Position(7).line(&document), 2);
        assert_eq!(Position(500).line(&document), 2);
    }

    #[test]
    fn position_character() {
        let document = Document::new("abc\ndef".into());
        assert_eq!(document.lines, vec![4]);
        assert_eq!(Position(0).character(&document), 0);
        assert_eq!(Position(1).character(&document), 1);
        assert_eq!(Position(2).character(&document), 2);
        assert_eq!(Position(3).character(&document), 3);
        assert_eq!(Position(4).character(&document), 0);
        assert_eq!(Position(5).character(&document), 1);
        assert_eq!(Position(6).character(&document), 2);
        assert_eq!(Position(7).character(&document), 3);
        assert_eq!(Position(8).character(&document), 4);
        assert_eq!(Position(500).character(&document), 496);

        let document = Document::new("abc\n".into());
        assert_eq!(document.lines, vec![4]);
        assert_eq!(Position(0).character(&document), 0);
        assert_eq!(Position(1).character(&document), 1);
        assert_eq!(Position(2).character(&document), 2);
        assert_eq!(Position(3).character(&document), 3);
        assert_eq!(Position(4).character(&document), 0);
        assert_eq!(Position(5).character(&document), 1);
        assert_eq!(Position(6).character(&document), 2);
        assert_eq!(Position(7).character(&document), 3);
        assert_eq!(Position(8).character(&document), 4);
        assert_eq!(Position(500).character(&document), 496);

        assert_eq!("\u{0041}".len(), 1);
        assert_eq!("\u{00DF}".len(), 2);
        assert_eq!("\u{4E01}".len(), 3);
        assert_eq!("\u{1F701}".len(), 4);

        let document = Document::new("\u{0041}\u{00DF}\u{4E01}\u{1F701}".into());
        assert_eq!(document.lines, vec![]);
        assert_eq!(Position(0).character(&document), 0);
        assert_eq!(Position(1).character(&document), 1);
        assert_eq!(Position(2).character(&document), 2);
        assert_eq!(Position(3).character(&document), 2);
        assert_eq!(Position(4).character(&document), 3);
        assert_eq!(Position(5).character(&document), 3);
        assert_eq!(Position(6).character(&document), 3);
        assert_eq!(Position(7).character(&document), 4);
        assert_eq!(Position(8).character(&document), 4);
        assert_eq!(Position(9).character(&document), 4);
        assert_eq!(Position(10).character(&document), 5);
        assert_eq!(Position(11).character(&document), 6);
        assert_eq!(Position(12).character(&document), 7);
        assert_eq!(Position(13).character(&document), 8);
        assert_eq!(Position(14).character(&document), 9);
        assert_eq!(Position(500).character(&document), 495);
    }

    #[test]
    fn document_chars_end() {
        let document = Document::new("abc".into());
        let mut chars = document.chars();
        assert_eq!(chars.advance(), Some('a'));
        assert_eq!(chars.advance(), Some('b'));
        assert_eq!(chars.advance(), Some('c'));
        assert_eq!(chars.advance(), None);
        assert_eq!(chars.advance(), None);
        assert_eq!(chars.advance(), None);
    }

    #[test]
    fn document_chars_end_panic_lookahead() {
        let document = Document::new("abc".into());
        let mut chars = document.chars();
        assert_eq!(chars.lookahead(), Some('a'));
        assert_eq!(chars.lookahead(), Some('a'));
        assert_eq!(chars.advance(), Some('a'));
        assert_eq!(chars.lookahead(), Some('b'));
        assert_eq!(chars.lookahead(), Some('b'));
        assert_eq!(chars.advance(), Some('b'));
        assert_eq!(chars.lookahead(), Some('c'));
        assert_eq!(chars.lookahead(), Some('c'));
        assert_eq!(chars.advance(), Some('c'));
        assert_eq!(chars.lookahead(), None);
        assert_eq!(chars.lookahead(), None);
        assert_eq!(chars.advance(), None);
        assert_eq!(chars.lookahead(), None);
        assert_eq!(chars.lookahead(), None);
        assert_eq!(chars.advance(), None);
        assert_eq!(chars.lookahead(), None);
        assert_eq!(chars.lookahead(), None);
    }
}
