use std::iter::Peekable;
use std::str::Chars;

/// A Brite source code document. Source code is represented as text and turned into an AST through
/// the process of lexing and parsing. This object contains metadata about a source document
/// including the document’s URI, the full document text, and information for determining the
/// placement of source positions.
pub struct Document {
    /// The document’s contents as a UTF-8 string.
    source: String,
    /// Locations in our document where new lines begin. New lines are created by `\n`, `\r\n`,
    /// and `\r`.
    lines: Vec<Position>,
}

impl Document {
    /// Creates a new source code document.
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
                    lines.push(Position(i as u32));
                } else if byte == b'\r' {
                    // If the next byte is `\n` then skip it. We only want to add a single line for
                    // the sequence `\r\n`.
                    if i + 1 < bytes.len() && bytes[i] == b'\n' {
                        i += 1;
                    }
                    lines.push(Position(i as u32));
                }
            }
            lines
        };
        // Return the created document.
        Document { source, lines }
    }

    /// The position our document starts at.
    pub fn start(&self) -> Position {
        Position(0)
    }

    /// The position our document ends at.
    pub fn end(&self) -> Position {
        Position(self.source.len() as u32)
    }

    /// An iterator over the characters of the document which also keeps track of the
    /// current position.
    pub fn chars<'a>(&'a self) -> DocumentChars<'a> {
        DocumentChars {
            chars: self.source.chars().peekable(),
            position: 0,
        }
    }
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
/// We need to keep this small as an AST will contain a _lot_ of positions. Currently a 32 bit
/// unsigned integer which represents the _byte_ offset into the source document.
///
/// [1]: https://microsoft.github.io/language-server-protocol/specification
#[derive(Copy, Clone, Eq, Ord, PartialEq, PartialOrd)]
pub struct Position(u32);

impl Position {
    /// The index of the position in a UTF-8 string.
    pub fn utf8_index(&self) -> u32 {
        self.0
    }

    /// Gets the zero-based line number of this position in the provided document. A new line is
    /// created by `\n`, `\r\n`, or `\r`.
    pub fn line(&self, document: &Document) -> usize {
        match document.lines.binary_search(self) {
            Ok(line) => line + 1,
            Err(line) => line,
        }
    }

    /// Gets the zero-based character number of this position in the provided document. As per the
    /// [LSP][1] specification, character offsets are measured in UTF-16 code units.
    ///
    /// [1]: https://microsoft.github.io/language-server-protocol/specification
    pub fn character(&self, document: &Document) -> usize {
        let bytes = document.source.as_bytes();
        let line = self.line(document);
        let start = if line == 0 {
            0
        } else {
            document.lines[line - 1].0 as usize
        };
        let end = self.0 as usize;
        if start >= bytes.len() {
            end - bytes.len()
        } else if end > bytes.len() {
            let line_bytes = &document.source.as_bytes()[start..];
            (end - bytes.len()) + String::from_utf8_lossy(line_bytes)
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
}

/// A range in a text document expressed as start and end positions. A range is comparable to a
/// selection in an editor. Therefore the end position is exclusive.
///
/// We need to keep this small as an AST will contain a _lot_ of ranges. Currently 64 bits.
#[derive(Clone, Copy, Eq, PartialEq)]
pub struct Range {
    /// The range’s start position.
    start: Position,
    /// The number of characters covered by this range.
    length: u32,
}

impl Range {
    /// Creates a range between two positions.
    pub fn between(start: Position, end: Position) -> Self {
        if start <= end {
            let length = end.0 - start.0;
            Range { start, length }
        } else {
            let (start, end) = (end, start);
            let length = end.0 - start.0;
            Range { start, length }
        }
    }

    /// Returns the start position of our range.
    pub fn start(&self) -> Position {
        self.start
    }

    /// Returns the end position of our range. Will always be greater than or equal to the start
    /// position. Remember that range is not inclusive.
    pub fn end(&self) -> Position {
        Position(self.start.0 + self.length)
    }
}

/// An iterator over the characters of a document. Also keeps track of the current `Position` in
/// the document.
pub struct DocumentChars<'a> {
    chars: Peekable<Chars<'a>>,
    position: u32,
}

impl<'a> DocumentChars<'a> {
    /// Gets the current position of the `DocumentChars` iterator.
    pub fn position(&self) -> Position {
        Position(self.position)
    }

    /// Peek at the next character without advancing the iterator.
    pub fn peek(&mut self) -> Option<char> {
        self.chars.peek().cloned()
    }
}

impl<'a> Iterator for DocumentChars<'a> {
    type Item = char;

    fn next(&mut self) -> Option<char> {
        let next = self.chars.next();
        if let Some(c) = next {
            self.position += c.len_utf8() as u32;
        }
        next
    }
}
