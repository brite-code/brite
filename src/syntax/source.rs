use num::BigInt;
use unicode_xid::UnicodeXID;

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

/// References slices of the source string we parsed the token from.
pub struct Token<'src> {
    leading_trivia: Vec<Trivia<'src>>,
    trailing_trivia: Vec<Trivia<'src>>,
    kind: TokenKind,
}

/// The kind of a token.
enum TokenKind {
    /// A glyph represents some constant sequence of characters that is used in Brite syntax.
    Glyph(Glyph),
    /// A name in the program.
    Identifier(Identifier),
    /// Some number written in the program.
    Number(Number),
    /// An unexpected character in the program.
    UnexpectedChar(char),
}

/// A glyph represents some constant sequence of characters that is used in Brite syntax.
#[derive(Clone, Copy)]
enum Glyph {
    /// A reserved identifier.
    Keyword(Keyword),
    /// `&`
    Ampersand,
    /// `&&`
    AmpersandDouble,
    /// `->`
    Arrow,
    /// `*`
    Asterisk,
    /// `!`
    Bang,
    /// `|`
    Bar,
    /// `||`
    BarDouble,
    /// `{`
    BraceLeft,
    /// `}`
    BraceRight,
    /// `[`
    BracketLeft,
    /// `]`
    BracketRight,
    /// `^`
    Caret,
    /// `,`
    Comma,
    /// `.`
    Dot,
    /// `=`
    Equals,
    /// `==`
    EqualsDouble,
    /// `!=`
    EqualsNot,
    /// `>`
    GreaterThan,
    /// `>=`
    GreaterThanOrEqual,
    /// `<`
    LessThan,
    /// `<=`
    LessThanOrEqual,
    /// `-`
    Minus,
    /// `(`
    ParenLeft,
    /// `)`
    ParenRight,
    /// `%`
    Percent,
    /// `+`
    Plus,
    /// `;`
    Semicolon,
    /// `/`
    Slash,
}

/// A reserved identifier.
#[derive(Clone, Copy)]
enum Keyword {
    /// `_`
    Hole,
    /// `true`
    True,
    /// `false`
    False,
}

/// A name written in a Brite program. Brite identifiers follow the [Unicode Identifier
/// Specification][1] including the optional underscore (`_`) character.
///
/// Some strings which are valid identifier syntax are reserved as keywords to disambiguate parsing.
/// We try to reserve the minimum number of keywords possible.
///
/// We could only have keywords in certain positions. For instance, only reserve the keyword `fun`
/// when in an expression context. However, this introduces a potentially confusing rule. It also
/// means, in this example, code transformations could not easily make expression identifiers out of
/// pattern identifiers.
///
/// [1]: http://www.unicode.org/reports/tr31
struct Identifier(
    // TODO: Intern identifier strings.
    String,
);

impl Identifier {
    /// Does this start an identifier?
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

    /// Is this a continuation of an identifier?
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

/// A number in Brite source code could be written in a few different ways:
///
/// - Integer: `42`
/// - Binary integer: `0b1101`
/// - Hexadecimal integer: `0xFFF`
/// - Floating point: `3.1415`, `1e2`
///
/// For floating point numbers we use the same syntax as the [JSON specification][1] with the one
/// modification that we permit leading zeroes.
///
/// We never parse a negative sign as part of our number syntax. Instead we use a negative operator
/// in our language syntax.
///
/// [1]: http://www.ecma-international.org/publications/files/ECMA-ST/ECMA-404.pdf
enum Number {
    /// `42`
    ///
    /// A base 10 integer. We have the raw text representation of the integer in addition to
    /// its value.
    DecimalInteger(BigInt),
    /// `0b1101`
    ///
    /// An integer written in binary form. The boolean is true if the “b” after `0` was lowercase.
    /// Then we have the integer’s raw text form and value.
    BinaryInteger(BigInt),
    /// `0xFFF`
    ///
    /// An integer written in hexadecimal form. The boolean is true if the “x” after `0` was lowercase.
    /// Then we have the integer’s raw text form and value.
    HexadecimalInteger(BigInt),
    /// `3.1415`, `1e2`
    ///
    /// A 64-bit floating point number. We aim for our floating point syntax to be compatible with the
    /// [JSON specification][1].
    ///
    /// [1]: http://www.ecma-international.org/publications/files/ECMA-ST/ECMA-404.pdf
    Float(f64),
}

/// Pieces of Brite syntax which (usually) don’t affect program behavior. Like comments or spaces.
enum Trivia<'src> {
    /// Contiguous space characters (` `).
    Spaces(usize),
    /// Contiguous tab characters (`\t`). The Brite formatter prefers spaces to tabs, but since tabs
    /// are common enough we’ll add a special case for them in `Trivia`.
    Tabs(usize),
    /// Contiguous newlines. The Brite formatter prefers line feeds (`\n`) to other forms
    /// of newlines.
    Newlines(Newline, usize),
    /// Some comment about the source code.
    Comment(Comment<'src>),
    /// Other whitespace characters which we optimize such as obscure Unicode whitespace
    /// characters like U+00A0.
    OtherWhitespace(char),
}

/// Supported newline sequences.
enum Newline {
    /// `\n`
    LF,
    /// `\r`
    CR,
    /// `\r\n`
    CRLF,
}

enum Comment<'src> {
    /// `// ...` does not include the newline that ends the comment. Does not include the
    /// `//` characters.
    Line(&'src str),
    /// `/* ... */` does include the `/*` and `*/` characters.
    Block(&'src str),
}
