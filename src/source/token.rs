use super::document::{Position, Range};
use super::identifier::{Identifier, Keyword};
use super::number::Number;

/// A token in Brite source code is a range of text with some simple semantic meaning. When parsing
/// a source document we produce a list of tokens whose positions when added together should be the
/// full range of the document.
pub enum Token {
    Glyph(GlyphToken),
    Identifier(IdentifierToken),
    Number(NumberToken),
    Error(ErrorToken),
}

/// The range covered by a token. Every token has two start positions. The “full start” position
/// and the actual start position.
///
/// The actual start position is the true start of the token. This is where the significant
/// characters in the token begin. The “full start” position includes whitespace and comments.
pub struct TokenRange {
    /// The token’s full starting position. Including whitespace and comments.
    full_start: Position,
    /// The token’s actual range.
    range: Range,
}

impl TokenRange {
    /// Creates a new token range.
    pub fn new(full_start: Position, range: Range) -> Self {
        debug_assert!(full_start <= range.start());
        TokenRange { full_start, range }
    }

    /// Gets the full start of this token range.
    pub fn full_start(&self) -> Position {
        self.full_start
    }

    /// Gets the start of this token range.
    pub fn start(&self) -> Position {
        self.range.start()
    }

    /// Gets the end of this token range.
    pub fn end(&self) -> Position {
        self.range.end()
    }
}

/// Some sequence of characters that helps define a construct in our programming language.
pub struct GlyphToken {
    range: TokenRange,
    glyph: Glyph,
}

/// A glyph is some symbol which is a part of Brite source code.
pub enum Glyph {
    /// Any `Keyword`.
    Keyword(Keyword),
    /// `/`
    Slash,
}

impl GlyphToken {
    pub fn new(range: TokenRange, glyph: Glyph) -> Self {
        GlyphToken { range, glyph }
    }
}

/// Any `Identifier`.
pub struct IdentifierToken {
    range: TokenRange,
    identifier: Identifier,
}

impl IdentifierToken {
    pub fn new(range: TokenRange, identifier: Identifier) -> Self {
        IdentifierToken { range, identifier }
    }
}

/// Any `Number`.
pub struct NumberToken {
    range: TokenRange,
    number: Number,
}

impl NumberToken {
    pub fn new(range: TokenRange, number: Number) -> Self {
        NumberToken { range, number }
    }
}

/// An error we encountered while tokenizing the program. Errors are not fatal so an error is
/// represented as a token.
///
/// Instead of holding a diagnostic identifier, we hold information needed to convert this error
/// token into a diagnostic. It is difficult to perform a side-effect in our lexer as it returns
/// an iterator. So our parser converts error tokens into diagnostics.
pub struct ErrorToken {
    range: TokenRange,
    description: ErrorTokenDescription,
}

/// A token representing an error that occurred while tokenizing.
enum ErrorTokenDescription {
    /// We encountered an unexpected character while tokenizing.
    UnexpectedChar { unexpected: char },
    /// We tried to parse a number, but the number’s format was invalid.
    InvalidNumber { invalid: String },
}

impl ErrorToken {
    fn new(range: TokenRange, description: ErrorTokenDescription) -> Self {
        ErrorToken { range, description }
    }

    pub fn unexpected_char(range: TokenRange, unexpected: char) -> Self {
        Self::new(range, ErrorTokenDescription::UnexpectedChar { unexpected })
    }

    pub fn invalid_number(range: TokenRange, invalid: String) -> Self {
        Self::new(range, ErrorTokenDescription::InvalidNumber { invalid })
    }
}

impl Token {
    pub fn range(&self) -> &TokenRange {
        match self {
            Token::Glyph(t) => &t.range,
            Token::Identifier(t) => &t.range,
            Token::Number(t) => &t.range,
            Token::Error(t) => &t.range,
        }
    }
}

impl Into<Token> for GlyphToken {
    fn into(self) -> Token {
        Token::Glyph(self)
    }
}

impl Into<Token> for IdentifierToken {
    fn into(self) -> Token {
        Token::Identifier(self)
    }
}

impl Into<Token> for NumberToken {
    fn into(self) -> Token {
        Token::Number(self)
    }
}

impl Into<Token> for ErrorToken {
    fn into(self) -> Token {
        Token::Error(self)
    }
}
