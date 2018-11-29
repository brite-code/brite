use super::document::{Position, Range};
use super::identifier::{Identifier, Keyword};
use super::number::Number;
use crate::diagnostics::DiagnosticRef;

/// A token in Brite source code is a range of text with some simple semantic meaning. When parsing
/// a source document we produce a list of tokens whose positions when added together should be the
/// full range of the document.
#[derive(Clone, Debug, PartialEq)]
pub enum Token {
    Glyph(GlyphToken),
    Identifier(IdentifierToken),
    Number(NumberToken),
    Error(ErrorToken),
    End(EndToken),
}

/// The range covered by a token. Every token has two start positions. The “full start” position
/// and the actual start position.
///
/// The actual start position is the true start of the token. This is where the significant
/// characters in the token begin. The “full start” position includes whitespace and comments.
#[derive(Clone, Debug, PartialEq)]
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

    /// Gets the actual range of our token. Excluding the full start.
    pub fn range(&self) -> Range {
        self.range()
    }
}

/// Some sequence of characters that helps define a construct in our programming language.
#[derive(Clone, Debug, PartialEq)]
pub struct GlyphToken {
    range: TokenRange,
    glyph: Glyph,
}

/// A glyph is some symbol which is a part of Brite source code.
#[derive(Clone, Debug, PartialEq)]
pub enum Glyph {
    /// Any `Keyword`.
    Keyword(Keyword),
    /// `;`
    Semicolon,
    /// `/`
    Slash,
}

impl GlyphToken {
    pub fn new(range: TokenRange, glyph: Glyph) -> Self {
        GlyphToken { range, glyph }
    }

    pub fn keyword(range: TokenRange, keyword: Keyword) -> Self {
        Self::new(range, Glyph::Keyword(keyword))
    }
}

/// Any `Identifier`.
#[derive(Clone, Debug, PartialEq)]
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
#[derive(Clone, Debug, PartialEq)]
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
#[derive(Clone, Debug, PartialEq)]
pub struct ErrorToken {
    range: TokenRange,
    diagnostic: DiagnosticRef,
}

impl ErrorToken {
    pub fn new(range: TokenRange, diagnostic: DiagnosticRef) -> Self {
        ErrorToken { range, diagnostic }
    }
}

/// The last token in the document. Once our lexer returns `EndToken` it will continue returning
/// `EndToken` forever.
#[derive(Clone, Debug, PartialEq)]
pub struct EndToken {
    range: TokenRange,
}

impl EndToken {
    pub fn new(range: TokenRange) -> Self {
        EndToken { range }
    }
}

impl Token {
    pub fn is_identifier(&self) -> bool {
        match self {
            Token::Identifier(_) => true,
            _ => false,
        }
    }

    pub fn is_end(&self) -> bool {
        match self {
            Token::End(_) => true,
            _ => false,
        }
    }

    pub fn full_range(&self) -> &TokenRange {
        match self {
            Token::Glyph(t) => &t.range,
            Token::Identifier(t) => &t.range,
            Token::Number(t) => &t.range,
            Token::Error(t) => &t.range,
            Token::End(t) => &t.range,
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

impl Into<Token> for EndToken {
    fn into(self) -> Token {
        Token::End(self)
    }
}
