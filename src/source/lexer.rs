//! Turns a stream of source characters into a stream of `Token`s. Tokens are a more useful unit for
//! parsing than characters since tokens will parse numbers, identifiers, glyphs, and will skip
//! comments and whitespace.

use super::identifier::{Identifier, Keyword};
use super::number::Number;
use super::position::{Chars, Position, Range};

/// Turns an iterator of source characters into an iterator of source tokens.
#[cfg(not(debug_assertions))]
pub fn tokenize(chars: Chars<impl Iterator<Item = char>>) -> impl Iterator<Item = Token> {
    Lexer { chars }
}

/// Turns an iterator of source characters into an iterator of source tokens.
#[cfg(debug_assertions)]
pub fn tokenize(chars: Chars<impl Iterator<Item = char>>) -> impl Iterator<Item = Token> {
    let prev_position = chars.position();
    let lexer = Lexer { chars };
    DebugLexer {
        lexer,
        prev_position,
    }
}

/// A token in Brite source code is a range of text with some simple semantic meaning. When parsing
/// a source document we produce a list of tokens whose positions when added together should be the
/// full range of the document.
pub enum Token {
    Glyph(GlyphToken),
    Identifier(IdentifierToken),
    Number(NumberToken),
    Error(ErrorToken),
}

/// An owned enum for switching to a `Token` reference.
#[derive(Clone, Copy)]
pub enum TokenRef<'a> {
    Glyph(&'a GlyphToken),
    Identifier(&'a IdentifierToken),
    Number(&'a NumberToken),
    Error(&'a ErrorToken),
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
    fn new(full_start: Position, range: Range) -> Self {
        debug_assert!(full_start <= range.start());
        TokenRange { full_start, range }
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
    fn new(range: TokenRange, glyph: Glyph) -> Self {
        GlyphToken { range, glyph }
    }
}

/// Any `Identifier`.
pub struct IdentifierToken {
    range: TokenRange,
    identifier: Identifier,
}

impl IdentifierToken {
    fn new(range: TokenRange, identifier: Identifier) -> Self {
        IdentifierToken { range, identifier }
    }
}

/// Any `Number`.
pub struct NumberToken {
    range: TokenRange,
    number: Number,
}

impl NumberToken {
    fn new(range: TokenRange, number: Number) -> Self {
        NumberToken { range, number }
    }
}

/// An error we encountered while tokenizing the program. Errors are not fatal so an error is
/// represented as a token.
pub struct ErrorToken {
    range: TokenRange,
    description: ErrorTokenDescription,
}

/// A token representing an error that occurred while tokenizing.
enum ErrorTokenDescription {
    /// We encountered an unexpected character while tokenizing.
    UnexpectedChar { c: char },
    /// We tried to parse a number, but the number’s format was invalid.
    InvalidNumber { raw: String },
}

impl ErrorToken {
    fn new(range: TokenRange, description: ErrorTokenDescription) -> Self {
        ErrorToken { range, description }
    }

    fn unexpected_char(range: TokenRange, c: char) -> Self {
        Self::new(range, ErrorTokenDescription::UnexpectedChar { c })
    }

    fn invalid_number(range: TokenRange, raw: String) -> Self {
        Self::new(range, ErrorTokenDescription::InvalidNumber { raw })
    }
}

struct Lexer<I>
where
    I: Iterator<Item = char>,
{
    chars: Chars<I>,
}

impl<I> Iterator for Lexer<I>
where
    I: Iterator<Item = char>,
{
    type Item = Token;

    fn next(&mut self) -> Option<Token> {
        let full_start = self.chars.position();

        // Loop since we want to ignore whitespace and comments while still remembering our full
        // start position.
        loop {
            return match self.chars.peek() {
                None => return None,
                Some(c) => match c {
                    // Parse tokens that start with a slash.
                    '/' => {
                        let start = self.chars.position();
                        self.chars.next();
                        match self.chars.peek() {
                            // If we see a slash immediately following our last slash then we have
                            // a line comment! Ignore all characters until the end of the line.
                            Some('/') => {
                                self.chars.next();
                                // Ignore all characters until we find a newline.
                                loop {
                                    match self.chars.peek() {
                                        Some('\n') | Some('\r') => {
                                            self.chars.next();
                                            break;
                                        }
                                        _ => {}
                                    }
                                    self.chars.next();
                                }
                                // We’re done with the line comment so continue...
                                continue;
                            }

                            // If we see a star immediately following our last slash then we have a
                            // block comment! Ignore all characters until the block comment closes.
                            Some('*') => {
                                self.chars.next();
                                // Ignore all characters until we find a block comment end.
                                loop {
                                    if let Some('*') = self.chars.next() {
                                        if let Some('/') = self.chars.peek() {
                                            self.chars.next();
                                            break;
                                        }
                                    }
                                }
                                // We’re done with the block comment so continue...
                                continue;
                            }

                            // Otherwise, we have a slash glyph.
                            _ => {
                                let range = TokenRange::new(full_start, Range::position(start));
                                Some(GlyphToken::new(range, Glyph::Slash).into())
                            }
                        }
                    }

                    c => {
                        // Ignore whitespace...
                        if c.is_whitespace() {
                            self.chars.next();
                            continue;
                        }

                        // Record the current position.
                        let start = self.chars.position();

                        // If we could parse an identifier...
                        if let Some(identifier) = Identifier::parse(&mut self.chars) {
                            let end = self.chars.position();
                            let range = TokenRange::new(full_start, Range::new(start, end));
                            return match identifier {
                                Ok(identifier) => {
                                    Some(IdentifierToken::new(range, identifier).into())
                                }
                                Err(keyword) => {
                                    Some(GlyphToken::new(range, Glyph::Keyword(keyword)).into())
                                }
                            };
                        }

                        // If we could parse a number...
                        //
                        // We also want to error if numbers are followed immediately by an
                        // identifier without a space. For example, we want `4px` to be a
                        // syntax error.
                        if let Some(number) = Number::parse(&mut self.chars) {
                            /// Is the current character an identifier continuation? Lifted into a
                            /// function since it’s a bit long when written inline.
                            fn peek_identifier_continue(
                                lexer: &mut Lexer<impl Iterator<Item = char>>,
                            ) -> bool {
                                lexer
                                    .chars
                                    .peek()
                                    .map(Identifier::is_continue)
                                    .unwrap_or(false)
                            }

                            // If our number is immediately followed by an identifier or a number
                            // then we have an invalid number token. Collect all the identifier or
                            // number tokens before returning the error token.
                            if peek_identifier_continue(self) {
                                let mut raw = match number {
                                    Ok(number) => number.into_raw(),
                                    Err(raw) => raw,
                                };
                                while peek_identifier_continue(self) {
                                    raw.push(self.chars.next().unwrap());
                                }
                                let end = self.chars.position();
                                let range = TokenRange::new(full_start, Range::new(start, end));
                                return Some(ErrorToken::invalid_number(range, raw).into());
                            }

                            // Return the parsed number.
                            let end = self.chars.position();
                            let range = TokenRange::new(full_start, Range::new(start, end));
                            return match number {
                                Ok(number) => Some(NumberToken::new(range, number).into()),
                                Err(raw) => Some(ErrorToken::invalid_number(range, raw).into()),
                            };
                        }

                        // Otherwise we have an unexpected character!
                        self.chars.next();
                        let range = TokenRange::new(full_start, Range::position(start));
                        Some(ErrorToken::unexpected_char(range, c).into())
                    }
                },
            };
        }
    }
}

/// A wrapper around `Lexer` which performs some assertions for debugging purposes.
struct DebugLexer<I>
where
    I: Iterator<Item = char>,
{
    lexer: Lexer<I>,
    prev_position: Position,
}

impl<I> Iterator for DebugLexer<I>
where
    I: Iterator<Item = char>,
{
    type Item = Token;

    fn next(&mut self) -> Option<Token> {
        let next_token = self.lexer.next();
        if let Some(token) = &next_token {
            assert!(
                self.prev_position == token.range().full_start,
                "End of the previous token should be equal to the start of the next token."
            );
            self.prev_position = token.range().range.end();
        }
        next_token
    }
}

impl Token {
    fn range(&self) -> &TokenRange {
        match self {
            Token::Glyph(t) => &t.range,
            Token::Identifier(t) => &t.range,
            Token::Number(t) => &t.range,
            Token::Error(t) => &t.range,
        }
    }
}

impl<'a> TokenRef<'a> {
    fn range(self) -> &'a TokenRange {
        match self {
            TokenRef::Glyph(t) => &t.range,
            TokenRef::Identifier(t) => &t.range,
            TokenRef::Number(t) => &t.range,
            TokenRef::Error(t) => &t.range,
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

impl<'a> Into<TokenRef<'a>> for &'a GlyphToken {
    fn into(self) -> TokenRef<'a> {
        TokenRef::Glyph(self)
    }
}

impl<'a> Into<TokenRef<'a>> for &'a IdentifierToken {
    fn into(self) -> TokenRef<'a> {
        TokenRef::Identifier(self)
    }
}

impl<'a> Into<TokenRef<'a>> for &'a NumberToken {
    fn into(self) -> TokenRef<'a> {
        TokenRef::Number(self)
    }
}

impl<'a> Into<TokenRef<'a>> for &'a ErrorToken {
    fn into(self) -> TokenRef<'a> {
        TokenRef::Error(self)
    }
}
