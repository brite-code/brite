//! Turns a stream of source characters into a stream of `Token`s. Tokens are a more useful unit for
//! parsing than characters since tokens will parse numbers, identifiers, glyphs, and will skip
//! comments and whitespace.

use super::identifier::{Identifier, Keyword};
use super::number::Number;
use super::position::{Chars, Position, Range};

/// A token in Brite source code is a range of text with some simple semantic meaning. When parsing
/// a source document we produce a list of tokens whose positions when added together should be the
/// full range of the document.
pub struct Token {
    /// The token’s full starting position. Including whitespace and comments.
    pub full_start: Position,
    /// The token’s actual range.
    pub range: Range,
    /// The description of the token.
    pub description: TokenDescription,
    /// Tokens may only be constructed from this module.
    _private: (),
}

/// The description of the token.
pub enum TokenDescription {
    /// Some sequence of characters that helps define a construct in our programming language.
    Glyph(Glyph),
    /// Any `Identifier`.
    Identifier(Identifier),
    /// Any `Number`.
    Number(Number),
    /// An error we encountered while tokenizing the program. Errors are not fatal so an error is
    /// represented as a token.
    Error(ErrorToken),
}

impl Token {
    /// Creates a new token.
    fn new(full_start: Position, range: Range, description: TokenDescription) -> Self {
        debug_assert!(full_start <= range.start());
        Token {
            full_start,
            range,
            description,
            _private: (),
        }
    }

    /// Creates a new glyph token.
    fn glyph(full_start: Position, range: Range, glyph: Glyph) -> Self {
        Self::new(full_start, range, TokenDescription::Glyph(glyph))
    }

    /// Creates a new error token.
    fn error(full_start: Position, range: Range, error: ErrorToken) -> Self {
        Self::new(full_start, range, TokenDescription::Error(error))
    }
}

/// A glyph is some symbol which is a part of Brite source code.
pub enum Glyph {
    /// Any `Keyword`.
    Keyword(Keyword),
    /// `/`
    Slash,
}

/// A token representing an error that occurred while tokenizing.
pub enum ErrorToken {
    /// We encountered an unexpected character while tokenizing.
    UnexpectedChar(char),
    /// We tried to parse a number, but the number’s format was invalid.
    InvalidNumber(String),
}

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
                                let range = Range::position(start);
                                Some(Token::glyph(full_start, range, Glyph::Slash))
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
                            let range = Range::new(start, end);
                            let description = match identifier {
                                Ok(identifier) => TokenDescription::Identifier(identifier),
                                Err(keyword) => TokenDescription::Glyph(Glyph::Keyword(keyword)),
                            };
                            return Some(Token::new(full_start, range, description));
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
                                let range = Range::new(start, end);
                                let error = ErrorToken::InvalidNumber(raw);
                                return Some(Token::error(full_start, range, error));
                            }

                            // Return the parsed number.
                            let end = self.chars.position();
                            let range = Range::new(start, end);
                            let description = match number {
                                Ok(number) => TokenDescription::Number(number),
                                Err(raw) => TokenDescription::Error(ErrorToken::InvalidNumber(raw)),
                            };
                            return Some(Token::new(full_start, range, description));
                        }

                        // Otherwise we have an unexpected character!
                        self.chars.next();
                        let range = Range::position(start);
                        Some(Token::error(
                            full_start,
                            range,
                            ErrorToken::UnexpectedChar(c),
                        ))
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
                self.prev_position == token.full_start,
                "End of the previous token should be equal to the start of the next token."
            );
            self.prev_position = token.range.end();
        }
        next_token
    }
}
