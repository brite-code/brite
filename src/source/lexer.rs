//! Turns a stream of source characters into a stream of `Token`s. Tokens are a more useful unit for
//! parsing than characters since tokens will parse numbers, identifiers, glyphs, and will skip
//! comments and whitespace.

use super::identifier::Identifier;
use super::number::Number;
use super::position::{Chars, Position, Range};
use super::token::*;

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
                self.prev_position == token.range().full_start(),
                "End of the previous token should be equal to the start of the next token."
            );
            self.prev_position = token.range().end();
        }
        next_token
    }
}
