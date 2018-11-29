//! Turns a stream of source characters into a stream of `Token`s. Tokens are a more useful unit for
//! parsing than characters since tokens will parse numbers, identifiers, glyphs, and will skip
//! comments and whitespace.

use super::document::{Position, Range};
use super::identifier::Identifier;
use super::number::Number;
use super::token::*;
use std::iter::Peekable;

/// A lexer turns an iterator of source document characters into an iterator of tokens.
pub struct Lexer<I>
where
    I: Iterator<Item = (Position, char)>,
{
    chars: Peekable<I>,
}

impl<I> Lexer<I>
where
    I: Iterator<Item = (Position, char)>,
{
    /// Create a new lexer.
    pub fn new(chars: I) -> Self {
        let chars = chars.peekable();
        Lexer { chars }
    }
}

impl<I> Iterator for Lexer<I>
where
    I: Iterator<Item = (Position, char)>,
{
    type Item = Token;

    fn next(&mut self) -> Option<Token> {
        // The full start of our token including trivia like whitespace and comments.
        let full_start = match self.chars.peek() {
            None => return None,
            Some((full_start, _)) => *full_start,
        };

        // Loop since we want to ignore whitespace and comments while still remembering our full
        // start position.
        loop {
            return match self.chars.peek() {
                None => return None,
                Some(c) => match c {
                    // Parse tokens that start with a slash.
                    (start, '/') => {
                        let start = *start;
                        self.chars.next();
                        match self.chars.peek() {
                            // If we see a slash immediately following our last slash then we have
                            // a line comment! Ignore all characters until the end of the line.
                            Some((_, '/')) => {
                                self.chars.next();
                                // Ignore all characters until we find a newline. A newline is one
                                // of `\n`, `\r\n`, or `\r`.
                                loop {
                                    match self.chars.peek() {
                                        Some((_, '\n')) => {
                                            self.chars.next();
                                            break;
                                        }
                                        Some((_, '\r')) => {
                                            if let Some((_, '\n')) = self.chars.peek() {
                                                self.chars.next();
                                            }
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
                            Some((_, '*')) => {
                                self.chars.next();
                                // Ignore all characters until we find a block comment end.
                                loop {
                                    if let Some((_, '*')) = self.chars.next() {
                                        if let Some((_, '/')) = self.chars.peek() {
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
                                let range = TokenRange::new(full_start, Range::new(start, 1));
                                Some(GlyphToken::new(range, Glyph::Slash).into())
                            }
                        }
                    }

                    (start, c) => {
                        let (start, c) = (*start, *c);

                        // Ignore whitespace...
                        if c.is_whitespace() {
                            self.chars.next();
                            continue;
                        }

                        // If we could parse an identifier...
                        if let Some(identifier) = Identifier::parse(&mut self.chars) {
                            return match identifier {
                                Ok(identifier) => {
                                    let range = Range::new(start, identifier.len() as u32);
                                    let range = TokenRange::new(full_start, range);
                                    Some(IdentifierToken::new(range, identifier).into())
                                }
                                Err(keyword) => {
                                    let range = Range::new(start, keyword.len() as u32);
                                    let range = TokenRange::new(full_start, range);
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
                                lexer: &mut Lexer<impl Iterator<Item = (Position, char)>>,
                            ) -> bool {
                                lexer
                                    .chars
                                    .peek()
                                    .map(|(_, c)| Identifier::is_continue(*c))
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
                                    raw.push(self.chars.next().unwrap().1);
                                }
                                let range = Range::new(start, raw.len() as u32);
                                let range = TokenRange::new(full_start, range);
                                return Some(ErrorToken::invalid_number(range, raw).into());
                            }

                            // Return the parsed number.
                            return match number {
                                Ok(number) => {
                                    let range = Range::new(start, number.raw().len() as u32);
                                    let range = TokenRange::new(full_start, range);
                                    Some(NumberToken::new(range, number).into())
                                }
                                Err(raw) => {
                                    let range = Range::new(start, raw.len() as u32);
                                    let range = TokenRange::new(full_start, range);
                                    Some(ErrorToken::invalid_number(range, raw).into())
                                }
                            };
                        }

                        // Otherwise we have an unexpected character!
                        self.chars.next();
                        let range = Range::new(start, c.len_utf8() as u32);
                        let range = TokenRange::new(full_start, range);
                        Some(ErrorToken::unexpected_char(range, c).into())
                    }
                },
            };
        }
    }
}
