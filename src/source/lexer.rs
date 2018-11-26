use super::identifier::{Identifier, Keyword};
use super::number::Number;
use super::position::{Chars, Position, Range};

/// A token in Brite source code is a range of text with some simple semantic meaning. When parsing
/// a source document we produce a list of tokens whose positions when added together should be the
/// full range of the document.
pub struct Token {
    /// The token’s full starting position. Including whitespace and comments.
    full_start: Position,
    /// The token’s actual range.
    range: Range,
    /// The description of the token.
    description: TokenDescription,
}

impl Token {
    /// Creates a new token.
    fn new(full_start: Position, range: Range, description: TokenDescription) -> Self {
        debug_assert!(full_start <= range.start());
        Token {
            full_start,
            range,
            description,
        }
    }
}

enum TokenDescription {
    Glyph(Glyph),
    Identifier(Identifier),
    Number(Number),
    UnexpectedChar(char),
}

/// A glyph is some symbol which is a part of Brite source code.
pub enum Glyph {
    /// Any `Keyword`.
    Keyword(Keyword),
    /// `/`
    Slash,
}

/// Turns an iterator of source characters into an iterator of source tokens.
pub fn lex<I>(iter: Chars<I>) -> impl Iterator<Item = Token>
where
    I: Iterator<Item = char>,
{
    Lexer { iter }
}

struct Lexer<I>
where
    I: Iterator<Item = char>,
{
    iter: Chars<I>,
}

impl<I> Iterator for Lexer<I>
where
    I: Iterator<Item = char>,
{
    type Item = Token;

    fn next(&mut self) -> Option<Token> {
        let full_start = self.iter.position();

        // Loop since we want to ignore whitespace and comments while still remembering our full
        // start position.
        loop {
            return match self.iter.peek() {
                None => return None,
                Some(c) => match c {
                    // Parse tokens that start with a slash.
                    '/' => {
                        let start = self.next_position();
                        match self.iter.peek() {
                            // If we see a slash immediately following our last slash then we have
                            // a line comment! Ignore all characters until the end of the line.
                            Some('/') => {
                                self.iter.next();
                                // Ignore all characters until we find a newline.
                                loop {
                                    match self.iter.peek() {
                                        Some('\n') | Some('\r') => {
                                            self.iter.next();
                                            break;
                                        }
                                        _ => {}
                                    }
                                    self.iter.next();
                                }
                                // We’re done with the line comment so continue...
                                continue;
                            }

                            // If we see a star immediately following our last slash then we have a
                            // block comment! Ignore all characters until the block comment closes.
                            Some('*') => {
                                self.iter.next();
                                // Ignore all characters until we find a block comment end.
                                loop {
                                    if let Some('*') = self.iter.next() {
                                        if let Some('/') = self.iter.peek() {
                                            self.iter.next();
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

                    // Ignore whitespace...
                    c if c.is_whitespace() => {
                        self.iter.next();
                        continue;
                    }

                    // Unexpected character!
                    c => {
                        let start = self.iter.position();
                        if let Some(identifier) = Identifier::parse(&mut self.iter) {
                            // If we could parse an identifier...
                            let end = self.iter.position();
                            let range = Range::new(start, end);
                            match identifier {
                                Ok(identifier) => {
                                    Some(Token::identifier(full_start, range, identifier))
                                }
                                Err(keyword) => {
                                    Some(Token::glyph(full_start, range, Glyph::Keyword(keyword)))
                                }
                            }
                        } else if let Some(number) = Number::parse(&mut self.iter) {
                            // If we could parse a number...
                            let end = self.iter.position();
                            let range = Range::new(start, end);
                            unimplemented!()
                        } else {
                            // Otherwise we have an unexpected character!
                            let range = Range::position(self.next_position());
                            Some(Token::unexpected_char(full_start, range, c))
                        }
                    }
                },
            };
        }
    }
}

impl<I> Lexer<I>
where
    I: Iterator<Item = char>,
{
    /// Advances the iterator and returns the position before advancing.
    fn next_position(&mut self) -> Position {
        let position = self.iter.position();
        self.iter.next();
        position
    }
}
