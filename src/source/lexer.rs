//! Turns a stream of source characters into a stream of `Token`s. Tokens are a more useful unit for
//! parsing than characters since tokens will parse numbers, identifiers, glyphs, and will skip
//! comments and whitespace.

use super::document::{Document, DocumentChars, Range};
use super::identifier::Identifier;
use super::number::Number;
use super::token::*;
use crate::diagnostics::{Diagnostic, DiagnosticSet};
use std::cell::RefCell;

/// A lexer turns an iterator of source document characters into an iterator of tokens.
pub struct Lexer<'a> {
    diagnostics: &'a RefCell<DiagnosticSet>,
    chars: DocumentChars<'a>,
    lookahead: Option<Token>,
}

impl<'a> Lexer<'a> {
    /// Create a new lexer.
    pub fn new(diagnostics: &'a RefCell<DiagnosticSet>, document: &'a Document) -> Self {
        Lexer {
            diagnostics,
            chars: document.chars(),
            lookahead: None,
        }
    }

    /// Gets a list of all the tokens in a document along with the diagnostics reported while
    /// tokenizing the document.
    pub fn tokens(document: &'a Document) -> (DiagnosticSet, Vec<Token>) {
        // Construct everything we’ll need for tokenization.
        let diagnostics = RefCell::new(DiagnosticSet::new());
        let mut lexer = Lexer::new(&diagnostics, document);
        let mut tokens = Vec::new();
        // Advance through the lexer pushing every token. Once we reach the `EndToken` we push that
        // to our list and break out of the loop.
        loop {
            match lexer.advance() {
                token @ Token::End(_) => {
                    tokens.push(token);
                    break;
                }
                token => tokens.push(token),
            }
        }
        // Return our diagnostics and tokens.
        let diagnostics = diagnostics.into_inner();
        (diagnostics, tokens)
    }

    /// Consume the next token in the lexer. Calling `Lexer::advance()` again will return a
    /// new token. Eventually we will return an `EndToken`. When that happens subsequent calls to
    /// `Lexer::advance()` will only ever return an `EndToken`.
    #[inline]
    pub fn advance(&mut self) -> Token {
        match self.lookahead.take() {
            Some(token) => token,
            None => {
                let position = self.chars.position();
                let token = self.actually_advance();
                debug_assert_eq!(position, token.full_range().full_start());
                token
            }
        }
    }

    /// Look at the next token without consuming the token and advancing the lexer. Calling
    /// `Lexer::lookahead()` again will return the same token.
    #[inline]
    pub fn lookahead(&mut self) -> &Token {
        if self.lookahead.is_none() {
            self.lookahead = Some(self.advance());
        }
        match &self.lookahead {
            Some(token) => token,
            None => unreachable!(),
        }
    }

    /// Actually advances the lexer. The `Lexer::advance()` function performs some
    /// housekeeping work only.
    fn actually_advance(&mut self) -> Token {
        // The full start of our token including trivia like whitespace and comments.
        let full_start = self.chars.position();

        // Loop since we want to ignore whitespace and comments while still remembering our full
        // start position.
        loop {
            return match self.chars.lookahead() {
                Some(c) => match c {
                    ';' => {
                        let start = self.chars.position();
                        self.chars.advance();
                        let range = TokenRange::new(full_start, Range::new(start, 1));
                        GlyphToken::new(range, Glyph::Semicolon).into()
                    }

                    // Parse tokens that start with a slash.
                    '/' => {
                        let start = self.chars.position();
                        self.chars.advance();
                        match self.chars.lookahead() {
                            // If we see a slash immediately following our last slash then we have
                            // a line comment! Ignore all characters until the end of the line.
                            Some('/') => {
                                self.chars.advance();
                                // Ignore all characters until we find a newline. A newline is one
                                // of `\n`, `\r\n`, or `\r`.
                                loop {
                                    match self.chars.lookahead() {
                                        Some('\n') => {
                                            self.chars.advance();
                                            break;
                                        }
                                        Some('\r') => {
                                            self.chars.advance();
                                            self.chars.advance_char('\n');
                                            break;
                                        }
                                        _ => {
                                            self.chars.advance();
                                        }
                                    }
                                }
                                // We’re done with the line comment so continue...
                                continue;
                            }

                            // If we see a star immediately following our last slash then we have a
                            // block comment! Ignore all characters until the block comment closes.
                            Some('*') => {
                                self.chars.advance();
                                // Ignore all characters until we find a block comment end.
                                loop {
                                    // TODO: Test closing comments with `**/`.
                                    if self.chars.advance_char('*') {
                                        if self.chars.advance_char('/') {
                                            break;
                                        }
                                    } else {
                                        self.chars.advance();
                                    }
                                }
                                // We’re done with the block comment so continue...
                                continue;
                            }

                            // Otherwise, we have a slash glyph.
                            _ => {
                                let range = TokenRange::new(full_start, Range::new(start, 1));
                                GlyphToken::new(range, Glyph::Slash).into()
                            }
                        }
                    }

                    c => {
                        // Ignore whitespace...
                        if c.is_whitespace() {
                            self.chars.advance();
                            continue;
                        }

                        let start = self.chars.position();

                        // If we could parse an identifier...
                        if let Some(identifier) = Identifier::parse(&mut self.chars) {
                            let end = self.chars.position();
                            let range = TokenRange::new(full_start, Range::between(start, end));
                            return match identifier {
                                Ok(identifier) => IdentifierToken::new(range, identifier).into(),
                                Err(keyword) => GlyphToken::keyword(range, keyword).into(),
                            };
                        }

                        // If we could parse a number...
                        //
                        // We also want to error if numbers are followed immediately by an
                        // identifier without a space. For example, we want `4px` to be a
                        // syntax error.
                        if let Some(number) = Number::parse(&mut self.chars) {
                            // If our number is immediately followed by an identifier or a number
                            // then we have an invalid number token. Collect all the identifier or
                            // number tokens before returning the error token.
                            if self.chars.lookahead_is(Identifier::is_continue) {
                                let mut invalid = match number {
                                    Ok(number) => number.into_raw(),
                                    Err(invalid) => invalid,
                                };
                                while self.chars.lookahead_is(Identifier::is_continue) {
                                    invalid.push(self.chars.advance().unwrap());
                                }
                                let range = Range::new(start, invalid.len() as u32);
                                return self.error(
                                    TokenRange::new(full_start, range),
                                    Diagnostic::invalid_number(range, invalid),
                                );
                            }

                            // Return the parsed number.
                            return match number {
                                Ok(number) => {
                                    let range = Range::new(start, number.raw().len() as u32);
                                    let range = TokenRange::new(full_start, range);
                                    NumberToken::new(range, number).into()
                                }
                                Err(invalid) => {
                                    let range = Range::new(start, invalid.len() as u32);
                                    self.error(
                                        TokenRange::new(full_start, range),
                                        Diagnostic::invalid_number(range, invalid),
                                    )
                                }
                            };
                        }

                        // Otherwise we have an unexpected character!
                        self.chars.advance();
                        let range = Range::new(start, c.len_utf8() as u32); // TODO: Test `c.len_utf8()`.
                        self.error(
                            TokenRange::new(full_start, range),
                            Diagnostic::unexpected_char(range, c),
                        )
                    }
                },
                // When we reach the end of our document return an `EndToken` and keep returning an
                // `EndToken` for all time.
                None => {
                    let end = self.chars.position();
                    self.chars.advance();
                    let range = TokenRange::new(full_start, Range::new(end, 0));
                    EndToken::new(range).into()
                }
            };
        }
    }

    /// Reports an error diagnostic and returns an error token referencing that diagnostic.
    fn error(&self, range: TokenRange, diagnostic: Diagnostic) -> Token {
        let diagnostic = self.diagnostics.borrow_mut().report(diagnostic);
        ErrorToken::new(range, diagnostic).into()
    }
}

#[cfg(test)]
mod tests {
    use super::super::document::Document;
    use super::*;
    use crate::diagnostics::DiagnosticSet;
    use std::cell::RefCell;

    #[test]
    #[should_panic(
        expected = "Should not call `DocumentChars::advance()` again after it returns `None`."
    )]
    fn document_chars_end_panic() {
        let document = Document::new("/path/to/document.txt".into(), "abc".into());
        let diagnostics = RefCell::new(DiagnosticSet::new());
        let mut lexer = Lexer::new(&diagnostics, &document);
        assert!(lexer.advance().is_identifier());
        assert!(lexer.advance().is_end());
        lexer.advance();
    }

    #[test]
    #[should_panic(
        expected = "Should not call `DocumentChars::advance()` again after it returns `None`."
    )]
    fn document_chars_end_panic_lookahead() {
        let document = Document::new("/path/to/document.txt".into(), "abc".into());
        let diagnostics = RefCell::new(DiagnosticSet::new());
        let mut lexer = Lexer::new(&diagnostics, &document);
        assert!(lexer.lookahead().is_identifier());
        assert!(lexer.lookahead().is_identifier());
        assert!(lexer.advance().is_identifier());
        assert!(lexer.lookahead().is_end());
        assert!(lexer.lookahead().is_end());
        assert!(lexer.advance().is_end());
        lexer.lookahead();
    }
}
