//! Turns a stream of source characters into a stream of `Token`s. Tokens are a more useful unit for
//! parsing than characters since tokens will parse numbers, identifiers, glyphs, and will skip
//! comments and whitespace.

use super::document::{Document, DocumentChars, Range};
use super::identifier::Identifier;
use super::number::Number;
use super::token::*;
use crate::diagnostics::{Diagnostic, DiagnosticSet};

/// A lexer turns an iterator of source document characters into an iterator of tokens.
pub struct Lexer<'a> {
    diagnostics: &'a mut DiagnosticSet,
    /// The document characters we are lexing.
    chars: DocumentChars<'a>,
    /// If we have looked ahead then this will be `Some()`.
    lookahead: Option<Token>,
    /// If we have reached the end then this will be `Some()`. We will forever return this token now
    /// whenever `Lexer::advance()` is called.
    end: Option<Token>,
}

impl<'a> Lexer<'a> {
    /// Create a new lexer.
    pub fn new(diagnostics: &'a mut DiagnosticSet, document: &'a Document) -> Self {
        Lexer {
            diagnostics,
            chars: document.chars(),
            lookahead: None,
            end: None,
        }
    }

    /// Gets a mutable reference to the diagnostics object owned by this lexer.
    pub fn diagnostics_mut(&mut self) -> &mut DiagnosticSet {
        &mut self.diagnostics
    }

    /// Gets a list of all the tokens in a document.
    pub fn tokens(document: &'a Document) -> Vec<Token> {
        // Construct everything we’ll need for tokenization.
        let mut diagnostics = DiagnosticSet::new();
        let mut lexer = Lexer::new(&mut diagnostics, document);
        let mut tokens = Vec::new();
        // Advance through the lexer pushing every token. Once we reach the `EndToken` we push that
        // to our list and break out of the loop.
        loop {
            match lexer.advance() {
                token @ Token::End(_) => {
                    tokens.push(token);
                    break;
                }
                Token::Glyph(token) => {
                    debug_assert_eq!(
                        token.full_range().full_start().line(document)
                            == token.full_range().start().line(document),
                        token.single_line(),
                        "`GlyphToken::single_line()` is not accurate."
                    );
                    tokens.push(token.into());
                }
                token => tokens.push(token),
            }
        }
        // Return our tokens.
        tokens
    }

    /// Consume the next token in the lexer. Calling `Lexer::advance()` again will return a
    /// new token. Eventually we will return an `EndToken`. When that happens subsequent calls to
    /// `Lexer::advance()` will only ever return an `EndToken`.
    pub fn advance(&mut self) -> Token {
        match self.lookahead.take() {
            Some(token) => token,
            None => self.actually_advance(),
        }
    }

    /// Look at the next token without consuming the token and advancing the lexer. Calling
    /// `Lexer::lookahead()` again will return the same token.
    pub fn lookahead(&mut self) -> &Token {
        if self.lookahead.is_none() {
            self.lookahead = Some(self.actually_advance());
        }
        match &self.lookahead {
            Some(token) => token,
            None => unreachable!(),
        }
    }

    /// Looks at the next token and returns true if it is an identifier.
    pub fn lookahead_identifier(&mut self) -> bool {
        self.lookahead().is_identifier()
    }

    /// Advances the lexer, but only if the next token is an identifier.
    pub fn advance_identifier(&mut self) -> Option<IdentifierToken> {
        if self.lookahead_identifier() {
            match self.advance() {
                Token::Identifier(token) => Some(token),
                _ => unreachable!(),
            }
        } else {
            None
        }
    }

    /// Looks at the next token and returns true if it is a number.
    pub fn lookahead_number(&mut self) -> bool {
        self.lookahead().is_number()
    }

    /// Advances the lexer, but only if the next token is a number.
    pub fn advance_number(&mut self) -> Option<NumberToken> {
        if self.lookahead_number() {
            match self.advance() {
                Token::Number(token) => Some(token),
                _ => unreachable!(),
            }
        } else {
            None
        }
    }

    /// Looks at the next token and returns true if it is the specified glyph.
    pub fn lookahead_glyph(&mut self, glyph: Glyph) -> bool {
        self.lookahead().is_glyph(glyph)
    }

    /// Advances the lexer, but only if the next token is the specified glyph.
    pub fn advance_glyph(&mut self, glyph: Glyph) -> Option<GlyphToken> {
        if self.lookahead_glyph(glyph) {
            match self.advance() {
                Token::Glyph(token) => Some(token),
                _ => unreachable!(),
            }
        } else {
            None
        }
    }

    /// Looks at the next token and returns true if it is the end token.
    pub fn lookahead_end(&mut self) -> bool {
        self.lookahead().is_end()
    }

    /// Advances the lexer, but only if the next token is the end token.
    pub fn advance_end(&mut self) -> Option<EndToken> {
        if self.lookahead_end() {
            match self.advance() {
                Token::End(token) => Some(token),
                _ => unreachable!(),
            }
        } else {
            None
        }
    }

    /// Actually advances the lexer. The `Lexer::advance()` function performs some
    /// housekeeping work only.
    fn actually_advance(&mut self) -> Token {
        // If we have ended then just keep returning the same token!
        if let Some(token) = &self.end {
            return token.clone();
        }

        // The full start of our token including trivia like whitespace and comments.
        let full_start = self.chars.position();

        // Does the full range of this token fit on a single line?
        let mut single_line = true;

        // Loop since we want to ignore whitespace and comments while still remembering our full
        // start position.
        loop {
            return match self.chars.lookahead() {
                Some(c) => match c {
                    '{' => {
                        let start = self.chars.position();
                        self.chars.advance();
                        let range = TokenRange::new(full_start, Range::new(start, 1));
                        GlyphToken::new(range, Glyph::BraceLeft, single_line).into()
                    }

                    '}' => {
                        let start = self.chars.position();
                        self.chars.advance();
                        let range = TokenRange::new(full_start, Range::new(start, 1));
                        GlyphToken::new(range, Glyph::BraceRight, single_line).into()
                    }

                    ',' => {
                        let start = self.chars.position();
                        self.chars.advance();
                        let range = TokenRange::new(full_start, Range::new(start, 1));
                        GlyphToken::new(range, Glyph::Comma, single_line).into()
                    }

                    '=' => {
                        let start = self.chars.position();
                        self.chars.advance();
                        let range = TokenRange::new(full_start, Range::new(start, 1));
                        GlyphToken::new(range, Glyph::Equals, single_line).into()
                    }

                    '(' => {
                        let start = self.chars.position();
                        self.chars.advance();
                        let range = TokenRange::new(full_start, Range::new(start, 1));
                        GlyphToken::new(range, Glyph::ParenLeft, single_line).into()
                    }

                    ')' => {
                        let start = self.chars.position();
                        self.chars.advance();
                        let range = TokenRange::new(full_start, Range::new(start, 1));
                        GlyphToken::new(range, Glyph::ParenRight, single_line).into()
                    }

                    ';' => {
                        let start = self.chars.position();
                        self.chars.advance();
                        let range = TokenRange::new(full_start, Range::new(start, 1));
                        GlyphToken::new(range, Glyph::Semicolon, single_line).into()
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
                                            single_line = false;
                                            break;
                                        }
                                        Some('\r') => {
                                            self.chars.advance();
                                            self.chars.advance_char('\n');
                                            single_line = false;
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
                                GlyphToken::new(range, Glyph::Slash, single_line).into()
                            }
                        }
                    }

                    // Ignore newlines (`\n`).
                    '\n' => {
                        self.chars.advance();
                        single_line = false;
                        continue;
                    }

                    // Ignore newlines (`\r` and `\r\n`).
                    '\r' => {
                        self.chars.advance();
                        single_line = false;
                        continue;
                    }

                    c => {
                        // Parse a dot, but only if the next character is not a number.
                        if c == '.' {
                            if let Some('0'...'9') = self.chars.lookahead2() {
                                // Noop, this is a number...
                            } else {
                                let start = self.chars.position();
                                self.chars.advance();
                                let range = TokenRange::new(full_start, Range::new(start, 1));
                                return GlyphToken::new(range, Glyph::Dot, single_line).into();
                            }
                        }

                        // Ignore whitespace.
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
                                Err(keyword) => {
                                    GlyphToken::keyword(range, keyword, single_line).into()
                                }
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
                                let diagnostic = Diagnostic::invalid_number(range, invalid);
                                let diagnostic = self.diagnostics.report(diagnostic);
                                let range = TokenRange::new(full_start, range);
                                return NumberToken::new(range, Err(diagnostic)).into();
                            }

                            // Return the parsed number.
                            return match number {
                                Ok(number) => {
                                    let range = Range::new(start, number.raw().len() as u32);
                                    let range = TokenRange::new(full_start, range);
                                    NumberToken::new(range, Ok(number)).into()
                                }
                                Err(invalid) => {
                                    let range = Range::new(start, invalid.len() as u32);
                                    let diagnostic = Diagnostic::invalid_number(range, invalid);
                                    let diagnostic = self.diagnostics.report(diagnostic);
                                    let range = TokenRange::new(full_start, range);
                                    NumberToken::new(range, Err(diagnostic)).into()
                                }
                            };
                        }

                        // Otherwise we have an unexpected character!
                        self.chars.advance();
                        let range = Range::new(start, c.len_utf8() as u32);
                        let range = TokenRange::new(full_start, range);
                        UnexpectedToken::new(range, c).into()
                    }
                },

                // When we reach the end of our document return an `EndToken` and keep returning an
                // `EndToken` for all time.
                None => {
                    let end = self.chars.position();
                    self.chars.advance();
                    let range = TokenRange::new(full_start, Range::new(end, 0));
                    let token: Token = EndToken::new(range).into();
                    self.end = Some(token.clone());
                    token
                }
            };
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn document_chars_end() {
        let document = Document::new("/path/to/document.txt".into(), "abc".into());
        let mut diagnostics = DiagnosticSet::new();
        let mut lexer = Lexer::new(&mut diagnostics, &document);
        assert!(lexer.advance().is_identifier());
        assert!(lexer.advance().is_end());
        assert!(lexer.advance().is_end());
        assert!(lexer.advance().is_end());
    }

    #[test]
    fn document_chars_end_lookahead() {
        let document = Document::new("/path/to/document.txt".into(), "abc".into());
        let mut diagnostics = DiagnosticSet::new();
        let mut lexer = Lexer::new(&mut diagnostics, &document);
        assert!(lexer.lookahead().is_identifier());
        assert!(lexer.lookahead().is_identifier());
        assert!(lexer.advance().is_identifier());
        assert!(lexer.lookahead().is_end());
        assert!(lexer.lookahead().is_end());
        assert!(lexer.advance().is_end());
        assert!(lexer.lookahead().is_end());
        assert!(lexer.lookahead().is_end());
    }

    #[test]
    fn document_chars_end_full_range() {
        let document = Document::new("/path/to/document.txt".into(), "  ".into());
        let mut diagnostics = DiagnosticSet::new();
        let mut lexer = Lexer::new(&mut diagnostics, &document);
        let token = lexer.advance();
        assert!(token.is_end());
        assert_eq!(
            token.full_range(),
            &TokenRange::new(document.start(), Range::new(document.end(), 0))
        );
        let token = lexer.advance();
        assert!(token.is_end());
        assert_eq!(
            token.full_range(),
            &TokenRange::new(document.start(), Range::new(document.end(), 0))
        );
        let token = lexer.advance();
        assert!(token.is_end());
        assert_eq!(
            token.full_range(),
            &TokenRange::new(document.start(), Range::new(document.end(), 0))
        );
    }
}
