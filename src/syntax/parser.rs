use super::ast::*;
use super::source::{Glyph, Lexer, TokenKind};
use crate::ui::{Diagnostic, DiagnosticRef, ExpectedSyntax};

struct Parser<'errs, 'src> {
    /// The lexer our parser uses.
    lexer: Lexer<'errs, 'src>,
}

impl<'errs, 'src> Parser<'errs, 'src> {
    /// Report a diagnostic.
    ///
    /// The implementation may change at any time. Currently calls `Lexer::report_diagnostic` since
    /// the lexer owns a unique mutable reference to our diagnostics collection.
    fn report_diagnostic(&mut self, diagnostic: Diagnostic) -> DiagnosticRef {
        self.lexer.report_diagnostic(diagnostic)
    }

    /// Parses a glyph. Reports an error if the next token is not a glyph.
    fn parse_glyph(&mut self, expected: Glyph) -> Result<(), DiagnosticRef> {
        match self.lexer.next() {
            Some(token) => match token.kind() {
                TokenKind::Glyph(actual) if expected == *actual => Ok(()),
                _ => Err(self.report_diagnostic(Diagnostic::unexpected_token(
                    &token,
                    ExpectedSyntax::Glyph(expected),
                ))),
            },
            None => {
                let end_position = self.lexer.peek_end().unwrap().position();
                Err(self.report_diagnostic(Diagnostic::unexpected_ending(
                    end_position,
                    ExpectedSyntax::Glyph(expected),
                )))
            }
        }
    }

    fn parse_declaration() {}

    fn parse_function(&mut self) -> Result<Function, DiagnosticRef> {
        self.parse_glyph(Glyph::ParenLeft)?;
        self.parse_glyph(Glyph::ParenRight)?;
        unimplemented!()
    }
}
