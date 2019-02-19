use super::ast::*;
use super::source::{EndToken, Glyph, Lexer, Token};
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

    // /// Parses a glyph. Returns an error if the next token is not a glyph.
    // fn parse_glyph(&mut self, glyph: Glyph) -> Result<(), DiagnosticRef> {
    //     match self.advance() {
    //         Some(token) => match token.kind() {},
    //         None => Err(self.report_diagnostic(Diagnostic::unexpected_ending(
    //             position: Position,
    //             ExpectedSyntax::Glyph(glyph),
    //         ))),
    //     }
    // }

    fn parse_declaration() {}

    fn parse_function(&mut self) -> Result<Function, DiagnosticRef> {
        // self.parse_glyph(Glyph::ParenLeft)?;
        // self.parse_glyph(Glyph::ParenRight)?;
        unimplemented!()
    }
}
