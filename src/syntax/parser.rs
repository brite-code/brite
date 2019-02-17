use super::source::{Lexer, Token};
use crate::ui::{Diagnostic, DiagnosticRef};

struct Parser<'src> {
    /// The lexer our parser uses. You shouldn’t use the lexer directly. Instead use provided
    /// utility functions like `Parser::advance()` and `Parser::lookahead()`.
    _lexer: Lexer<'src>,
    peeked: Option<Option<Token<'src>>>,
}

impl<'src> Parser<'src> {
    /// Advance the lexer and return the next token.
    fn advance(&mut self) -> Option<Token<'src>> {
        match self.peeked.take() {
            Some(next) => next,
            None => self._lexer.next(),
        }
    }

    /// Lookahead at the next token.
    fn lookahead(&mut self) -> Option<&Token<'src>> {
        if self.peeked.is_none() {
            self.peeked = Some(self._lexer.next());
        }
        match &self.peeked {
            Some(next) => next.as_ref(),
            None => unreachable!(),
        }
    }

    /// Report a diagnostic.
    fn report(&mut self, diagnostic: Diagnostic) -> DiagnosticRef {
        self._lexer.report_diagnostic(diagnostic)
    }
}
