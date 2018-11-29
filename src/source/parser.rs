//! Parses a stream of tokens into an Abstract Syntax Tree (AST). We designed our parser with error
//! recovery in mind! The parser should be able to process any text document thrown at it. The error
//! recovery design is partly inspired by [Microsoft’s error tolerant PHP parser][1] design.
//!
//! Our goal for the error recovery behavior is:
//!
//! 1. Don’t over-complicate the parser. We’d rather the parser be easily extendable then have super
//!    robust error recovery.
//! 2. Don’t go to great lengths to produce some reasonable AST for code immediately around a
//!    syntax error. We’d rather show one syntax error than five type checker errors because we
//!    parsed some nonsense AST.
//! 3. Do parse surrounding declarations. That way we may continue providing services like
//!    hover types.
//!
//! [1]: https://github.com/Microsoft/tolerant-php-parser/blob/master/docs/HowItWorks.md

use super::ast::*;
use super::document::*;
use super::token::*;
use crate::diagnostics::{Diagnostic, DiagnosticRef, DiagnosticSet};
use std::cell::RefCell;
use std::iter::Peekable;

/// Parses a stream of tokens into an Abstract Syntax Tree (AST).
pub struct Parser<'a, I>
where
    I: Iterator<Item = Token>,
{
    diagnostics: &'a RefCell<DiagnosticSet>,
    tokens: Peekable<I>,
}

impl<'a, I> Parser<'a, I>
where
    I: Iterator<Item = Token>,
{
    fn new(diagnostics: &'a RefCell<DiagnosticSet>, tokens: I) -> Self {
        let tokens = tokens.peekable();
        Parser {
            diagnostics,
            tokens,
        }
    }

    /// Parses a module from a token stream consuming _all_ tokens in the stream.
    pub fn parse(diagnostics: &'a RefCell<DiagnosticSet>, tokens: I) -> Module {
        unimplemented!()
    }

    fn block(&mut self) {
        self.statement(Mode::Normal);
    }

    fn statement(&mut self, mode: Mode) -> Result<Statement, Error> {
        match mode {
            Mode::Normal => unimplemented!(),
            Mode::Recovery(error) => unimplemented!(),
        }
    }

    /// Reports an error diagnostic.
    fn error<T>(&mut self, diagnostic: Diagnostic) -> Result<T, Error> {
        Err(self.diagnostics.borrow_mut().report(diagnostic))
    }
}

type Error = DiagnosticRef;

enum Mode {
    Normal,
    Recovery(Error),
}
