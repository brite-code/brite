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
use super::lexer::Lexer;
use crate::diagnostics::DiagnosticSet;
use std::cell::RefCell;

/// Parses a stream of tokens into an Abstract Syntax Tree (AST).
pub struct Parser<'a> {
    diagnostics: &'a RefCell<DiagnosticSet>,
    lexer: Lexer<'a>,
}

impl<'a> Parser<'a> {
    fn new(diagnostics: &'a RefCell<DiagnosticSet>, lexer: Lexer<'a>) -> Self {
        Parser { diagnostics, lexer }
    }

    /// Parses a module from a token stream consuming _all_ tokens in the stream.
    pub fn parse(diagnostics: &'a RefCell<DiagnosticSet>, lexer: Lexer<'a>) -> Module {
        unimplemented!()
    }
}
