//! Parses a stream of tokens into an Abstract Syntax Tree (AST). We designed our parser with error
//! recovery in mind! The parser should be able to process any text document thrown at it. The error
//! recovery design is partly inspired by [Microsoft’s error tolerant PHP parser][1] design.
//!
//! [1]: https://github.com/Microsoft/tolerant-php-parser/blob/master/docs/HowItWorks.md

use super::ast::*;
use super::token::*;
use crate::diagnostics::DiagnosticSet;
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

    /// Parses a module consuming all tokens in the provided iterator.
    pub fn parse(diagnostics: &'a RefCell<DiagnosticSet>, tokens: I) -> Module {
        let parser = Self::new(diagnostics, tokens);
        unimplemented!()
    }

    fn parse_items(&mut self) {}
}
