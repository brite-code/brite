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
    pub fn parse(
        document: &'a Document,
        diagnostics: &'a RefCell<DiagnosticSet>,
        tokens: I,
    ) -> Module {
        let mut parser = Self::new(diagnostics, tokens);
        let items = parser.parse_item_list();

        // Assert that we consumed all the tokens while parsing.
        debug_assert!(
            parser.tokens.peek().is_none(),
            "Parsing must consume all tokens."
        );

        Module {
            range: document.range(),
            items,
        }
    }

    fn parse_item_list(&mut self) -> Vec<Item> {
        let mut items = Vec::new();

        // While we still have tokens continue to parse items.
        while !self.tokens.peek().is_none() {
            unimplemented!()
            // // Try to parse an item. If successful we add it to the `items` array and try to parse
            // // another item.
            // if let Some(item) = self.parse_item() {
            //     items.push(item);
            // } else {
            //     // We expect there to always be a token because of our while loop condition. Also,
            //     // `self.parse_item()` should only return `None` if it did not consume any tokens.
            //     let token = self.tokens.next().unwrap();
            // }
        }

        // Shrink the items list. We’ve parsed all of them!
        items.shrink_to_fit();
        items
    }

    fn parse_item(&mut self) -> Result<Item, DiagnosticRef> {
        self.parse_statement().map(Item::Statement)
    }

    fn parse_statement(&mut self) -> Result<Statement, DiagnosticRef> {
        self.parse_expression().map(Statement::Expression)
    }

    fn parse_expression(&mut self) -> Result<Expression, DiagnosticRef> {
        match self.tokens.peek() {
            _ => {
                let token = self.tokens.next().unwrap();
                self.error(Diagnostic::unexpected_token(
                    token.full_range().range(),
                    token,
                ))
            }
        }
    }

    /// Reports an error diagnostic.
    fn error<T>(&mut self, diagnostic: Diagnostic) -> Result<T, DiagnosticRef> {
        Err(self.diagnostics.borrow_mut().report(diagnostic))
    }
}
