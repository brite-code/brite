//! Parses a stream of tokens into an Abstract Syntax Tree (AST). We designed our parser with error
//! recovery in mind! The parser should be able to process any text document thrown at it. The error
//! recovery design is partly inspired by [Microsoft’s error tolerant PHP parser][1] design.
//!
//! [1]: https://github.com/Microsoft/tolerant-php-parser/blob/master/docs/HowItWorks.md

use super::ast::*;
use super::token::*;
use crate::diagnostics::Diagnostics;
use std::iter::Peekable;

/// Parses a stream of tokens into an Abstract Syntax Tree (AST).
pub fn parse(diagnostics: &mut Diagnostics, tokens: impl Iterator<Item = Token>) -> Module {
    let parser = Parser {
        diagnostics,
        tokens: tokens.peekable(),
    };
    unimplemented!()
}

struct Parser<'a, I>
where
    I: Iterator<Item = Token>,
{
    diagnostics: &'a mut Diagnostics,
    tokens: Peekable<I>,
}

impl<'a, I> Parser<'a, I>
where
    I: Iterator<Item = Token>,
{
    fn parse_items(&mut self) {}
}
