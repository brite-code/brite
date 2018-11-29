//! Parses a stream of tokens into an Abstract Syntax Tree (AST). We designed our parser with error
//! recovery in mind! The parser should be able to process any text document thrown at it. The error
//! recovery design is partly inspired by [Microsoft’s error tolerant PHP parser][1] design.
//!
//! [1]: https://github.com/Microsoft/tolerant-php-parser/blob/master/docs/HowItWorks.md

use super::ast::*;
use super::token::*;
use std::iter::Peekable;

/// Parses a stream of tokens into an Abstract Syntax Tree (AST).
pub struct Parser<I>
where
    I: Iterator<Item = Token>,
{
    tokens: Peekable<I>,
}

impl<I> Parser<I>
where
    I: Iterator<Item = Token>,
{
    /// Parses a module consuming all tokens in the provided iterator.
    pub fn parse(tokens: I) -> Module {
        let tokens = tokens.peekable();
        let parser = Parser { tokens };
        unimplemented!()
    }

    fn parse_items(&mut self) {}
}
