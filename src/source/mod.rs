//! Parsing, printing, and traversing Brite source code.

mod ast;
mod document;
mod identifier;
mod lexer;
mod number;
mod parser;
mod token;

pub use self::ast::*;
pub use self::document::*;
pub use self::token::*;

use self::lexer::Lexer;
use self::parser::Parser;
use crate::diagnostics::DiagnosticSet;
use std::cell::RefCell;

/// Parses a Brite source document into an Abstract Syntax Tree (AST). We can handle any source
/// text thrown at this function. Only correct Brite code can be executed, though.
pub fn parse(document: &Document) -> (DiagnosticSet, Module) {
    // Create a new diagnostics set. We wrap the set in a `RefCell` because we want to give both
    // `Lexer` and `Parser` a mutable reference. We can’t have two mutable references so we use
    // a `RefCell`.
    let diagnostics = RefCell::new(DiagnosticSet::new());
    // Create the lexer out of document characters.
    let lexer = Lexer::new(&diagnostics, document.chars());
    // Parse our module from the lexer. Optionally add some extra debugging information on
    // the lexer.
    let module = if cfg!(debug_assertions) {
        let mut tokens = Vec::new();
        let mut prev_position = document.range().start();
        // Our parser drives our lexer to tokenize the document characters. While this happens we
        // inspect each new token and perform some assertions to ensure our lexer is well formed.
        let lexer = lexer.inspect(|token| {
            // Clone our token and add it to a local list.
            tokens.push(token.clone());
            // Make sure the previous end position is equal to the current start position.
            let range = token.full_range();
            assert_eq!(
                prev_position,
                range.full_start(),
                "The token should begin where the previous token ended."
            );
            prev_position = range.end();
        });
        // Now parse our module. This will consume all the tokens in the lexer.
        let module = Parser::parse(document, &diagnostics, lexer);
        // Assert that we can convert our module AST back into our tokens list. This assertion
        // ensures we don’t lose _any_ information while parsing.
        assert_eq!(
            module.clone().into_tokens(),
            tokens,
            "We should be able to turn our module back into our tokens list."
        );
        // Return the module.
        module
    } else {
        Parser::parse(document, &diagnostics, lexer)
    };
    // Return the parsed module and the reported diagnostics.
    let diagnostics = diagnostics.into_inner();
    (diagnostics, module)
}
