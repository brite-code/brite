//! Parsing, printing, and traversing Brite source code.

mod ast;
mod document;
mod identifier;
mod lexer;
mod number;
// mod parser;
mod parser2;
mod token;

pub use self::ast::Module;
pub use self::document::{Document, Range};
pub use self::token::*;

use self::lexer::Lexer;
use crate::diagnostics::DiagnosticSet;

/// Parses a Brite source document into an Abstract Syntax Tree (AST). We can handle any source
/// text thrown at this function. Only correct Brite code can be executed, though.
pub fn parse(document: &Document) -> (DiagnosticSet, Module) {
    // Create the lexer out of document characters.
    let lexer = Lexer::new(&document);
    // Parse our module using the lexer. Reporting all diagnostics to the `DiagnosticSet` we return.
    let (diagnostics, module) = parser2::parse(lexer);
    // If debug assertions are enabled then run our invariants to make sure that the tokens list and
    // the module AST are well formed.
    //
    // These assertions are very important for test suite coverage.
    if cfg!(test) || cfg!(debug_assertions) {
        let tokens = Lexer::tokens(&document);
        // Assert that the start of each token is equal to the end of the previous token.
        let mut prev_position = document.start();
        for token in &tokens {
            assert_eq!(
                prev_position,
                token.full_range().full_start(),
                "Start of the next token does not equal end of the previous token."
            );
            prev_position = token.full_range().end();
        }
        // Assert that the end of the last token is equal to the end of the document.
        assert_eq!(
            prev_position,
            document.end(),
            "End of the last token does not equal end of the document."
        );
        // Assert that we can convert our module AST back into the list of tokens it was
        // parsed from.
        assert_eq!(
            tokens,
            module.clone().into_tokens(),
            "Could not turn the module AST back into the tokens list it was parsed from."
        );
    }
    // Return the parsed module and the reported diagnostics.
    (diagnostics, module)
}
