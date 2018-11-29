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
use crate::diagnostics::Diagnostic;
use std::rc::Rc;

/// Parses a Brite source document into an Abstract Syntax Tree (AST). We can handle any source
/// text thrown at this function. Only correct Brite code can be executed, though.
pub fn parse(document: &Document) -> (Vec<Rc<Diagnostic>>, Module) {
    // Create the lexer out of document characters.
    let lexer = Lexer::new(document.chars());
    // Parse our module from the lexer. Optionally add some extra debugging information on
    // the lexer.
    let module = if cfg!(debug_assertions) {
        // If debug assertions are enabled then let’s make sure that tokens returned by our lexer
        // are tightly packed.
        let lexer = lexer.scan(Position::initial(), |position, token| {
            let range = token.range();
            assert_eq!(*position, range.full_start());
            *position = range.end();
            Some(token)
        });
        // Now parse our module.
        Parser::parse(lexer)
    } else {
        Parser::parse(lexer)
    };
    unimplemented!()
}
