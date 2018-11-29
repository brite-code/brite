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
    // Parse our module using the lexer.
    let module = Parser::parse(&diagnostics, lexer);
    // Return the parsed module and the reported diagnostics.
    let diagnostics = diagnostics.into_inner();
    (diagnostics, module)
}
