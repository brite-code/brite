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

use crate::diagnostics::Diagnostic;
use std::rc::Rc;

/// Parses a Brite source document into an Abstract Syntax Tree (AST). We can handle any source
/// text thrown at this function. Only well formatted code will be useful, though.
pub fn parse(document: &Document) -> (Vec<Rc<Diagnostic>>, Module) {
    // TODO: Lexer debug check.
    unimplemented!()
}
