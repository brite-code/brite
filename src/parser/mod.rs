//! Understanding and manipulating Brite source code.

mod document;
mod lexer;
mod parser;

pub mod syntax;

pub use self::document::*;
pub use self::lexer::*;
pub use self::parser::*;
