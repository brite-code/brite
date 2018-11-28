//! The Abstract Syntax Tree (AST) for Brite source code. The AST is a literal translation of
//! source code. This means it might not be semantically correct. The AVT is a code
//! representation which ensures semantic correctness.
//!
//! We use an AST for:
//!
//! - Type checking.
//! - Pretty printing.

use super::position::Position;

/// A module represents a single Brite file. A module is made up of any number of declarations
/// or statements.
pub struct Module {
    pub items: Vec<Item>,
    pub end: Position,
}

/// A Brite source code item is either a declarative `Declaration` whose order does not matter or an
/// imperative `Statement` whose order does matter.
pub enum Item {}
