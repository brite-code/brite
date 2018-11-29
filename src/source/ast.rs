//! The Abstract Syntax Tree (AST) for Brite source code. The AST is a literal translation of
//! source code. This means it might not be semantically correct. The AVT is a code
//! representation which ensures semantic correctness.
//!
//! We use an AST for:
//!
//! - Type checking.
//! - Pretty printing.

/// A module represents a single Brite file. A module is made up of any number of declarations
/// or statements.
#[derive(Clone, Debug)]
pub struct Module {
    /// All the items in our module.
    items: Vec<Item>,
}

/// A Brite source code item is either a declarative `Declaration` whose order does not matter or an
/// imperative `Statement` whose order does matter.
#[derive(Clone, Debug)]
pub enum Item {
    Statement(Statement),
}

/// Represents some imperative action to be carried out.
#[derive(Clone, Debug)]
pub enum Statement {
    /// Any `Expression`.
    Expression(Expression),
}

/// Some instructions our programming language interprets to return a value and possibly perform
/// some side effects.
#[derive(Clone, Debug)]
pub enum Expression {}
