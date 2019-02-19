//! Construction and printing of [S-expressions][1] for debugging complex tree data structures like
//! an AST.
//!
//! [1]: https://en.wikipedia.org/wiki/S-expression

use super::vecn::Vec2;
use crate::syntax::{Identifier, Range};

/// Creates a `SymbolicExpression`.
#[macro_export]
macro_rules! s {
    ($atom:expr) => {
        SymbolicExpression::from($atom)
    };
    ($first:expr, $second:expr) => {
        SymbolicExpression::Expression(vec2![$first.into(), $second.into()])
    };
    ($first:expr, $second:expr, $($item:expr),*) => {
        SymbolicExpression::Expression(vec2![$first.into(), $second.into(), $($item.into()),*])
    };
    ($first:expr, $second:expr, $($item:expr,)*) => (s![$first, $second, $($item),*]);
}

/// An [S-expression][1].
///
/// [1]: https://en.wikipedia.org/wiki/S-expression
pub enum SymbolicExpression {
    /// An atom.
    Atom(String),
    /// An expression which represents the concatenation of at least two `S` expressions.
    Expression(Vec2<SymbolicExpression>),
}

impl From<&'static str> for SymbolicExpression {
    fn from(str: &'static str) -> Self {
        SymbolicExpression::Atom(str.to_string())
    }
}

impl<'a> From<&'a Identifier> for SymbolicExpression {
    fn from(identifier: &'a Identifier) -> Self {
        SymbolicExpression::Atom(identifier.source().to_string())
    }
}

impl From<Range> for SymbolicExpression {
    fn from(range: Range) -> Self {
        unimplemented!()
    }
}
