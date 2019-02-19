//! Construction and printing of [S-expressions][1] for debugging complex tree data structures like
//! an AST. We call this module “lisp” to avoid the common abbreviation for S-expressions.
//!
//! [1]: https://en.wikipedia.org/wiki/S-expression

use super::vecn::Vec2;
use crate::syntax::{Identifier, Range};

/// Creates a `Lisp`.
#[macro_export]
macro_rules! lisp {
    ($atom:expr) => {
        Lisp::from($atom)
    };
    ($first:expr, $second:expr) => {
        Lisp::List(vec2![$first.into(), $second.into()])
    };
    ($first:expr, $second:expr, $($item:expr),*) => {
        Lisp::List(vec2![$first.into(), $second.into(), $($item.into()),*])
    };
    ($first:expr, $second:expr, $($item:expr,)*) => (lisp![$first, $second, $($item),*]);
}

/// An [S-expression][1].
///
/// [1]: https://en.wikipedia.org/wiki/S-expression
pub enum Lisp {
    /// An atom.
    Atom(String),
    /// An expression which represents the concatenation of at least two `S` expressions.
    List(Vec2<Lisp>),
}

impl From<&'static str> for Lisp {
    fn from(str: &'static str) -> Self {
        Lisp::Atom(str.to_string())
    }
}

impl<'a> From<&'a Identifier> for Lisp {
    fn from(identifier: &'a Identifier) -> Self {
        Lisp::Atom(identifier.source().to_string())
    }
}

impl From<Range> for Lisp {
    fn from(range: Range) -> Self {
        unimplemented!()
    }
}
