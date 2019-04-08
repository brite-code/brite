//! Construction and printing of [S-expressions][1] for debugging complex tree data structures like
//! an AST. We call this module “lisp” to avoid the common abbreviation for S-expressions.
//!
//! [1]: https://en.wikipedia.org/wiki/S-expression

use super::vecn::Vec2;
use crate::parser::Identifier;
use pretty::{Arena, DocAllocator, DocBuilder};

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
    /// An atom. The text should not contain line breaks or parentheses (`(` and `)`).
    Atom(String),
    /// An expression which represents the concatenation of at least two `S` expressions.
    List(Vec2<Lisp>),
}

impl Lisp {
    /// Prints an S-expression to a string with the provided width.
    pub fn print(self, width: usize) -> String {
        let mut w = Vec::new();
        self.pretty(&Arena::new()).1.render(width, &mut w).unwrap();
        String::from_utf8(w).unwrap()
    }

    /// Creates a pretty-printable document from an S-expression.
    fn pretty<'a, D>(self, doc_allocator: &'a D) -> DocBuilder<'a, D>
    where
        D: DocAllocator<'a>,
    {
        match self {
            Lisp::Atom(s) => doc_allocator.as_string(s),
            Lisp::List(xs) => doc_allocator
                .text("(")
                .append({
                    let mut doc = doc_allocator.nil();
                    let mut xs_iter = xs.into_iter();
                    if let Some(x1) = xs_iter.next() {
                        doc = doc.append(x1.pretty(doc_allocator));
                        for x in xs_iter {
                            doc = doc.append(doc_allocator.space());
                            doc = doc.append(x.pretty(doc_allocator));
                        }
                    }
                    doc.nest(1).group()
                })
                .append(doc_allocator.text(")")),
        }
    }
}

impl From<&'static str> for Lisp {
    fn from(str: &'static str) -> Self {
        Lisp::Atom(str.to_string())
    }
}

impl From<String> for Lisp {
    fn from(string: String) -> Self {
        Lisp::Atom(string)
    }
}

impl<'a> From<&'a Identifier> for Lisp {
    fn from(identifier: &'a Identifier) -> Self {
        Lisp::Atom(identifier.as_str().to_string())
    }
}
