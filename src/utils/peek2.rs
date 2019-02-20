//! A peekable iterator except it allows you to peek two items into the future instead of just one.

use std::mem;

/// A peekable iterator except it allows you to peek two items into the future instead of just one.
///
/// We don’t implement [`Iterator`] since [`Iterator::peekable`] in the standard library optimizes
/// a bunch of other iterator combinators for peekable iterators. We don’t care about those so
/// instead of implementing [`Iterator`] and not optimizing all the other functions we choose to
/// not even implement [`Iterator`]. If we need this to be an iterator we can implement those custom
/// functions in the future.
pub struct Peekable2<I: Iterator> {
    iter: I,
    peeked1: Option<Option<I::Item>>,
    peeked2: Option<Option<I::Item>>,
}

impl<I: Iterator> Peekable2<I> {
    /// Create a new peekable iterator.
    #[inline]
    pub fn new(iter: I) -> Self {
        Peekable2 {
            iter,
            peeked1: None,
            peeked2: None,
        }
    }

    /// Advance the iterator.
    #[inline]
    pub fn next(&mut self) -> Option<I::Item> {
        match self.peeked1.take() {
            Some(next) => {
                mem::swap(&mut self.peeked1, &mut self.peeked2);
                next
            }
            None => self.iter.next(),
        }
    }

    /// Peek at the next character without advancing the iterator.
    #[inline]
    pub fn peek(&mut self) -> Option<&I::Item> {
        if self.peeked1.is_none() {
            self.peeked1 = Some(self.iter.next());
        }
        match &self.peeked1 {
            Some(peek) => peek.as_ref(),
            None => unreachable!(),
        }
    }

    /// Peek ahead two characters without advancing the iterator.
    #[inline]
    pub fn peek2(&mut self) -> Option<&I::Item> {
        if self.peeked1.is_none() {
            self.peeked1 = Some(self.iter.next());
        }
        if self.peeked2.is_none() {
            self.peeked2 = Some(self.iter.next());
        }
        match &self.peeked2 {
            Some(peek) => peek.as_ref(),
            None => unreachable!(),
        }
    }

    /// Give immutable access to the underlying iterator in case there is state you’d like to view.
    /// The underlying iterator will have advanced when peek is called, so be careful!
    #[inline]
    pub fn iter(&self) -> &I {
        &self.iter
    }
}
