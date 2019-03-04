//! Various `Vec` variants with a minimum number of elements. For example, a [`Vec2`] must have a
//! minimum of two elements.

use std::fmt;
use std::slice::Iter;
use std::vec::IntoIter;

/// Creates a `Vec1` which must have at least one element.
#[macro_export]
macro_rules! vec1 {
    ($first:expr) => ($crate::utils::vecn::Vec1::new($first));
    ($first:expr, $($item:expr),*) => ($crate::utils::vecn::Vec1::from_vec(vec![$first, $($item),*]));
    ($first:expr, $($item:expr,)*) => (vec1![$first, $($item),*]);
}

/// Creates a `Vec2` which must have at least two elements.
#[macro_export]
macro_rules! vec2 {
    ($first:expr, $second:expr) => ($crate::utils::vecn::Vec2::new($first, $second));
    ($first:expr, $second:expr, $($item:expr),*) => ($crate::utils::vecn::Vec2::from_vec(vec![$first, $second, $($item),*]));
    ($first:expr, $second:expr, $($item:expr,)*) => (vec2![$first, $second, $($item),*]);
}

/// A vector with at least one element.
pub struct Vec1<T> {
    /// We put all our items in a vec. So our types don’t hold the contract of our utility only the
    /// external interface.
    vec: Vec<T>,
}

impl<T> Vec1<T> {
    /// Creates a new `Vec1` with at least one element.
    pub fn new(first: T) -> Self {
        Vec1 { vec: vec![first] }
    }

    /// Creates a `Vec1` from a `Vec`. If the `Vec` is empty then we will panic.
    ///
    /// # Panics
    ///
    /// Panics if the `Vec` has less than one element.
    pub fn from_vec(vec: Vec<T>) -> Self {
        assert!(vec.len() >= 1);
        Vec1 { vec }
    }

    /// Push an item to our `Vec1`.
    pub fn push(&mut self, item: T) {
        self.vec.push(item)
    }

    /// Removes the last item from the vector and returns it. Returns `None` if there is only one
    /// item in the vector so that the vector is never empty.
    pub fn pop(&mut self) -> Option<T> {
        if self.vec.len() > 1 {
            self.vec.pop()
        } else {
            None
        }
    }

    /// Gets an [`Iterator`] for traversing through the vector.
    pub fn iter(&self) -> Iter<T> {
        self.vec.iter()
    }

    /// Gets a reference to the last element. A last element always exists because a `Vec1` has at
    /// least one element.
    pub fn last(&self) -> &T {
        let i = self.vec.len() - 1;
        &self.vec[i]
    }

    /// Gets a mutable reference to the last element. A last element always exists because a `Vec1`
    /// has at least one element.
    pub fn last_mut(&mut self) -> &mut T {
        let i = self.vec.len() - 1;
        &mut self.vec[i]
    }
}

impl<T: fmt::Debug> fmt::Debug for Vec1<T> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        self.vec.fmt(f)
    }
}

impl<T> IntoIterator for Vec1<T> {
    type Item = T;
    type IntoIter = IntoIter<T>;

    fn into_iter(self) -> Self::IntoIter {
        self.vec.into_iter()
    }
}

/// A vector with at least two elements.
pub struct Vec2<T> {
    /// We put all our items in a vec. So our types don’t hold the contract of our utility only the
    /// external interface.
    vec: Vec<T>,
}

impl<T> Vec2<T> {
    /// Creates a new `Vec2` with at least two elements.
    pub fn new(first: T, second: T) -> Self {
        Vec2 {
            vec: vec![first, second],
        }
    }

    /// Creates a `Vec2` from a `Vec`. If the `Vec` is empty then we will panic.
    ///
    /// # Panics
    ///
    /// Panics if the `Vec` has less than two elements.
    pub fn from_vec(vec: Vec<T>) -> Self {
        assert!(vec.len() >= 2);
        Vec2 { vec }
    }

    /// Push an item to our `Vec2`.
    pub fn push(&mut self, item: T) {
        self.vec.push(item)
    }
}

impl<T> IntoIterator for Vec2<T> {
    type Item = T;
    type IntoIter = IntoIter<T>;

    fn into_iter(self) -> Self::IntoIter {
        self.vec.into_iter()
    }
}
