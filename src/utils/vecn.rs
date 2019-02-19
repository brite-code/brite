//! Various `Vec` variants with a minimum number of elements. For example, a [`Vec2`] must have a
//! minimum of two elements.

/// Creates a `Vec2` which must have at least two elements.
#[macro_export]
macro_rules! vec2 {
    ($first:expr, $second:expr) => ($crate::utils::vecn::Vec2::new($first, $second));
    ($first:expr, $second:expr, $($item:expr),*) => ($crate::utils::vecn::Vec2::from_vec(vec![$first, $second, $($item),*]));
    ($first:expr, $second:expr, $($item:expr,)*) => (vec2![$first, $second, $($item),*]);
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

    /// Creates a `Vec2` from a `Vec`. If the `Vec` is empty then we will return `None`.
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
