use std::borrow::Borrow;
use std::collections::HashSet;
use std::hash::{Hash, Hasher};
use std::ops::Deref;
use std::sync::RwLock;

lazy_static! {
    /// An intern pool for strings. There is only one string intern pool. Once a string has been
    /// interned it is never de-allocated. The memory will be _intentionally_ leaked.
    pub static ref STRING_INTERN_POOL: InternPool<str> = InternPool::new();
}

/// Implements an [intern pool][1] whose purpose is to save space by only ever allocating one copy
/// of each value in the intern pool. A common usage is with small strings that get reused a lot. By
/// interning a string, all copies of the same interned string will use an identical reference which
/// is cheap to compare and very space efficient.
///
/// In our implementation, once a string has been interned it will never be de-allocated for the
/// lifetime of the program. The memory will be _intentionally_ leaked.
///
/// The implementation takes inspiration from the [rustc interner][2].
///
/// [1]: https://en.wikipedia.org/wiki/String_interning
/// [2]: https://github.com/rust-lang/rust/blob/fd42f24b0129b32d66f174510518c083cdcec3eb/src/bootstrap/cache.rs#L162-L165
pub struct InternPool<T: Eq + Hash + ?Sized + 'static> {
    pool: RwLock<HashSet<&'static T>>,
}

impl<T: Eq + Hash + ?Sized> InternPool<T> {
    /// Creates a new intern pool. Private so that we can have one, canonical, pool for every type
    /// we want to intern.
    fn new() -> Self {
        InternPool {
            pool: RwLock::new(HashSet::new()),
        }
    }

    /// Interns a value. Either returns a reference to the previously interned value or creates a
    /// new interned value.
    ///
    /// **Warning:** This function intentionally leaks! If this is the first time we see `value`
    /// then we will never drop it for the lifetime of our program.
    pub fn intern<U>(&self, value: U) -> Intern<T>
    where
        U: Borrow<T> + 'static,
    {
        if let Some(value) = self.pool.read().unwrap().get(value.borrow()) {
            Intern(value)
        } else {
            let value: &'static U = Box::leak(Box::new(value));
            let value = value.borrow();
            self.pool.write().unwrap().insert(value);
            Intern(value)
        }
    }

    /// Interns a borrowed value. Either returns a reference to the previously interned value or
    /// creates a new interned value.
    ///
    /// **Warning:** This function intentionally leaks! If this is the first time we see `value`
    /// then we will never drop it for the lifetime of our program.
    pub fn intern_borrow(&self, value: &T) -> Intern<T>
    where
        T: ToOwned,
    {
        if let Some(value) = self.pool.read().unwrap().get(value) {
            Intern(value)
        } else {
            let value: &'static T::Owned = Box::leak(Box::new(value.to_owned()));
            let value = value.borrow();
            self.pool.write().unwrap().insert(value);
            Intern(value)
        }
    }
}

/// An interned value. [`Eq`] and [`Hash`] implementations use referential equality instead of
/// physical equality since all interned values should become the same reference.
pub struct Intern<T: ?Sized + 'static>(&'static T);

impl<T: ?Sized> PartialEq for Intern<T> {
    fn eq(&self, other: &Self) -> bool {
        (self.0 as *const T) == (other.0 as *const T)
    }
}

impl<T: ?Sized> Eq for Intern<T> {}

impl<T: ?Sized> Hash for Intern<T> {
    fn hash<H: Hasher>(&self, hasher: &mut H) {
        (self.0 as *const T).hash(hasher)
    }
}

impl<T: ?Sized> Clone for Intern<T> {
    fn clone(&self) -> Self {
        Intern(self.0)
    }
}

impl<T: ?Sized> Copy for Intern<T> {}

impl<T: ?Sized> Deref for Intern<T> {
    type Target = T;
    fn deref(&self) -> &T {
        self.0
    }
}
