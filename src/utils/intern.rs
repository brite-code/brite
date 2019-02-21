use std::collections::HashMap;
use std::fmt;
use std::hash::{Hash, Hasher};
use std::marker::PhantomData;
use std::sync::RwLock;
use typed_arena::Arena;

// lazy_static! {
//     /// A shared pool for interned strings.
//     pub static ref STRING_INTERN_POOL: InternPool<String> = InternPool::new();
// }

/// A value interned by an [`InternPool`].
#[derive(Copy, Clone, Eq, PartialEq)]
pub struct Intern<T> {
    ptr: *const T,
}

// /// We know that interned strings are all allocated in [`STRING_INTERN_POOL`].
// impl fmt::Debug for Intern<String> {
//     fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
//         STRING_INTERN_POOL.pool.read().unwrap().values[self.index].fmt(f)
//     }
// }

// /// We know that interned strings are all allocated in [`STRING_INTERN_POOL`].
// impl fmt::Display for Intern<String> {
//     fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
//         STRING_INTERN_POOL.pool.read().unwrap().values[self.index].fmt(f)
//     }
// }

/// Responsible for [interning strings][1]. Interning strings allows us to save memory for many,
/// small, identical strings by only storing the string once and then using a small reference in the
/// string’s place. Also makes it faster to compare small string equality.
///
/// Currently, we don’t expose a way to construct `InternPool`s. Use one of the pre-constructed
/// `InternPool`s like [`STRING_INTERN_POOL`]. This allows specific instantiations of [`Intern`]
/// like `Intern<String>` to have instances for [`Display`], for instance, since we know the pool
/// they were allocated in.
///
/// While mostly designed to be used with strings, we can intern anything that implements [`Eq`],
/// [`Hash`], and [`Clone`].
///
/// Implementation heavily inspired by the [rustc string interner][2].
///
/// An `InternPool` may be shared across threads.
///
/// [1]: https://en.wikipedia.org/wiki/String_interning
/// [2]: https://github.com/rust-lang/rust/blob/b244f61b77c42d7be695afd7901ee4418559e518/src/bootstrap/cache.rs#L162-L165
pub struct InternPool<T>
where
    T: Clone + Eq + Hash,
{
    pool: RwLock<InternPoolData<T>>,
}

/// Shared data for an [`InternPool`].
struct InternPoolData<T>
where
    T: Clone + Eq + Hash,
{
    /// The interned values. We use an [`Arena`] as data allocated in an arena never moves.
    values: Arena<T>,
    /// Associates values with their position in the `values` list.
    set: HashMap<InternRef<T>, Intern<T>>,
}

impl<T> InternPool<T>
where
    T: Clone + Eq + Hash,
{
    /// Create an empty intern pool.
    fn new() -> Self {
        InternPool {
            pool: RwLock::new(InternPoolData {
                values: Arena::new(),
                set: HashMap::new(),
            }),
        }
    }

    /// Interns a value.
    pub fn intern(&self, value: T) -> Intern<T> {
        if let Some(intern) = self.pool.read().unwrap().set.get(&value) {
            return intern.clone();
        }
        let mut pool = self.pool.write().unwrap();
        let intern = Intern {
            index: pool.values.len(),
            phantom: PhantomData,
        };
        pool.set.insert(value.clone(), intern.clone());
        pool.values.push(value);
        intern
    }
}

/// Internal reference to an item used only within the [`InternPool`] itself to encapsulate the
/// unsafe behavior of interior references.
///
/// Different from [`Intern`] in that equality and hashing are based on the data this references and
/// not the pointer itself.
#[derive(Debug, Copy, Clone, Eq)]
struct InternRef<T>
where
    T: Eq + Hash,
{
    ptr: *const T,
}

impl<T> InternRef<T>
where
    T: Eq + Hash,
{
    /// Creates an `InternRef` from a reference.
    fn from_ref(value: &T) -> Self {
        InternRef {
            ptr: value as *const T,
        }
    }

    /// Reinterprets this `InternRef` as a reference.
    ///
    /// This is “safe” as long as this `InternRef` only refers to values that outlive this instance
    /// or the instance that owns this `InternRef`.
    ///
    /// Does not allocate memory!
    fn as_ref(&self) -> &T {
        unsafe { &*self.ptr }
    }
}

impl<T> Hash for InternRef<T>
where
    T: Eq + Hash,
{
    fn hash<H: Hasher>(&self, state: &mut H) {
        self.as_ref().hash(state)
    }
}

impl<T> PartialEq for InternRef<T>
where
    T: Eq + Hash,
{
    fn eq(&self, other: &Self) -> bool {
        self.as_ref() == other.as_ref()
    }
}
