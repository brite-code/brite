use super::vecn::Vec1;
use std::collections::HashMap;
use std::hash::Hash;

/// A data structure for efficient manipulation of a binding list with shadowed variables in
/// imperative code.
///
/// For example, in the lambda calculus you might have `λx.( ... (λy.λx.pair x y) ... )`. We need an
/// efficient way to resolve `x` and `y`. We also may need to access the shadowed `x` again after
/// the deeper `x` binding goes out of scope.
///
/// This data structure provides that service.
pub struct BindingMap<Key: Hash + Eq, Value> {
    stack: Vec1<HashMap<Key, Value>>,
}

impl<Key: Hash + Eq, Value> BindingMap<Key, Value> {
    /// Creates a new, empty, binding map.
    pub fn new() -> Self {
        BindingMap {
            stack: Vec1::new(HashMap::new()),
        }
    }

    /// Adds a level of nesting to our binding map for the duration of a function’s execution. When
    /// the function finishes executing any bindings inserted while executing are removed from
    /// the map.
    ///
    /// Safely calls [`BindingMap::manual_nest`] and [`BindingMap::manual_unnest`].
    #[allow(dead_code)]
    pub fn nest<T>(&mut self, f: impl FnOnce(&mut Self) -> T) -> T {
        self.manual_nest();
        let x = f(self);
        self.manual_unnest();
        x
    }

    /// Manually adds a level of nesting to the binding map. Must call [`BindingMap::manual_unnest`]
    /// afterwards for each time that you nest. Whenever possible, prefer to
    /// use [`BindingMap::nest`].
    ///
    /// Incorrect manual nesting will cause very difficult to debug problems! We only provide manual
    /// nesting support since managing ownership when a binding map is embedded in another struct
    /// is cumbersome.
    pub fn manual_nest(&mut self) {
        self.stack.push(HashMap::new());
    }

    /// Manually removes a level of nesting from the binding map. Must only be called after a
    /// [`BindingMap::manual_nest`]. Whenever possible, prefer to use [`BindingMap::nest`].
    ///
    /// Incorrect manual nesting will cause very difficult to debug problems! We only provide manual
    /// nesting support since managing ownership when a binding map is embedded in another struct
    /// is cumbersome.
    pub fn manual_unnest(&mut self) {
        self.stack.pop();
    }

    /// Insert a new binding into the map. If we are inside of a [`BindingMap::nest`] then when we
    /// leave that scope the binding will be removed.
    pub fn insert(&mut self, key: Key, value: Value) {
        self.stack.last_mut().insert(key, value);
    }

    /// Get the binding for a key from our bindings map. If the binding does not exist we will
    /// return `None`.
    pub fn get(&self, key: &Key) -> Option<&Value> {
        for map in self.stack.iter().rev() {
            if let Some(value) = map.get(key) {
                return Some(value);
            }
        }
        None
    }

    /// Gets the binding for a key in the current nesting level.
    ///
    /// If a binding exists in the map and then we call [`BindingMap::nest`], then this function
    /// will return false when asked if that binding exists.
    pub fn get_shallow(&self, key: &Key) -> Option<&Value> {
        self.stack.last().get(key)
    }

    /// Checks if the map contains *any* binding for the provided key.
    #[allow(dead_code)]
    pub fn contains(&self, key: &Key) -> bool {
        for map in self.stack.iter().rev() {
            if map.contains_key(key) {
                return true;
            }
        }
        false
    }

    /// Checks if the current nesting level contains a binding for the provided key.
    ///
    /// If a binding exists in the map and then we call [`BindingMap::nest`], then this function
    /// will return false when asked if that binding exists.
    pub fn contains_shallow(&self, key: &Key) -> bool {
        self.stack.last().contains_key(key)
    }
}
