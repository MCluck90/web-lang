use std::{collections::HashMap, hash::Hash};

/// Manage scope-based bindings.
/// Assumes that items from parent scopes are available to child scopes.
pub struct Scope<K: PartialEq + Eq + Hash, V> {
    tables: Vec<HashMap<K, V>>,
}

impl<K: PartialEq + Eq + Hash, V> Default for Scope<K, V> {
    fn default() -> Self {
        Self {
            tables: vec![HashMap::new()],
        }
    }
}

impl<K: PartialEq + Eq + Hash, V> Scope<K, V> {
    /// Returns a reference to the value corresponding to the key.
    pub fn get(&self, key: &K) -> Option<&V> {
        for table in self.tables.iter().rev() {
            if let Some(value) = table.get(key) {
                return Some(value);
            }
        }
        None
    }

    /// Inserts a value in to the current scope.
    /// If a value had previously been inserted with that key, the old value is returned.
    pub fn insert(&mut self, key: K, value: V) -> Option<V> {
        self.tables.last_mut().unwrap().insert(key, value)
    }

    /// Start a new scope.
    pub fn start_new_scope(&mut self) {
        self.tables.push(HashMap::new());
    }

    /// End the current scope.
    /// Returns the underlying table in case it needs to be used again in the future.
    pub fn end_scope(&mut self) -> HashMap<K, V> {
        if self.tables.len() == 1 {
            panic!("Unbalanced scope creation. Attempted to pop the root scope.");
        }

        self.tables.pop().unwrap()
    }
}

impl<K: PartialEq + Eq + Hash, V: Copy> Scope<K, V> {
    /// Returns a copy of the value corresponding to the key.
    pub fn get_copied(&self, key: &K) -> Option<V> {
        for table in self.tables.iter().rev() {
            if let Some(value) = table.get(key) {
                return Some(*value);
            }
        }
        None
    }
}
