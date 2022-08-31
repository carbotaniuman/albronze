use std::collections::HashMap;
use std::hash::Hash;

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Scope<K: Hash + Eq, V>(Vec<HashMap<K, V>>);

impl<K: Hash + Eq, V> Scope<K, V> {
    #[inline]
    pub(crate) fn new() -> Self {
        Self(vec![HashMap::new()])
    }

    #[inline]
    pub(crate) fn enter(&mut self) {
        self.0.push(HashMap::<K, V>::new())
    }

    #[inline]
    pub(crate) fn exit(&mut self) {
        self.0.pop();
    }

    pub(crate) fn get(&self, name: &K) -> Option<&V> {
        self.iter()
            .filter_map(|(key, value)| if key == name { Some(value) } else { None })
            .next()
    }

    // returns whether the _immediate_ scope contains `name`
    #[inline]
    pub(crate) fn insert(&mut self, key: K, value: V) -> Option<V> {
        self.0.last_mut().unwrap().insert(key, value)
    }

    #[inline]
    pub(crate) fn get_immediate(&self, name: &K) -> Option<&V> {
        self.0.last().unwrap().get(name)
    }

    #[inline]
    pub(crate) fn get_all_immediate(&mut self) -> &mut HashMap<K, V> {
        self.0.last_mut().unwrap()
    }

    pub(crate) fn is_global(&self) -> bool {
        self.0.len() == 1
    }

    pub(crate) fn _remove(&mut self, key: &K) -> Option<V> {
        debug_assert!(!self.0.is_empty());
        self.0.last_mut().unwrap().remove(key)
    }

    pub(crate) fn iter(&self) -> impl Iterator<Item = (&K, &V)> {
        self.0.iter().rev().flatten()
    }
}
