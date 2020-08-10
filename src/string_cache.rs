use std::collections::HashMap;

pub struct StringCache {
    cache: Vec<String>,
    lookup: HashMap<String, usize>,
}

impl StringCache {
    pub fn new() -> Self {
        Self {
            cache: Vec::new(),
            lookup: HashMap::new(),
        }
    }

    pub fn get(&self, index: usize) -> Option<&str> {
        self.cache.get(index).map(|s| s.as_str())
    }

    pub fn cache(&mut self, s: String) -> usize {
        if let Some(value) = self.lookup.get(&s).copied() {
            value
        } else {
            let index = self.cache.len();
            self.lookup.insert(s.to_owned(), index);
            self.cache.push(s);
            index
        }
    }
}
