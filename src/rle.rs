use std::cmp::PartialEq;

#[derive(Debug, Default, Clone)]
pub struct RLE<T: PartialEq> {
    data: Vec<(T, usize)>,
}

impl<T: PartialEq> RLE<T> {
    pub fn new() -> Self {
        Self { data: Vec::new() }
    }

    pub fn push(&mut self, item: T) {
        if let Some(last) = self.data.last_mut() {
            if last.0 == item {
                last.1 += 1;
                return;
            }
        }
        self.data.push((item, 1));
    }

    pub fn get(&self, index: usize) -> Option<&T> {
        let mut index = index;
        for (item, count) in self.data.iter() {
            if index > *count {
                index -= count;
            } else {
                return Some(item);
            }
        }
        None
    }
}
