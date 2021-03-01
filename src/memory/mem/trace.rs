use std::cell::Cell;

// pub trait Trace {
//     fn set_mark(&mut self, value: bool);
//     fn marked(&self) -> bool;
//     fn data_size(&self) -> usize;
// }

pub struct Traced<T: ?Sized> {
    marked: Cell<bool>,
    data: T,
}

impl<T> Traced<T> {
    pub fn new(data: T) -> Self {
        Self {
            marked: Cell::new(false),
            data,
        }
    }

    fn set_mark(&mut self, value: bool) {
        self.marked.set(value);
    }

    fn marked(&self) -> bool {
        self.marked.get()
    }

    fn data_size(&self) -> usize {
        std::mem::size_of::<T>()
    }
}

// impl<T> Trace for Traced<T> {
//     fn set_mark(&mut self, value: bool) {
//         self.marked.set(value);
//     }

//     fn marked(&self) -> bool {
//         self.marked.get()
//     }

//     fn data_size(&self) -> usize {
//         std::mem::size_of::<T>()
//     }
// }
