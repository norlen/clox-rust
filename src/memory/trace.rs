use std::{cell::Cell, fmt};

#[derive(Debug)]
pub struct Traced<T: ?Sized> {
    marked: Cell<bool>,
    pub(super) data: T,
}

impl<T> Traced<T> {
    /// Creates a new `Traced` with `marked` set to `false`.
    pub(super) fn new(data: T) -> Self {
        Self {
            marked: Cell::new(false),
            data,
        }
    }
}

impl<T: ?Sized> Traced<T> {
    /// Sets the reachable mark to `true` or `false`.
    pub(super) fn set_mark(&mut self, value: bool) {
        self.marked.set(value);
    }

    /// Returns if the marked status, i.e. if it has been marked as reachable
    /// or not.
    pub(super) fn marked(&self) -> bool {
        self.marked.get()
    }
}

impl<T: fmt::Display> fmt::Display for Traced<T> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "Traced [marked={}] {}", self.marked.get(), &self.data)
    }
}
