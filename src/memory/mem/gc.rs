use super::trace::Traced;
use std::fmt;
use std::ptr::NonNull;

pub struct Gc<T: ?Sized> {
    pub(super) ptr: NonNull<Traced<T>>,
}

impl<T> Gc<T> {
    pub fn new(ptr: &mut Traced<T>) -> Self {
        Self {
            ptr: NonNull::new(ptr).unwrap(),
        }
    }
}

impl<T: fmt::Debug> fmt::Debug for Gc<T> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "Gc {{ ptr: {:?} }}", self.ptr.as_ptr())
    }
}

impl<T> Copy for Gc<T> {}

impl<T> Clone for Gc<T> {
    fn clone(&self) -> Self {
        *self
    }
}
