use colored::*;
use std::{
    fmt,
    ops::{Deref, DerefMut},
    ptr::NonNull,
};

use super::trace::Traced;
use crate::debug::LOG_OBJECT;

/// Holds a pointer to some Gc object. Used by the garbage collector.
pub struct Gc<T: ?Sized> {
    /// Pointer to a struct containing the marker, which contains the actual data.
    pub(crate) ptr: NonNull<Traced<T>>,
}

impl<T: fmt::Debug> Gc<T> {
    /// Create a new `Gc<T>` pointer, called by the GC itself.
    pub(super) fn new(ptr: &mut Traced<T>) -> Self {
        let s = Self {
            ptr: NonNull::new(ptr).unwrap(),
        };
        if LOG_OBJECT {
            println!(
                "{}\tGc::new() ({}) ({:?}) : {:?}",
                "[OBJECT]".purple(),
                std::any::type_name::<T>(),
                s.ptr.as_ptr(),
                ptr
            );
        }
        s
    }
}

impl<T> Gc<T> {
    /// Returns a reference to the inner type `T`.
    pub fn as_ref(&self) -> &T {
        unsafe { &self.ptr.as_ref().data }
    }

    /// Returns a mutable reference to the inner type `T`.
    pub fn as_mut(&mut self) -> &mut T {
        unsafe { &mut self.ptr.as_mut().data }
    }

    /// Re-implemenation of marked on `Traced<T>`, returns if this
    /// object has been marked as reachable or not.
    pub(super) fn marked(&self) -> bool {
        unsafe { self.ptr.as_ref().marked() }
    }

    /// Re-implemenation of `set_mark` on `Traced<T>`. Sets this object
    /// as being reachable or not.
    pub(super) fn set_mark(&mut self, val: bool) {
        unsafe {
            self.ptr.as_mut().set_mark(val);
        }
    }
}

impl<T> Copy for Gc<T> {}

impl<T> Clone for Gc<T> {
    fn clone(&self) -> Self {
        *self
    }
}

impl<T> Deref for Gc<T> {
    type Target = T;

    fn deref(&self) -> &Self::Target {
        self.as_ref()
    }
}

impl<T> DerefMut for Gc<T> {
    fn deref_mut(&mut self) -> &mut Self::Target {
        self.as_mut()
    }
}

impl<T: fmt::Debug> fmt::Debug for Gc<T> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "Gc {{ ptr: {:?} }}", self.ptr.as_ptr())
    }
}
