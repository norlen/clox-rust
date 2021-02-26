use colored::*;
use std::cell::Cell;
use std::fmt::Debug;
use std::ops::{Deref, DerefMut};
use std::ptr::NonNull;
use super::{GC, Instance, NativeFn, Class, BoundMethod, Function, Closure, Upvalue};

use crate::debug::LOG_OBJECT;

pub trait TracedObject {}

impl TracedObject for Traced<NativeFn> {}
impl TracedObject for Traced<Class> {}
impl TracedObject for Traced<Instance> {}
impl TracedObject for Traced<BoundMethod> {}
impl TracedObject for Traced<Function> {}
impl TracedObject for Traced<Closure> {}
impl TracedObject for Traced<Upvalue> {}

/// Returned by the GC when tracking to temporarily hold on to these
/// values and not collect them until this object has gone out out
/// scope.
pub struct Root<'a, T: Object2> {
    gc: &'a mut GC,
    obj: T,
}

impl<'a, T: Object2> Root<'a, T> {
    pub fn new(gc: &'a mut GC, obj: T) -> Self {
        Self {
            gc,
            obj,
        }
    }
}

impl<'a, T: Object2> Drop for Root<'a, T> {
    fn drop(&mut self) {
        self.gc.remove_root();
    }
}

/// An object is a type that is tracked by the GC.
pub trait Object2 {}
impl Object2 for Gc<String> {}

#[derive(Debug, Clone)]
pub struct Traced<T: ?Sized> {
    marked: Cell<bool>,
    pub data: T,
}

impl<T> Traced<T> {
    pub(super) fn new(data: T) -> Self {
        Self {
            marked: Cell::new(false),
            data,
        }
    }

    pub(super) fn marked(&self) -> bool {
        self.marked.get()
    }

    pub(super) fn mark(&self) {
        self.marked.set(true);
    }

    pub(super) fn unmark(&self) {
        self.marked.set(false);
    }
}

// Holds a pointer to some Gc object. Used by the garbage collector.
#[derive(Debug)]
pub struct Gc<T: ?Sized> {
    pub ptr: NonNull<Traced<T>>,
}

impl<T: Debug> Copy for Gc<T> {}

impl<T: Debug> Clone for Gc<T> {
    fn clone(&self) -> Self {
        *self
    }
}

impl<T: Debug> Gc<T> {
    pub(super) fn new(ptr: &mut Traced<T>) -> Self {
        if LOG_OBJECT {
            println!("{}\tGc::new() : {:?}", "[OBJECT]".purple(), ptr);
        }
        Self {
            ptr: NonNull::new(ptr).unwrap(),
        }
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

    pub fn get(&self) -> &T {
        // Yep!
        unsafe { &self.ptr.as_ref().data }
    }

    pub fn get_mut(&mut self) -> &mut T {
        // Yep again!
        unsafe { &mut self.ptr.as_mut().data }
    }

    pub(super) fn marked(&self) -> bool {
        unsafe { self.ptr.as_ref().marked() }
    }

    pub(super) fn mark(&self) {
        unsafe {
            self.ptr.as_ref().mark();
        }
    }
}

impl<T: Debug> Deref for Gc<T> {
    type Target = T;

    fn deref(&self) -> &Self::Target {
        self.as_ref()
    }
}

impl<T: Debug> DerefMut for Gc<T> {
    fn deref_mut(&mut self) -> &mut Self::Target {
        self.as_mut()
    }
}
