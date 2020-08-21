use colored::*;
use std::cell::Cell;
use std::fmt::Debug;
use std::ops::{Deref, DerefMut};
use std::ptr::NonNull;

use crate::debug::LOG_OBJECT;

#[derive(Debug, Clone)]
pub struct Traced<T: Debug> {
    marked: Cell<bool>,
    pub data: T,
}

impl<T: Debug> Traced<T> {
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
#[derive(Debug, Copy, Clone)]
pub struct Gc<T: Debug> {
    pub ptr: NonNull<Traced<T>>,
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
        &self.get()
    }
}

impl<T: Debug> DerefMut for Gc<T> {
    fn deref_mut(&mut self) -> &mut Self::Target {
        self.get_mut()
    }
}
