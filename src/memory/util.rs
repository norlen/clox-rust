use colored::*;
use std::fmt::Debug;
use std::ops::{Deref, DerefMut};
use std::ptr::NonNull;

use crate::debug::LOG_OBJECT;

#[derive(Debug, Copy, Clone)]
pub struct Traced<T: Debug> {
    pub(super) marked: bool,
    pub data: T,
}

impl<T: Debug> Traced<T> {
    pub(super) fn new(data: T) -> Self {
        Self {
            marked: false,
            data,
        }
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

    pub fn get(&self) -> &Traced<T> {
        // Yep!
        unsafe { self.ptr.as_ref() }
    }

    pub fn get_mut(&mut self) -> &mut Traced<T> {
        // Yep again!
        unsafe { self.ptr.as_mut() }
    }
}

impl<T: Debug> Deref for Gc<T> {
    type Target = T;

    fn deref(&self) -> &Self::Target {
        &self.get().data
    }
}

impl<T: Debug> DerefMut for Gc<T> {
    fn deref_mut(&mut self) -> &mut Self::Target {
        &mut self.get_mut().data
    }
}