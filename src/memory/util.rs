use super::{BoundMethod, Class, Closure, Function, Instance, NativeFn, Upvalue, GC};
use colored::*;
use std::cell::Cell;
use std::fmt::Debug;
use std::ops::{Deref, DerefMut};
use std::ptr::NonNull;

use crate::debug::LOG_OBJECT;

pub trait TracedObject {
    fn marked(&self) -> bool;
    fn set_marked(&mut self, val: bool);
    fn size(&self) -> usize;

    fn blacken(&mut self, _gc: &mut GC) {}
}

impl TracedObject for Traced<NativeFn> {
    fn marked(&self) -> bool {
        self.marked.get()
    }

    fn set_marked(&mut self, val: bool) {
        self.marked.set(val);
    }

    fn size(&self) -> usize {
        std::mem::size_of::<NativeFn>()
    }
}

impl TracedObject for Traced<Class> {
    fn marked(&self) -> bool {
        self.marked.get()
    }

    fn set_marked(&mut self, val: bool) {
        self.marked.set(val);
    }

    fn size(&self) -> usize {
        std::mem::size_of::<Class>()
    }

    fn blacken(&mut self, gc: &mut GC) {
        gc.mark(self.data.name);
        self.data
            .methods
            .values()
            .for_each(|method| gc.mark(*method));
    }
}
impl TracedObject for Traced<Instance> {
    fn marked(&self) -> bool {
        self.marked.get()
    }

    fn set_marked(&mut self, val: bool) {
        self.marked.set(val);
    }

    fn size(&self) -> usize {
        std::mem::size_of::<Instance>()
    }

    fn blacken(&mut self, gc: &mut GC) {
        gc.mark(self.data.class);
        self.data
            .fields
            .values()
            .for_each(|value| gc.mark_value(*value));
    }
}
impl TracedObject for Traced<BoundMethod> {
    fn marked(&self) -> bool {
        self.marked.get()
    }

    fn set_marked(&mut self, val: bool) {
        self.marked.set(val);
    }

    fn size(&self) -> usize {
        std::mem::size_of::<BoundMethod>()
    }
    fn blacken(&mut self, gc: &mut GC) {
        gc.mark(self.data.receiver);
        gc.mark(self.data.closure);
    }
}
impl TracedObject for Traced<Function> {
    fn marked(&self) -> bool {
        self.marked.get()
    }

    fn set_marked(&mut self, val: bool) {
        self.marked.set(val);
    }

    fn size(&self) -> usize {
        std::mem::size_of::<Function>()
    }
    fn blacken(&mut self, gc: &mut GC) {
        // For referenced function we want to first mark the function name, and then
        // everything in the constant list that's used by the code.
        if let Some(name) = self.data.name {
            gc.mark(name);
        }

        self.data.chunk.constants.iter().for_each(|constant| {
            gc.mark_value(constant.clone());
        });
    }
}

impl TracedObject for Traced<Closure> {
    fn marked(&self) -> bool {
        self.marked.get()
    }

    fn set_marked(&mut self, val: bool) {
        self.marked.set(val);
    }

    fn size(&self) -> usize {
        std::mem::size_of::<Closure>()
    }
    fn blacken(&mut self, gc: &mut GC) {
        gc.mark(self.data.function);
        self.data
            .upvalues
            .iter()
            .for_each(|upvalue| gc.mark(*upvalue));
    }
}

impl TracedObject for Traced<Upvalue> {
    fn marked(&self) -> bool {
        self.marked.get()
    }

    fn set_marked(&mut self, val: bool) {
        self.marked.set(val);
    }

    fn size(&self) -> usize {
        std::mem::size_of::<Upvalue>()
    }
    fn blacken(&mut self, gc: &mut GC) {
        match self.data {
            Upvalue::Open(_) => {} // Do nothing.
            Upvalue::Closed(closed) => gc.mark_value(closed),
        }
    }
}

/// Returned by the GC when tracking to temporarily hold on to these
/// values and not collect them until this object has gone out out
/// scope.
// pub struct Root<'a, T: Object2> {
//     gc: &'a mut GC,
//     obj: T,
// }

// impl<'a, T: Object2> Root<'a, T> {
//     pub fn new(gc: &'a mut GC, obj: T) -> Self {
//         Self {
//             gc,
//             obj,
//         }
//     }
// }

// impl<'a, T: Object2> Drop for Root<'a, T> {
//     fn drop(&mut self) {
//         self.gc.remove_root();
//     }
// }

#[derive(Debug, Clone)]
pub struct Traced<T: ?Sized> {
    pub(super) marked: Cell<bool>,
    pub data: T,
}

impl<T> Traced<T> {
    pub(super) fn new(data: T) -> Self {
        Self {
            marked: Cell::new(false),
            data,
        }
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

    pub(super) fn marked(&self) -> bool {
        unsafe { self.ptr.as_ref().marked.get() }
    }

    pub(super) fn set_marked(&self, val: bool) {
        unsafe {
            self.ptr.as_ref().marked.set(val);
        }
    }

    pub(super) fn traced(&mut self) -> &mut Traced<T> {
        unsafe { self.ptr.as_mut() }
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
