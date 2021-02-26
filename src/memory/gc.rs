use colored::*;
use std::collections::HashMap;

use super::{
    object::{Function, Upvalue},
    TracedObject,
};
use super::{Gc, Traced};
use crate::compiler::compiler::FunctionState;
use crate::debug::{LOG_GC, STRESS_GC};
use crate::vm::{value::Value, CallFrame};

const DEFAULT_NEXT_GC: usize = 1024 * 1024;
const HEAP_GROW_FACTOR: usize = 2;

// enum GCObject {
//     String(Box<Traced<String>>),
//     NativeFn(Box<Traced<NativeFn>>),
//     Function(Box<Traced<Function>>),
//     Closure(Box<Traced<Closure>>),
//     Upvalue(Box<Traced<Upvalue>>),
//     Class(Box<Traced<Class>>),
//     Instance(Box<Traced<Instance>>),
//     BoundMethod(Box<Traced<BoundMethod>>),
// }

/// Mark and sweep garbage collector.
///
/// Uses a tri-color abstracton. Objects start out as white, everything in the stack, globals etc are then marked
/// as gray. Then all the objects reachable by those are found, the already processed objects become black. Afterwards
/// it proceeds with sweeping all objects still left as white.
///
/// Currently it is a bit messy, as the GC "owns" all these arrays and maps used by the compiler and VM. This should be
/// fixed later, but getting it to work first and foremost.
pub struct GC {
    /// All values on the stack, used by the VM.
    pub stack: Vec<Value>,

    /// All global values, used by the VM.
    pub globals: HashMap<String, Value>,

    /// CallFrames used by the VM.
    pub call_frames: Vec<CallFrame>,

    /// Functions currently being compiled.
    pub functions: Vec<FunctionState>,

    /// Functions that have been compiled.
    pub compiled_fns: Vec<Gc<Function>>,

    /// Open upvalues used by the VM.
    pub open_upvalues: Vec<Gc<Upvalue>>,

    /// All objects tracked by the GC, excluding strings.
    objects: Vec<Box<dyn TracedObject>>,

    /// All strings are interned, the GC also keep track of these.
    interned_strings: HashMap<String, Box<Traced<String>>>,

    /// The list of all objects that have recently been reached, either by marking roots, or by tracking their references.
    /// The items in this list are then blackened and removed on each collection cycle.
    gray_list: Vec<Value>,

    /// The total amount of bytes Gc so far.
    bytes_allocated: usize,

    /// When `bytes_Gc` reaches this amount the GC starts collecting.
    next_gc: usize,
}

impl GC {
    pub fn new() -> Self {
        Self {
            objects: Vec::new(),
            interned_strings: HashMap::new(),
            gray_list: Vec::new(),
            stack: Vec::new(),
            globals: HashMap::new(),
            call_frames: Vec::new(),
            functions: Vec::new(),
            compiled_fns: Vec::new(),
            open_upvalues: Vec::new(),
            bytes_allocated: 0,
            next_gc: DEFAULT_NEXT_GC,
        }
    }

    pub fn track_string(&mut self, object: String) -> Gc<String> {
        self.on_track(std::mem::size_of::<String>());
        let string = self
            .interned_strings
            .entry(object.clone())
            .or_insert_with(|| Box::new(Traced::new(object)));
        Gc::new(string)
    }

    // pub fn track_string_root(&mut self, object: String) -> Root<'_, Gc<String>> {
    //     let obj = self.track_string(object);
    //     Root::new(self, obj)
    // }

    // pub fn remove_root(&mut self) {
    //     todo!()
    // }

    /// Helper to avoid lifetime issues, when in the compiler we want to
    /// track a newly finished closure.
    pub fn track_last_fn(&mut self) -> Gc<Function> {
        let fun = self.functions.last().unwrap().function.clone();
        self.track(fun)
    }

    /// Starts tracking the object `T`. Returning a pointer in the form of `Gc<T>` which
    /// is tracked by the garbage collector, and freed when all references to it are gone.
    pub fn track<T>(&mut self, object: T) -> Gc<T>
    where
        Traced<T>: TracedObject + 'static,
        Box<Traced<T>>: Clone,
        T: std::fmt::Debug,
    {
        self.on_track(std::mem::size_of::<T>());
        let mut object = Box::new(Traced::new(object));
        let gcd = Gc::new(&mut object);
        self.objects.push(object);
        gcd
    }

    fn on_track(&mut self, allocated: usize) {
        if STRESS_GC {
            self.collect();
        }
        self.bytes_allocated += allocated;
        if self.bytes_allocated > self.next_gc {
            self.collect();
        }
    }

    fn on_sweep(&mut self, deallocated: usize) {
        self.bytes_allocated -= deallocated;
    }

    /// Traces through all objects tracked by the garbage collector and determines which ones
    /// can be reached. The objects that cannot be reached are freed from memory.
    fn collect(&mut self) {
        let before = self.bytes_allocated;
        if LOG_GC {
            println!("{}", "[GC]\t\tBEGIN".cyan());
        }

        self.mark_roots();
        self.trace_references();
        self.sweep();

        // Adjust when the GC should run next.
        self.next_gc = self.bytes_allocated * HEAP_GROW_FACTOR;

        if LOG_GC {
            println!(
                "{}\t\tCollected {} bytes (from {} to {}) next at {}",
                "[GC]".cyan(),
                before - self.bytes_allocated,
                before,
                self.bytes_allocated,
                self.next_gc
            );
            println!("{}", "[GC]\t\tEND".cyan());
        }
    }

    /// Goes through all the roots and marks those objects, as well as adding them to the gray list
    /// whose elements are later traced through.
    fn mark_roots(&mut self) {
        // Temporary clones.

        // Mark stack.
        self.stack.clone().iter().for_each(|v| self.mark_value(*v));

        // Mark globals.
        self.globals
            .clone()
            .values()
            .for_each(|global| self.mark_value(*global));

        // Mark compiler roots.
        // Since the function being compiled isn't tracked yet by the GC
        // we have to go inside and mark the constantly directly.
        let names: Vec<_> = self
            .functions
            .iter()
            .filter_map(|f| f.function.name)
            .collect();
        names.into_iter().for_each(|name| self.mark(name));

        // Mark currently compiling functions in compiler.
        let constants: Vec<Value> = self
            .functions
            .iter()
            .flat_map(|fun| fun.function.chunk.constants.iter().map(|v| *v))
            .collect();
        constants
            .into_iter()
            .for_each(|value: Value| self.mark_value(value));

        // Mark already compiled functions in compiler.
        self.compiled_fns
            .clone()
            .iter()
            .for_each(|fun| self.mark(*fun));

        // Mark closures in the call frames.
        self.call_frames
            .clone()
            .iter()
            .for_each(|frame| self.mark(frame.closure));

        // Mark open upvalues.
        self.open_upvalues
            .clone()
            .iter()
            .for_each(|upvalue| self.mark(*upvalue));
    }

    /// Traces all references that the objects in the gray list has. Goes through
    /// every gray object and marks them as black, while marking the objects they
    /// can reach.
    fn trace_references(&mut self) {
        while let Some(value) = self.gray_list.pop() {
            self.blacken_value(value);
        }
    }

    /// Marks values as reachable if is an object, otherwise it does nothing.
    pub(super) fn mark_value(&mut self, value: Value) {
        match value {
            Value::String(o) => self.mark(o),
            Value::Function(o) => self.mark(o),
            Value::Native(o) => self.mark(o),
            Value::Closure(o) => self.mark(o),
            Value::Upvalue(o) => self.mark(o),
            Value::Class(o) => self.mark(o),
            Value::Instance(o) => self.mark(o),
            Value::BoundMethod(o) => self.mark(o),
            _ => {}
        }
    }

    /// Marks objects as reachable, and adds them once to the gray list for further processing.
    pub(super) fn mark<T>(&mut self, object: Gc<T>)
    where
        T: std::fmt::Display + std::fmt::Debug,
        Gc<T>: Into<Value>,
    {
        // Using the tri-color abstraction with white, gray and black nodes.
        // If the node is set to gray, we have that as marked being true. If
        // this gets called again the node is black so we should not add it
        // to the gray list.
        if !object.marked() {
            println!(
                "{}\t\tMarking: {} [{:?}]",
                "[GC]".cyan(),
                object.as_ref(),
                object
            );
            object.set_marked(true);
            self.gray_list.push(object.into());
        }
    }

    /// Sweeps all objects left as white, as they cannot be reached any more.
    fn sweep(&mut self) {
        // Sweep interned strings.
        let unmarked_keys: Vec<String> = self
            .interned_strings
            .iter()
            .filter_map(|(key, value)| {
                if !value.marked.get() {
                    Some(key.clone())
                } else {
                    None
                }
            })
            .collect();

        // Since we're using regular strings in a regular hash table we have to remove possible global values that
        // are being sweeped, as they won't get automatically removed.
        unmarked_keys.iter().for_each(|s| {
            self.globals.remove(s);
        });

        for unmarked_key in unmarked_keys {
            if LOG_GC {
                println!(
                    "{}\t\t[Sweep interned string] {}",
                    "[GC]".cyan(),
                    unmarked_key
                );
            }
            self.on_sweep(std::mem::size_of::<String>());
            self.interned_strings.remove(&unmarked_key);
        }

        // Sweep regular objects.
        let mut i = 0;
        while i < self.objects.len() {
            if !self.objects.get(i).unwrap().marked() {
                if LOG_GC {
                    // println!(
                    //     "{}\t\t[Sweep object] {} [{:?}]",
                    //     "[GC]".cyan(),
                    //     self.objects,
                    //     self.objects
                    // );
                }
                let removed = self.objects.swap_remove(i);
                let size = removed.size();
                // let size = match removed.data {
                //     Object::Function(_) => std::mem::size_of::<Function>(),
                //     // Object::Native(_) => std::mem::size_of::<NativeFn>(),
                //     Object::Closure(_) => std::mem::size_of::<Closure>(),
                //     Object::Upvalue(_) => std::mem::size_of::<Upvalue>(),
                //     // Object::Class(_) => std::mem::size_of::<Class>(),
                //     // Object::Instance(_) => std::mem::size_of::<Instance>(),
                //     // Object::BoundMethod(_) => std::mem::size_of::<BoundMethod>(),
                //     // Object::String(_) => panic!("Should never encounter a string here"),
                // };
                self.on_sweep(size);
            // Don't increment i as we swap the last element to this location.
            } else {
                self.objects[i].set_marked(false);
                i += 1;
            }
        }
    }

    /// Finishes processing of a gray value, these should all be objects by now. So it gets
    /// the enclosed types and calls blacken on it.
    fn blacken_value(&mut self, value: Value) {
        match value {
            Value::String(_) => {}
            Value::Native(o) => self.blacken(o),
            Value::Function(o) => self.blacken(o),
            Value::Closure(o) => self.blacken(o),
            Value::Upvalue(o) => self.blacken(o),
            Value::Class(o) => self.blacken(o),
            Value::Instance(o) => self.blacken(o),
            Value::BoundMethod(o) => self.blacken(o),
            _ => panic!("Unexpected variant in blacken"),
        }
    }

    /// Finishes the processing of a gray object, will mark other objects that are reachable
    /// by the object.
    fn blacken<T>(&mut self, mut object: Gc<T>)
    where
        T: std::fmt::Display + std::fmt::Debug,
        Traced<T>: TracedObject,
    {
        if LOG_GC {
            println!(
                "{}\t\tBlacken: {} [{:?}]",
                "[GC]".cyan(),
                object.as_ref(),
                object
            );
        }
        object.traced().blacken(self);
    }
}
