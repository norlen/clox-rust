use colored::*;
use std::{cell::RefCell, collections::HashMap, fmt, ptr::NonNull, rc::Rc};

use super::{object::Object, trace::Traced, Gc};
use crate::{
    compiler::compiler::Compiler,
    debug::{LOG_GC, STRESS_GC},
    vm::{value::Value, vm::VM},
};

const DEFAULT_NEXT_GC: usize = 1024 * 1024;
const HEAP_GROW_FACTOR: usize = 2;

/// Mark and sweep garbage collector.
///
/// Uses a tri-color abstracton. Objects start out as white, everything in the stack, globals etc are then marked
/// as gray. Then all the objects reachable by those are found, the already processed objects become black. Afterwards
/// it proceeds with sweeping all objects still left as white.
///
/// Currently it is a bit messy, as the GC "owns" all these arrays and maps used by the compiler and VM. This should be
/// fixed later, but getting it to work first and foremost.
pub struct GC {
    /// Pointer to the compiler, so the compiler roots can be retrieved.
    pub compiler: Option<Rc<RefCell<Compiler>>>,

    /// Pointer to the VM, so the GC can grab the roots used inside the VM.
    pub vm: Option<Rc<RefCell<VM>>>,

    /// All objects tracked by the GC, excluding strings.
    objects: Vec<Box<Traced<dyn Object>>>,

    /// All strings are interned, the GC also keep track of these.
    interned_strings: HashMap<String, Box<Traced<String>>>,

    /// The list of all objects that have recently been reached, either by marking roots, or by tracking their references.
    /// The items in this list are then blackened and removed on each collection cycle.
    gray_list: Vec<NonNull<Traced<dyn Object>>>,

    /// The total amount of bytes Gc so far.
    bytes_allocated: usize,

    /// When `bytes_Gc` reaches this amount the GC starts collecting.
    next_gc: usize,
}

impl GC {
    pub fn new() -> Self {
        Self {
            compiler: None,
            vm: None,
            objects: Vec::new(),
            interned_strings: HashMap::new(),
            gray_list: Vec::new(),
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

    /// Starts tracking the object `T`. Returning a pointer in the form of `Gc<T>` which
    /// is tracked by the garbage collector, and freed when all references to it are gone.
    pub fn track<T>(&mut self, object: T) -> Gc<T>
    where
        T: Object + 'static,
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
        if let Some(vm) = self.vm.clone() {
            let vm = vm.borrow();

            // Mark stack.
            vm.stack.clone().iter().for_each(|v| self.mark_value(*v));

            // Mark globals.
            vm.globals
                .clone()
                .values()
                .for_each(|global| self.mark_value(*global));

            // Mark closures in the call frames.
            vm.call_frames
                .clone()
                .iter()
                .for_each(|frame| self.mark(frame.closure));

            // Mark open upvalues.
            vm.open_upvalues
                .clone()
                .iter()
                .for_each(|upvalue| self.mark(*upvalue));
        }

        if let Some(compiler) = self.compiler.clone() {
            let compiler = compiler.borrow();

            // Mark compiler roots.
            // Since the function being compiled isn't tracked yet by the GC
            // we have to go inside and mark the constantly directly.
            let names: Vec<_> = compiler
                .functions
                .iter()
                .filter_map(|f| f.function.name)
                .collect();
            names.into_iter().for_each(|name| self.mark(name));

            // Mark currently compiling functions in compiler.
            let constants: Vec<Value> = compiler
                .functions
                .iter()
                .flat_map(|fun| fun.function.chunk.constants.iter().map(|v| *v))
                .collect();
            constants
                .into_iter()
                .for_each(|value: Value| self.mark_value(value));

            // Mark already compiled functions in compiler.
            compiler
                .compiled_fns
                .clone()
                .iter()
                .for_each(|fun| self.mark(*fun));
        }
    }

    /// Traces all references that the objects in the gray list has. Goes through
    /// every gray object and marks them as black, while marking the objects they
    /// can reach.
    fn trace_references(&mut self) {
        while let Some(object) = self.gray_list.pop() {
            // Finishes the processing of a gray object, will mark other objects that are reachable
            // by the object.
            unsafe {
                if LOG_GC {
                    println!(
                        "{}\t\tBlacken: {:?} [{:?}]",
                        "[GC]".cyan(),
                        object.as_ref(),
                        object.as_ptr()
                    );
                }
                object.as_ref().data.trace_references(self);
            }
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
    pub(super) fn mark<T>(&mut self, mut object: Gc<T>)
    where
        T: Object + fmt::Display + fmt::Debug + 'static,
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
            object.set_mark(true);
            self.gray_list.push(object.ptr);
        }
    }

    /// Sweeps all objects left as white, as they cannot be reached any more.
    fn sweep(&mut self) {
        // Sweep interned strings.
        let unmarked_keys: Vec<String> = self
            .interned_strings
            .iter()
            .filter_map(|(key, value)| {
                if !value.marked() {
                    Some(key.clone())
                } else {
                    None
                }
            })
            .collect();

        if let Some(vm) = self.vm.clone() {
            let mut vm = vm.borrow_mut();

            // Since we're using regular strings in a regular hash table we have to remove possible global values that
            // are being sweeped, as they won't get automatically removed.
            unmarked_keys.iter().for_each(|s| {
                vm.globals.remove(s);
            });
        }

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

        // Sweep regular objects, don't use iterators as we will modify the vec.
        let mut i = 0;
        while i < self.objects.len() {
            if let Some(object) = self.objects.get(i) {
                let object = object.as_ref();
                if !object.marked() {
                    // if LOG_GC {
                    //     println!(
                    //         "{}\t\t[Sweep object] {} [{:?}]",
                    //         "[GC]".cyan(),
                    //         self.objects,
                    //         self.objects
                    //     );
                    // }
                    let removed = self.objects.swap_remove(i);
                    self.on_sweep(removed.as_ref().data.size());
                    // Don't increment i as we swap the last element to this location.
                } else {
                    self.objects[i].set_mark(false);
                    i += 1;
                }
            }
        }
    }
}
