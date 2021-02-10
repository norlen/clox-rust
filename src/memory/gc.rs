use colored::*;
use std::collections::HashMap;

use super::object::{Class, Closure, Function, NativeFn, Object, Upvalue};
use super::{Gc, Traced};
use crate::compiler::compiler::FunctionState;
use crate::debug::{LOG_GC, STRESS_GC};
use crate::vm::{value::Value, CallFrame};

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
    /// All values on the stack, used by the VM.
    pub stack: Vec<Value>,

    /// All global values, used by the VM.
    pub globals: HashMap<String, Value>,

    /// CallFrames used by the VM.
    pub call_frames: Vec<CallFrame>,

    /// Functions currently being compiled.
    pub functions: Vec<FunctionState>,

    pub compiled_fns: Vec<Gc<Object>>,

    /// Open upvalues used by the VM.
    pub open_upvalues: Vec<Gc<Object>>,

    /// All objects tracked by the GC, excluding strings.
    objects: Vec<Box<Traced<Object>>>,

    /// All strings are interned, the GC also keep track of these.
    interned_strings: HashMap<String, Box<Traced<Object>>>,

    /// The list of all objects that have recently been reached, either by marking roots, or by tracking their references.
    /// The items in this list are then blackened and removed on each collection cycle.
    gray_list: Vec<Gc<Object>>,

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

    /// Adds a string to the garbage collector.
    pub fn track_string(&mut self, string: String) -> Gc<Object> {
        self.on_track(std::mem::size_of::<String>());
        let string = self
            .interned_strings
            .entry(string.clone())
            .or_insert_with(|| Box::new(Traced::new(Object::String(string))));
        Gc::new(string)
    }

    /// Adds a function to the gargbace collector.
    pub fn track_function(&mut self, function: Function) -> Gc<Object> {
        self.on_track(std::mem::size_of::<Function>());
        self.objects
            .push(Box::new(Traced::new(Object::Function(function))));
        let object = self.objects.last_mut().unwrap();
        Gc::new(object)
    }

    /// Adds a native function to the garbage collector.
    pub fn track_native(&mut self, native_fn: NativeFn) -> Gc<Object> {
        self.on_track(std::mem::size_of::<NativeFn>());
        self.objects
            .push(Box::new(Traced::new(Object::Native(native_fn))));
        let object = self.objects.last_mut().unwrap();
        Gc::new(object)
    }

    /// Adds a closure to the garbage collector.
    pub fn track_closure(&mut self, closure: Closure) -> Gc<Object> {
        self.on_track(std::mem::size_of::<Closure>());
        self.objects
            .push(Box::new(Traced::new(Object::Closure(closure))));
        let object = self.objects.last_mut().unwrap();
        Gc::new(object)
    }

    /// Adds an upvalue to the garbage collector.
    pub fn track_upvalue(&mut self, upvalue: Upvalue) -> Gc<Object> {
        self.on_track(std::mem::size_of::<Upvalue>());
        self.objects
            .push(Box::new(Traced::new(Object::Upvalue(upvalue))));
        let object = self.objects.last_mut().unwrap();
        Gc::new(object)
    }

    /// Adds a class to the garbage collector.
    pub fn track_class(&mut self, class: Class) -> Gc<Object> {
        self.on_track(std::mem::size_of::<Class>());
        self.objects.push(Box::new(Traced::new(Object::Class(class))));
        let object = self.objects.last_mut().unwrap();
        Gc::new(object)
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

    fn mark_roots(&mut self) {
        // Helper to grab the object inside if it exists.
        let filter_objects = |v: &Value| match v {
            Value::Object(obj) => Some(obj.clone()),
            _ => None,
        };

        // Mark stack.
        let stack_objects: Vec<Gc<Object>> = self.stack.iter().filter_map(filter_objects).collect();
        stack_objects
            .iter()
            .for_each(|o| self.mark_object(o.clone()));

        // Mark globals.
        let mut global_objects = Vec::new();
        self.globals.iter().for_each(|(_k, v)| {
            if let Some(obj) = filter_objects(v) {
                global_objects.push(obj);
            }
        });
        self.mark_objects(global_objects.into_iter());

        // Mark compiler roots.
        // Since the function being compiled isn't tracked yet by the GC
        // we have to go inside and mark the constantly directly.
        println!(
            "||GC|| COMPILER ROOTS: NUM FUNCTIONS: {}",
            self.functions.len()
        );
        for f in self.functions.iter() {
            println!("FUNCTION {:?}", f);
        }
        let fn_names: Vec<_> = self
            .functions
            .iter()
            .filter_map(|f| f.function.name.clone())
            .collect();
        self.mark_objects(fn_names.into_iter());

        let compiler_objects: Vec<_> = self
            .functions
            .iter()
            .flat_map(|f| f.function.chunk.constants.iter().filter_map(filter_objects))
            .collect();
        self.mark_objects(compiler_objects.into_iter());

        println!(
            "||GC|| COMPILER ROOTS: NUM COMPILED FUNCTIONS: {}",
            self.compiled_fns.len()
        );
        self.mark_objects(self.compiled_fns.clone().into_iter());

        // let compiler_functions: Vec<_> = self.functions.iter().map(|f| f.function.clone()).collect();
        // self.mark_objects(compiler_functions.into_iter());

        // Mark closures in the call frames.
        let closure_objects: Vec<_> = self
            .call_frames
            .iter()
            .map(|cf| cf.closure.clone())
            .collect();
        self.mark_objects(closure_objects.into_iter());

        // Mark open upvalues.
        self.mark_objects(self.open_upvalues.clone().into_iter());
    }

    /// Traces all references that the objects in the gray list has. Goes through
    /// every gray object and marks them as black, while marking the objects they
    /// can reach.
    fn trace_references(&mut self) {
        while let Some(value) = self.gray_list.pop() {
            self.blacken(value);
        }
    }

    /// Marks values as reachable if is an object, otherwise it does nothing.
    fn mark_value(&mut self, value: Value) {
        match value {
            Value::Object(object) => {
                self.mark_object(object);
            }
            // Only objects are managed by the GC.
            _ => {}
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
                    println!(
                        "{}\t\t[Sweep object] {} [{:?}]",
                        "[GC]".cyan(),
                        self.objects.get(i).unwrap().data,
                        self.objects.get(i).unwrap().data
                    );
                }
                let removed = self.objects.swap_remove(i);
                let size = match removed.data {
                    Object::Function(_) => std::mem::size_of::<Function>(),
                    Object::Native(_) => std::mem::size_of::<NativeFn>(),
                    Object::Closure(_) => std::mem::size_of::<Closure>(),
                    Object::Upvalue(_) => std::mem::size_of::<Upvalue>(),
                    Object::Class(_) => std::mem::size_of::<Class>(),
                    Object::String(_) => panic!("Should never encounter a string here"),
                };
                self.on_sweep(size);
            // Don't increment i as we swap the last element to this location.
            } else {
                self.objects[i].unmark();
                i += 1;
            }
        }
    }

    /// Marks objects as reachable, and adds them once to the gray list for further processing.
    fn mark_object(&mut self, object: Gc<Object>) {
        // Using the tri-color abstraction with white, gray and black nodes.
        // If the node is set to gray, we have that as marked being true. If
        // this gets called again the node is black so we should not add it
        // to the gray list.
        if !object.marked() {
            if LOG_GC {
                println!(
                    "{}\t\tMarking: {} [{:?}]",
                    "[GC]".cyan(),
                    object.get(),
                    object
                );
            }
            object.mark();
            self.gray_list.push(object);
        }
    }

    /// Marks multiple objects as reachable.
    fn mark_objects(&mut self, objects_it: impl Iterator<Item = Gc<Object>>) {
        objects_it.for_each(|o| self.mark_object(o));
    }

    /// Finishes the processing of a gray object, will mark other objects that are reachable
    /// by the object.
    fn blacken(&mut self, object: Gc<Object>) {
        if LOG_GC {
            println!(
                "{}\t\tBlacken: {} [{:?}]",
                "[GC]".cyan(),
                object.get(),
                object
            );
        }
        match object.get() {
            Object::String(_) => return,
            Object::Native(_) => return,
            Object::Function(ref object) => {
                // For referenced function we want to first mark the function name, and then
                // everything in the constant list that's used by the code.
                if let Some(name) = object.name.clone() {
                    self.mark_object(name);
                }
                object.chunk.constants.iter().for_each(|constant| {
                    self.mark_value(constant.clone());
                });
            }
            Object::Closure(ref closure) => {
                self.mark_object(closure.function.clone());
                self.mark_objects(closure.upvalues.clone().into_iter());
            }
            Object::Upvalue(ref upvalue) => {
                match upvalue {
                    Upvalue::Closed(closed) => self.mark_value(closed.clone()),
                    Upvalue::Open(_) => {} // No nothing.
                }
            }
            Object::Class(ref class) => {
                self.mark_object(class.name.clone());
            }
        }
    }
}
