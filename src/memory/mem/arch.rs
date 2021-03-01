use super::gc::Gc;
use super::objects::Object;
use super::trace::Traced;
use super::Function;
use super::Root;
use crate::vm::value::Value;
use std::cell::RefCell;
use std::ptr::NonNull;
use std::rc::Rc;

fn run() {
    let gc = Rc::new(RefCell::new(GC::new()));
    let compiler = Rc::new(RefCell::new(Compiler::new(gc.clone())));
    let vm = Rc::new(RefCell::new(VM::new(gc.clone())));

    gc.borrow_mut().compiler = Some(compiler);
    gc.borrow_mut().vm = Some(vm);
}

pub struct GC {
    pub compiler: Option<Rc<RefCell<Compiler>>>,
    pub vm: Option<Rc<RefCell<VM>>>,

    objects: Vec<Box<Traced<dyn Object>>>,

    temp_roots: Vec<NonNull<Traced<dyn Object>>>,

    gray_list: Vec<NonNull<Traced<dyn Object>>>,
}

pub struct Compiler {
    gc: Rc<RefCell<GC>>,
    functions: Vec<Function>,
}

impl Compiler {
    pub fn new(gc: Rc<RefCell<GC>>) -> Self {
        Self {
            gc,
            functions: Vec::new(),
        }
    }
}

pub struct VM {
    gc: Rc<RefCell<GC>>,
    stack: Vec<Value>,
}

impl VM {
    pub fn new(gc: Rc<RefCell<GC>>) -> Self {
        Self {
            gc,
            stack: Vec::new(),
        }
    }
}

impl GC {
    pub fn new() -> Self {
        Self {
            compiler: None,
            vm: None,
            objects: Vec::new(),
            temp_roots: Vec::new(),
            gray_list: Vec::new(),
        }
    }

    pub fn track<T>(&mut self, object: T) -> Gc<T>
    where
        T: Object + 'static,
    {
        let mut object = Box::new(Traced::new(object));
        let gcd = Gc::new(object.as_mut());
        self.objects.push(object);
        gcd
    }

    pub fn track_root<T>(&mut self, object: T) -> Root<T>
    where
        T: Object + 'static,
    {
        let mut object = Box::new(Traced::new(object));
        let gcd = Gc::new(object.as_mut());
        self.objects.push(object);
        self.temp_roots.push(gcd.ptr);
        Root { data: gcd }
    }

    pub fn mark(&mut self, object: NonNull<Traced<dyn Object>>) {
        // println!("{:?}", object);
        self.gray_list.push(object);
        todo!()
    }

    pub fn mark_roots(&mut self) {
        // let mut mark = |obj| self.gray_list.push(obj);

        for root in self.temp_roots.iter().copied() {
            self.gray_list.push(root);
        }

        if let Some(compiler) = &self.compiler {
            for f in compiler.borrow().functions.iter() {
                if let Some(name) = f.name {
                    self.gray_list.push(name.ptr)
                }
            }
        }
    }

    pub fn mark_value(&mut self, _value: Value) {
        todo!()
    }
}
