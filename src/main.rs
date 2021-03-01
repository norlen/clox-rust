#![warn(rust_2018_idioms)]

mod compiler;
mod debug;
mod memory;
mod vm;

use std::{
    cell::RefCell,
    error::Error,
    fs,
    io::{self, BufRead},
    path::Path,
    rc::Rc,
};

use compiler::compiler::Compiler;
use memory::GC;
use vm::vm::VM;

fn main() -> Result<(), Box<dyn Error>> {
    let file_path = std::env::args().nth(1);

    let gc = Rc::new(RefCell::new(GC::new()));
    let vm = Rc::new(RefCell::new(VM::new(gc.clone())));
    gc.borrow_mut().vm = Some(vm.clone());

    if let Some(file_path) = file_path {
        run_file(Path::new(&file_path), vm, gc)
    } else {
        repl(vm, gc)
    }
}

fn repl(vm: Rc<RefCell<VM>>, gc: Rc<RefCell<GC>>) -> Result<(), Box<dyn Error>> {
    let stdin = io::stdin();
    for line in stdin.lock().lines() {
        // Temp
        let line = Box::new(line?);
        let line = Box::leak(line);
        interpret(line, vm.clone(), gc.clone())?;
    }
    Ok(())
}

fn run_file(path: &Path, vm: Rc<RefCell<VM>>, gc: Rc<RefCell<GC>>) -> Result<(), Box<dyn Error>> {
    let contents = fs::read_to_string(path)?;
    // Temp
    let contents = Box::new(contents);
    let contents = Box::leak(contents);
    interpret(contents, vm, gc)
}

fn interpret(
    source: &'static str,
    vm: Rc<RefCell<VM>>,
    gc: Rc<RefCell<GC>>,
) -> Result<(), Box<dyn Error>> {
    let compiler = Rc::new(RefCell::new(Compiler::new(source, vm.clone(), gc.clone())));
    gc.borrow_mut().compiler = Some(compiler.clone());
    let function = compiler.borrow_mut().compile()?;

    vm.borrow_mut().interpret_function(function)?;

    Ok(())
}
