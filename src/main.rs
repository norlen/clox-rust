#![warn(rust_2018_idioms)]
#![allow(dead_code)]

use std::error::Error;
use std::fs;
use std::io::{self, BufRead};
use std::path::Path;

mod chunk;
mod compiler;
mod debug;
mod instruction;
mod rle;
mod scanner;
mod string_cache;
mod token;
mod util;
mod value;
mod vm;
mod gc;
mod object;

use compiler::Compiler;
use vm::VM;
use gc::GC;

fn main() -> Result<(), Box<dyn Error>> {
    let file_path = std::env::args().nth(1);

    let mut gc = GC::new();
    if let Some(file_path) = file_path {
        run_file(Path::new(&file_path), &mut gc)
    } else {
        repl(&mut gc)
    }
}

fn repl(gc: &mut GC) -> Result<(), Box<dyn Error>> {
    let stdin = io::stdin();
    for line in stdin.lock().lines() {
        interpret(&line?, gc)?;
    }
    Ok(())
}

fn run_file(path: &Path, gc: &mut GC) -> Result<(), Box<dyn Error>> {
    let contents = fs::read_to_string(path)?;
    interpret(&contents, gc)
}

fn interpret(source: &str, gc: &mut GC) -> Result<(), Box<dyn Error>> {

    let function = {
        let compiler = Compiler::new(source, gc);
        compiler.compile()?
    };

    let mut vm = VM::new(gc);
    vm.interpret_function(function)?;

    Ok(())
}