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

fn main() -> Result<(), Box<dyn Error>> {
    let mut vm = vm::VM::new();
    repl(&mut vm)
}

fn repl(vm: &mut vm::VM) -> Result<(), Box<dyn Error>> {
    let stdin = io::stdin();
    for line in stdin.lock().lines() {
        vm.interpret(&line?)?;
    }
    Ok(())
}

fn run_file(vm: &mut vm::VM, path: &Path) -> Result<(), Box<dyn Error>> {
    let contents = fs::read_to_string(path)?;
    vm.interpret(&contents)?;

    Ok(())
}
