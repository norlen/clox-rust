# clox-rust

Work in progress implementing the [lox](https://craftinginterpreters.com/a-bytecode-virtual-machine.html) bytecode compiler and virtual machine.

Main differences currently from the [reference implementation](https://github.com/munificent/craftinginterpreters) is the `Object`s are all separate values so the types can be checked and there are no global variables.
The messiest part is probably the GC which should be refactored.

# Usage

For computationally intensive programs it will be really slow without the `release` flag.

## Build

`cargo build`

## Run

`cargo run -- <file>` to run a file, otherwise `cargo run` will start it a REPL.

# Progress

- [x] 14. Chunks of Bytecode
- [x] 15. A Virtual Machine
- [x] 16. Scanning on Demand
- [x] 17. Compiling Expressions
- [x] 18. Types of Values
- [x] 19. Strings
- [ ] 20. ~~Hash Tables~~ using `std::collections::HashMap` instead
- [x] 21. Global Variables
- [x] 22. Local Variables
- [x] 23. Jumping Back and Forth
- [x] 24. Calls and Functions
- [x] 25. Closures
- [x] 26. Garbage Collection
- [x] 27. Classes and Instances
- [ ] 28. Methods and Initializers
- [ ] 29. Superclasses
- [ ] 30. Optimization
