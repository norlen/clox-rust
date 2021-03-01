use super::arch::*;
use super::*;

pub trait Object {
    fn blacken(&self, gc: &mut GC);
}

impl Object for String {
    fn blacken(&self, _gc: &mut GC) {}
}

impl Object for Function {
    fn blacken(&self, gc: &mut GC) {
        // For referenced function we want to first mark the function name, and then
        // everything in the constant list that's used by the code.
        if let Some(name) = self.name {
            gc.mark(name.ptr);
        }

        self.chunk.constants.iter().for_each(|constant| {
            gc.mark_value(constant.clone());
        });
    }
}
