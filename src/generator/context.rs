use std::{cell::{Cell, RefCell}, collections::HashMap};

// TODO currently not thread-safe
pub struct Context {
    label_counter: Cell<u32>,
    var_map: RefCell<HashMap<String, i32>>,
    stack_index: Cell<i32>,
}

impl Context {
    pub fn new() -> Context {
        Context { 
            label_counter: Cell::new(0),
            var_map: RefCell::new(HashMap::new()),
            stack_index: Cell::new(0),
        }
    }

    pub fn get_and_increase_label(&self) -> String {
        self.label_counter.set(self.label_counter.get() + 1);
        format!("_{}", self.label_counter.get())
    }

    pub fn declare_var(&self, var_name: &str) {
        let var_stack_index = self.stack_index.get() - 8;
        self.stack_index.set(var_stack_index);
        let mut map = self.var_map.borrow_mut();
        if let Some(_) = map.get(var_name) {
            panic!("Variable name already exists")
        }
        map.insert(String::from(var_name), var_stack_index);
    }

    pub fn get_var_stack_index(&self, var_name: &str) -> i32 {
        if let Some(i) = self.var_map.borrow().get(var_name) {
            *i
        } else {
            panic!("Variable name not found")
        }

    }
}
