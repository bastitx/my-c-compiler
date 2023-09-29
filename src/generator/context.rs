use std::{cell::{Cell, RefCell}, collections::{HashMap, HashSet}, sync::atomic::{AtomicU32, Ordering}};

#[derive(Clone)]
struct FunctionMetadata {
    pub param_len: usize,
    pub defined: bool,
}

// TODO currently not thread-safe
pub struct Context {
    var_map: RefCell<HashMap<String, i32>>,
    fun_map: RefCell<HashMap<String, FunctionMetadata>>,
    stack_index: Cell<i32>,
    current_scope_var: RefCell<HashSet<String>>,
    pub current_break: Option<String>,
    pub current_continue: Option<String>,
    current_scope_stack_size: Cell<u32>,
}

static LABEL_COUNTER: AtomicU32 = AtomicU32::new(0);

impl Context {
    pub fn new(parent: Option<&Context>) -> Context {
        if let Some(parent) = parent {
            Context { 
                var_map: RefCell::new(parent.var_map.borrow().clone()),
                fun_map: RefCell::new(parent.fun_map.borrow().clone()),
                stack_index: Cell::new(parent.stack_index.get().clone()),
                current_scope_var: RefCell::new(HashSet::new()),
                current_break: parent.current_break.clone(),
                current_continue: parent.current_continue.clone(),
                current_scope_stack_size: Cell::new(0),
            }
        } else {
            Context { 
                var_map: RefCell::new(HashMap::new()),
                fun_map: RefCell::new(HashMap::new()),
                stack_index: Cell::new(0),
                current_scope_var: RefCell::new(HashSet::new()),
                current_break: None,
                current_continue: None,
                current_scope_stack_size: Cell::new(0),
            }
        }
    }

    pub fn new_loop(parent: &Context, current_break: Option<String>, current_continue: Option<String>) -> Context {
        Context { 
            var_map: RefCell::new(parent.var_map.borrow().clone()),
            fun_map: RefCell::new(parent.fun_map.borrow().clone()),
            stack_index: Cell::new(parent.stack_index.get().clone()),
            current_scope_var: RefCell::new(HashSet::new()),
            current_break: current_break,
            current_continue: current_continue,
            current_scope_stack_size: Cell::new(0),
        }
    }

    pub fn new_function(parent: &Context) -> Context {
        Context { 
            var_map: RefCell::new(parent.var_map.borrow().clone()),
            fun_map: RefCell::new(parent.fun_map.borrow().clone()),
            stack_index: Cell::new(0),
            current_scope_var: RefCell::new(HashSet::new()),
            current_break: None,
            current_continue: None,
            current_scope_stack_size: Cell::new(0),
        }
    }

    pub fn get_and_increase_label(&self) -> String {
        let label = LABEL_COUNTER.fetch_add(1, Ordering::SeqCst);
        format!(".L{}", label)
    }

    pub fn declare_var(&self, var_name: &str) {
        let var_stack_index = self.stack_index.get() - 8;
        self.stack_index.set(var_stack_index);
        let mut set = self.current_scope_var.borrow_mut();
        if let Some(_) = set.get(var_name) {
            panic!("Variable name already exists")
        }
        set.insert(String::from(var_name));
        self.var_map.borrow_mut().insert(String::from(var_name), var_stack_index);
        self.current_scope_stack_size.set(self.current_scope_stack_size.get()+1);
    }

    pub fn declare_var_with_offset(&self, var_name: &str, offset: i32) {
        let mut set = self.current_scope_var.borrow_mut();
        if let Some(_) = set.get(var_name) {
            panic!("Variable name already exists")
        }
        set.insert(String::from(var_name));
        self.var_map.borrow_mut().insert(String::from(var_name), offset);
    }

    pub fn declare_function(&self, name: &str, parameter_len: usize) {
        let mut map = self.fun_map.borrow_mut();
        if let Some(meta) = map.get(name) {
            if meta.param_len != parameter_len {
                panic!("Function name already exists with different parameters")
            }
        }
        self.current_scope_var.borrow_mut().insert(String::from(name));
        map.insert(String::from(name), FunctionMetadata { param_len: parameter_len, defined: false });
    }

    pub fn define_function(&self, name: &str, parameter_len: usize) {
        let mut map = self.fun_map.borrow_mut();
        if let Some(meta) = map.get(name) {
            if meta.defined {
                panic!("Can't redefine function")
            }
            if meta.param_len != parameter_len {
                panic!("Function name already exists with different parameters")
            }
        }
        self.current_scope_var.borrow_mut().insert(String::from(name));
        map.insert(String::from(name), FunctionMetadata { param_len: parameter_len, defined: true });
    }

    pub fn get_var_stack_index(&self, var_name: &str) -> i32 {
        if let Some(i) = self.var_map.borrow().get(var_name) {
            *i
        } else {
            panic!("Variable name not found")
        }
    }

    pub fn get_current_scope_size(&self) -> u32 {
        self.current_scope_stack_size.get()
    }

    pub fn is_function_in_scope(&self, var_name: &str, parameter_len: usize) -> bool {
        if let Some(meta) = self.fun_map.borrow().get(var_name) {
            meta.param_len == parameter_len
        } else {
            false
        }
    }
}
