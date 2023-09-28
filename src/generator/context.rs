use std::{cell::{Cell, RefCell}, collections::{HashMap, HashSet}, sync::atomic::{AtomicU32, Ordering}};

// TODO currently not thread-safe
pub struct Context {
    var_map: RefCell<HashMap<String, i32>>,
    stack_index: Cell<i32>,
    current_scope_var: RefCell<HashSet<String>>,
    pub current_break: Option<String>,
    pub current_continue: Option<String>,
}

static LABEL_COUNTER: AtomicU32 = AtomicU32::new(0);

impl Context {
    pub fn new(parent: Option<&Context>) -> Context {
        if let Some(parent) = parent {
            Context { 
                var_map: RefCell::new(parent.var_map.borrow().clone()),
                stack_index: Cell::new(parent.stack_index.get().clone()),
                current_scope_var: RefCell::new(HashSet::new()),
                current_break: parent.current_break.clone(),
                current_continue: parent.current_continue.clone(),
            }
        } else {
            Context { 
                var_map: RefCell::new(HashMap::new()),
                stack_index: Cell::new(0),
                current_scope_var: RefCell::new(HashSet::new()),
                current_break: None,
                current_continue: None,
            }
        }
    }

    pub fn new_loop(parent: &Context, current_break: Option<String>, current_continue: Option<String>) -> Context {
        Context { 
            var_map: RefCell::new(parent.var_map.borrow().clone()),
            stack_index: Cell::new(parent.stack_index.get().clone()),
            current_scope_var: RefCell::new(HashSet::new()),
            current_break: current_break,
            current_continue: current_continue,
        }
    }

    pub fn get_and_increase_label(&self) -> String {
        let label = LABEL_COUNTER.fetch_add(1, Ordering::SeqCst);
        format!("_{}", label)
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
    }

    pub fn get_var_stack_index(&self, var_name: &str) -> i32 {
        if let Some(i) = self.var_map.borrow().get(var_name) {
            *i
        } else {
            panic!("Variable name not found")
        }
    }

    pub fn get_current_scope_size(&self) -> usize {
        self.current_scope_var.borrow().len()
    }
}
