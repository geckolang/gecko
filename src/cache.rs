use crate::ast;

pub type DefinitionKey = usize;

pub struct Cache {
  key_counter: usize,
  pub declarations:
    std::collections::HashMap<DefinitionKey, std::rc::Rc<std::cell::RefCell<ast::Node>>>,
}

impl Cache {
  pub fn new() -> Self {
    Cache {
      key_counter: 0,
      declarations: std::collections::HashMap::new(),
    }
  }

  pub fn create_definition_key(&mut self) -> DefinitionKey {
    let key = self.key_counter;

    self.key_counter += 1;

    key
  }
}