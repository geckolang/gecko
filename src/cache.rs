use crate::ast;
use std::cell::Ref;

pub type DefinitionKey = usize;

pub type CachedNode = std::rc::Rc<std::cell::RefCell<ast::Node>>;

pub struct Cache {
  key_counter: usize,
  pub declarations: std::collections::HashMap<DefinitionKey, CachedNode>,
}

impl Cache {
  pub fn new() -> Self {
    Self {
      key_counter: 0,
      declarations: std::collections::HashMap::new(),
    }
  }

  pub fn get(&self, key: &DefinitionKey) -> Ref<'_, ast::Node> {
    self.declarations.get(key).unwrap().as_ref().borrow()
  }

  pub fn bind(&mut self, key: DefinitionKey, node: CachedNode) {
    self.declarations.insert(key, node);
  }

  pub fn create_definition_key(&mut self) -> DefinitionKey {
    let key = self.key_counter;

    self.key_counter += 1;

    key
  }
}

pub fn create_cached_node(node: ast::Node) -> CachedNode {
  std::rc::Rc::new(std::cell::RefCell::new(node))
}
