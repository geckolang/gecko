use crate::ast;
use std::cell::Ref;

pub type UniqueId = usize;

// TODO: For the `Cache` struct, we might not need a `RefCell<>`, since there are no mutable borrows.
pub type CachedNode = std::rc::Rc<std::cell::RefCell<ast::NodeKind>>;

pub struct Cache {
  key_counter: usize,
  pub declarations: std::collections::HashMap<UniqueId, CachedNode>,
}

impl Cache {
  pub fn new() -> Self {
    Self {
      key_counter: 0,
      declarations: std::collections::HashMap::new(),
    }
  }

  // TODO: Use this function as a guide to ensure that nothing is looked up or inferred before its actually resolved. Within the type-checker.
  pub fn get(&self, key: &UniqueId) -> Ref<'_, ast::NodeKind> {
    self.declarations.get(key).unwrap().as_ref().borrow()
  }

  pub fn bind(&mut self, key: UniqueId, node: CachedNode) {
    self.declarations.insert(key, node);
  }

  pub fn create_unique_id(&mut self) -> UniqueId {
    let key = self.key_counter;

    self.key_counter += 1;

    key
  }
}

pub fn create_cached_node(node: ast::NodeKind) -> CachedNode {
  std::rc::Rc::new(std::cell::RefCell::new(node))
}
