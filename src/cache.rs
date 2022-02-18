use crate::ast;
use std::cell::Ref;

pub type UniqueId = usize;

// TODO: For the `Cache` struct, we might not need a `RefCell<>`, since there are no mutable borrows.
pub type CachedNode = std::rc::Rc<std::cell::RefCell<ast::Node>>;

pub struct Cache {
  unique_id_counter: usize,
  pub declarations: std::collections::HashMap<UniqueId, CachedNode>,
}

impl Cache {
  pub fn new() -> Self {
    Self {
      unique_id_counter: 0,
      declarations: std::collections::HashMap::new(),
    }
  }

  // TODO: Use this function as a guide to ensure that nothing is looked up or inferred before its actually resolved. Within the type-checker.
  pub fn force_get(&self, key: &UniqueId) -> Ref<'_, ast::Node> {
    self.declarations.get(key).unwrap().as_ref().borrow()
  }

  pub fn bind(&mut self, unique_id: UniqueId, node: CachedNode) {
    self.declarations.insert(unique_id, node);
  }

  pub fn create_unique_id(&mut self) -> UniqueId {
    let unique_id = self.unique_id_counter;

    self.unique_id_counter += 1;

    unique_id
  }
}

pub fn create_cached_node(node: ast::Node) -> CachedNode {
  std::rc::Rc::new(std::cell::RefCell::new(node))
}
