use crate::ast;

pub type Id = usize;

pub struct Cache {
  pub struct_impls: std::collections::HashMap<Id, Vec<(Id, String)>>,
  // REVIEW: Is this necessary, or can we just have a direct mapping?
  pub links: std::collections::HashMap<Id, Id>,
  // FIXME: Instead of caching, nodes, use a mapping from their id to their type.
  // ... This is because all retrievals of cached nodes are simply to determine or
  // ... retrieve their type. So, we can avoid the headaches of caching the nodes.
  pub declarations: std::collections::HashMap<Id, ast::NodeKind>,
  // REVIEW: Should this be here?
  pub main_function_id: Option<Id>,
  id_counter: usize,
}

impl Cache {
  pub fn new() -> Self {
    Self {
      links: std::collections::HashMap::new(),
      struct_impls: std::collections::HashMap::new(),
      main_function_id: None,
      id_counter: 0,
      declarations: std::collections::HashMap::new(),
    }
  }

  pub fn create_id(&mut self) -> Id {
    let id = self.id_counter;

    self.id_counter += 1;

    id
  }

  pub fn add_struct_impl(&mut self, struct_id: Id, methods: Vec<(Id, String)>) {
    if let Some(existing_impls) = self.struct_impls.get_mut(&struct_id) {
      existing_impls.extend(methods);

      return;
    }

    self.struct_impls.insert(struct_id, methods);
  }

  // REVIEW: Will there ever be a need to follow multiple links?
  pub fn find_decl_via_link(&self, id: &Id) -> Option<&ast::NodeKind> {
    if let Some(target) = self.links.get(id) {
      return self.declarations.get(target);
    }

    None
  }
}
