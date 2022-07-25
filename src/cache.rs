use crate::ast;

pub type Id = usize;

pub struct Cache {
  pub struct_impls: std::collections::HashMap<Id, Vec<(Id, String)>>,
  // TODO: Update description with the generalization to allow for closure retrieval.
  /// A map of unique ids to their corresponding `NodeKind` construct.
  ///
  /// This map contains only nodes that may be referenced at some point (such as functions,
  /// variables, and other bindings). This serves as a snapshot of the AST, created during
  /// the `declare` name resolution step, which has also been resolved.
  pub symbols: std::collections::HashMap<Id, ast::NodeKind>,
  pub links: std::collections::HashMap<Id, Id>,
  // FIXME: Instead of caching, nodes, use a mapping from their id to their type.
  // ... This is because all retrievals of cached nodes are simply to determine or
  // ... retrieve their type. So, we can avoid the headaches of caching the nodes.
  pub cached_nodes: std::collections::HashMap<Id, ast::NodeKind>,
  // REVIEW: Should this be here?
  pub main_function_id: Option<Id>,
  id_counter: usize,
}

impl Cache {
  pub fn new() -> Self {
    Self {
      links: std::collections::HashMap::new(),
      struct_impls: std::collections::HashMap::new(),
      symbols: std::collections::HashMap::new(),
      main_function_id: None,
      id_counter: 0,
      cached_nodes: std::collections::HashMap::new(),
    }
  }

  /// Forcefully retrieve a node from the cache.
  ///
  /// This function will panic if the given key does not exist.
  pub fn force_get(&self, key: &Id) -> &ast::NodeKind {
    self.symbols.get(key).unwrap()
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
}
