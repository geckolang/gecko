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
  // REVIEW: Should this be here?
  pub main_function_id: Option<Id>,
  id_counter: usize,
  _types: std::collections::HashMap<Id, ast::Type>,
}

impl Cache {
  pub fn new() -> Self {
    Self {
      struct_impls: std::collections::HashMap::new(),
      symbols: std::collections::HashMap::new(),
      main_function_id: None,
      id_counter: 0,
      _types: std::collections::HashMap::new(),
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

  pub fn add_struct_impl(&mut self, struct_cache_id: Id, methods: Vec<(Id, String)>) {
    if let Some(existing_impls) = self.struct_impls.get_mut(&struct_cache_id) {
      existing_impls.extend(methods);

      return;
    }

    self.struct_impls.insert(struct_cache_id, methods);
  }
}
