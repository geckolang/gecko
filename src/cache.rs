use crate::ast;

pub type UniqueId = usize;

pub struct Cache {
  pub struct_impls: std::collections::HashMap<UniqueId, Vec<(UniqueId, String)>>,
  /// A map of unique ids to their corresponding `NodeKind` construct.
  ///
  /// This map contains only nodes that may be referenced at some point (such as functions,
  /// variables, and other bindings). This serves as a snapshot of the AST, created during
  /// the `declare` name resolution step, which has also been resolved.
  pub symbols: std::collections::HashMap<UniqueId, ast::NodeKind>,
  // REVIEW: Currently, a type-cache is favored under the semantic check context.
  // ... This is because it's currently only used there. But in the future, for other
  // ... phases, we might need to store them on a generalized container (this cache).
  /// A map of unique ids to their corresponding `Type` construct.
  ///
  /// This serves as a type cache, to avoid re-inferring the type of a node
  /// multiple times.
  pub types: std::collections::HashMap<UniqueId, ast::Type>,
  unique_id_counter: usize,
}

impl Cache {
  pub fn new() -> Self {
    Self {
      struct_impls: std::collections::HashMap::new(),
      symbols: std::collections::HashMap::new(),
      types: std::collections::HashMap::new(),
      unique_id_counter: 0,
    }
  }

  // TODO: Use this function as a guide to ensure that nothing is looked up
  // ... or inferred before its actually resolved. Within the type-checker.
  /// Forcefully retrieve a node from the cache.
  ///
  /// This function will panic if the given key does not exist.
  pub fn unsafe_get(&self, key: &UniqueId) -> &ast::NodeKind {
    self.symbols.get(key).unwrap()
  }

  pub fn create_unique_id(&mut self) -> UniqueId {
    let unique_id = self.unique_id_counter;

    self.unique_id_counter += 1;

    unique_id
  }

  pub fn add_struct_impl(&mut self, struct_unique_id: UniqueId, methods: Vec<(UniqueId, String)>) {
    if let Some(existing_impls) = self.struct_impls.get_mut(&struct_unique_id) {
      existing_impls.extend(methods);

      return;
    }

    self.struct_impls.insert(struct_unique_id, methods);
  }
}
