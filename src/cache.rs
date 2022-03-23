use crate::ast;

pub type UniqueId = usize;

pub struct Cache {
  pub struct_impls: std::collections::HashMap<UniqueId, Vec<(UniqueId, String)>>,
  /// A map of unique ids to their corresponding `NodeKind` construct.
  ///
  /// This serves as a snapshot of the AST, created during the `declare` name
  /// resolution step, which means that the cached AST has NOT been resolved.
  /// Any calls to infer the type of a node within this AST snapshot, or any
  /// other that depends on a node being resolved, will fail.
  pub symbols: std::collections::HashMap<UniqueId, ast::NodeKind>,
  unique_id_counter: usize,
}

impl Cache {
  pub fn new() -> Self {
    Self {
      struct_impls: std::collections::HashMap::new(),
      symbols: std::collections::HashMap::new(),
      unique_id_counter: 0,
    }
  }

  // TODO: Use this function as a guide to ensure that nothing is looked up or inferred before its actually resolved. Within the type-checker.
  /// Forcefully retrieve an unresolved node from the cache.
  ///
  /// This function will panic if the given key does not exist.
  pub fn unsafe_get(&self, key: &UniqueId) -> &ast::NodeKind {
    self.symbols.get(key).unwrap()
  }

  pub fn bind(&mut self, unique_id: UniqueId, node: ast::NodeKind) {
    self.symbols.insert(unique_id, node);
  }

  pub fn create_unique_id(&mut self) -> UniqueId {
    let unique_id = self.unique_id_counter;

    self.unique_id_counter += 1;

    unique_id
  }

  /// Retrieve any existing implementations for a given struct type.
  ///
  /// If there are none, an empty vector will be returned instead.
  pub fn get_struct_impls(&self, struct_unique_id: &UniqueId) -> Option<&Vec<(UniqueId, String)>> {
    self.struct_impls.get(struct_unique_id)
  }

  pub fn add_struct_impl(&mut self, struct_unique_id: UniqueId, methods: Vec<(UniqueId, String)>) {
    if let Some(existing_impls) = self.struct_impls.get_mut(&struct_unique_id) {
      existing_impls.extend(methods);

      return;
    }

    self.struct_impls.insert(struct_unique_id, methods);
  }
}
