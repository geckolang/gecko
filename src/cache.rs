use crate::ast;

pub type UniqueId = usize;

pub struct Cache {
  pub struct_impls: std::collections::HashMap<UniqueId, Vec<(UniqueId, String)>>,
  pub new_symbol_table: std::collections::HashMap<UniqueId, ast::Node>,
  unique_id_counter: usize,
}

impl Cache {
  pub fn new() -> Self {
    Self {
      struct_impls: std::collections::HashMap::new(),
      new_symbol_table: std::collections::HashMap::new(),
      unique_id_counter: 0,
    }
  }

  // TODO: Use this function as a guide to ensure that nothing is looked up or inferred before its actually resolved. Within the type-checker.
  pub fn force_get(&self, key: &UniqueId) -> &ast::Node {
    self.new_symbol_table.get(key).unwrap()
  }

  pub fn bind(&mut self, unique_id: UniqueId, node: ast::Node) {
    self.new_symbol_table.insert(unique_id, node);
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
