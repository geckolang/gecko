use crate::ast;

pub type BindingId = usize;

pub struct Cache {
  pub struct_impls: std::collections::HashMap<BindingId, Vec<(BindingId, String)>>,
  /// A map of unique ids to their corresponding `NodeKind` construct.
  ///
  /// This map contains only nodes that may be referenced at some point (such as functions,
  /// variables, and other bindings). This serves as a snapshot of the AST, created during
  /// the `declare` name resolution step, which has also been resolved.
  pub symbols: std::collections::HashMap<BindingId, ast::NodeKind>,
  pub main_function_id: Option<BindingId>,
  binding_id_counter: usize,
  _types: std::collections::HashMap<BindingId, ast::Type>,
}

impl Cache {
  pub fn new() -> Self {
    Self {
      struct_impls: std::collections::HashMap::new(),
      symbols: std::collections::HashMap::new(),
      main_function_id: None,
      binding_id_counter: 0,
      _types: std::collections::HashMap::new(),
    }
  }

  /// Forcefully retrieve a node from the cache.
  ///
  /// This function will panic if the given key does not exist.
  pub fn force_get(&self, key: &BindingId) -> &ast::NodeKind {
    self.symbols.get(key).unwrap()
  }

  pub fn create_binding_id(&mut self) -> BindingId {
    let binding_id = self.binding_id_counter;

    self.binding_id_counter += 1;

    binding_id
  }

  pub fn add_struct_impl(
    &mut self,
    struct_binding_id: BindingId,
    methods: Vec<(BindingId, String)>,
  ) {
    if let Some(existing_impls) = self.struct_impls.get_mut(&struct_binding_id) {
      existing_impls.extend(methods);

      return;
    }

    self.struct_impls.insert(struct_binding_id, methods);
  }
}
