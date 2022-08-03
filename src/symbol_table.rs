use crate::ast;

pub type NodeId = usize;
pub type LinkId = usize;

pub struct SymbolTable {
  pub struct_impls: std::collections::HashMap<NodeId, Vec<(NodeId, String)>>,
  /// A mapping from a referential id to the id of its target node.
  ///
  /// The reason why this is necessary is, and there is no direct mapping
  /// from id to the target node is because then the target node would need to
  /// be repeated per referential id.
  pub links: std::collections::HashMap<LinkId, NodeId>,
  // FIXME: Instead of caching, nodes, use a mapping from their id to their type.
  // ... This is because all retrievals of cached nodes are simply to determine or
  // ... retrieve their type. So, we can avoid the headaches of caching the nodes.
  pub declarations: std::collections::HashMap<NodeId, ast::NodeKind>,
  // REVIEW: Should this be here?
  pub main_function_id: Option<NodeId>,
  id_counter: usize,
}

impl SymbolTable {
  pub fn new() -> Self {
    Self {
      links: std::collections::HashMap::new(),
      struct_impls: std::collections::HashMap::new(),
      main_function_id: None,
      id_counter: 0,
      declarations: std::collections::HashMap::new(),
    }
  }

  pub fn next_id(&mut self) -> NodeId {
    let id = self.id_counter;

    self.id_counter += 1;

    id
  }

  pub fn add_struct_impl(&mut self, struct_id: NodeId, methods: Vec<(NodeId, String)>) {
    if let Some(existing_impls) = self.struct_impls.get_mut(&struct_id) {
      existing_impls.extend(methods);

      return;
    }

    self.struct_impls.insert(struct_id, methods);
  }

  // REVIEW: Will there ever be a need to follow multiple links?
  pub fn find_decl_via_link(&self, id: &NodeId) -> Option<&ast::NodeKind> {
    if let Some(target) = self.links.get(id) {
      return self.declarations.get(target);
    }

    None
  }
}

// TODO: Add tests.
