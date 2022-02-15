use crate::{ast, cache, diagnostic};

#[derive(Hash, PartialEq, Eq, Clone, Debug)]
pub enum SymbolKind {
  Definition,
  // A global type. Can be a struct, or enum.
  Type,
}

pub type Symbol = (String, SymbolKind);

type Scope = std::collections::HashMap<Symbol, cache::UniqueId>;

pub trait Resolve {
  fn declare(&self, _resolver: &mut NameResolver, _cache: &mut cache::Cache) {
    //
  }

  fn resolve(&mut self, _resolver: &mut NameResolver, _cache: &mut cache::Cache) {
    //
  }
}

impl Resolve for ast::Type {
  fn resolve(&mut self, resolver: &mut NameResolver, cache: &mut cache::Cache) {
    match self {
      ast::Type::Stub(stub_type) => stub_type.resolve(resolver, cache),
      ast::Type::Pointer(pointee_type) => pointee_type.resolve(resolver, cache),
      ast::Type::Array(element_type, _) => element_type.resolve(resolver, cache),
      // TODO: Are there any other types that may need to be resolved?
      _ => {}
    };
  }
}

impl Resolve for ast::NodeKind {
  // TODO: This `dispatch` may actually only apply for top-level nodes, so there might be room for simplification.

  fn declare(&self, resolver: &mut NameResolver, cache: &mut cache::Cache) {
    crate::dispatch!(self, Resolve::declare, resolver, cache);
  }

  fn resolve(&mut self, resolver: &mut NameResolver, cache: &mut cache::Cache) {
    crate::dispatch!(self, Resolve::resolve, resolver, cache);
  }
}

impl Resolve for ast::Closure {
  fn declare(&self, resolver: &mut NameResolver, cache: &mut cache::Cache) {
    // TODO: Here, captures should be force-declared.
    // FIXME: The body is resolved within a virtual environment. This means that declarations from here may not be accessible. To solve this, perhaps we may virtualize all but the last scope (this declaration's body's scope).

    self.prototype.declare(resolver, cache);
    self.body.declare(resolver, cache);
  }

  fn resolve(&mut self, resolver: &mut NameResolver, cache: &mut cache::Cache) {
    // FIXME: Continue implementation.

    for (_index, capture) in self.captures.iter_mut().enumerate() {
      let symbol = (capture.0.clone(), SymbolKind::Definition);

      capture.1 = resolver.relative_lookup_or_error(&symbol);

      // FIXME: Anything else needs to be done here?
    }

    // Cache the existing relative scopes, and create a new, empty
    // environment within the resolver, then restore the cached scopes
    // after the body has been resolved. This is done to encapsulate the
    // closure's environment.
    let relative_scopes_cache = resolver.relative_scopes.clone();

    resolver.relative_scopes.clear();
    self.body.resolve(resolver, cache);

    // FIXME: [!] Investigate: Should this closing of relative scopes occur
    // before or after the return type is possibly inferred?
    resolver.relative_scopes = relative_scopes_cache;

    self.prototype.resolve(resolver, cache);
  }
}

impl Resolve for ast::TypeAlias {
  fn resolve(&mut self, resolver: &mut NameResolver, cache: &mut cache::Cache) {
    self.ty.resolve(resolver, cache);
  }
}

// TODO: This might be getting too complicated. Maybe we should keep it simple in this case?
impl Resolve for ast::Pattern {
  fn resolve(&mut self, resolver: &mut NameResolver, _cache: &mut cache::Cache) {
    // TODO: Consider extending this as a function of `Pattern` (via `impl`).
    let symbol = (self.base_name.clone(), self.symbol_kind.clone());

    let lookup_result = match self.symbol_kind {
      SymbolKind::Definition => resolver.relative_lookup(&symbol),
      // SymbolKind::Type => resolver.absolute_lookup(self),
      // TODO: What else? Maybe `unreachable!()`?
      _ => todo!(),
    };

    if let Some(target_key) = lookup_result {
      self.target_key = Some(target_key.clone());
    } else {
      resolver.produce_lookup_error(&symbol.0);
    }
  }
}

impl Resolve for ast::IntrinsicCall {
  //
}

impl Resolve for ast::ExternStatic {
  fn resolve(&mut self, resolver: &mut NameResolver, cache: &mut cache::Cache) {
    self.1.resolve(resolver, cache);
  }
}

impl Resolve for ast::StubType {
  fn resolve(&mut self, resolver: &mut NameResolver, _cache: &mut cache::Cache) {
    // TODO: A bit misleading, since `lookup_or_error` returns `Option<>`.
    self.target_key = resolver.relative_lookup_or_error(&(self.name.clone(), SymbolKind::Type));
  }
}

impl Resolve for ast::StructValue {
  fn resolve(&mut self, resolver: &mut NameResolver, _cache: &mut cache::Cache) {
    // TODO: A bit misleading, since `lookup_or_error` returns `Option<>`.
    self.target_key = resolver.relative_lookup_or_error(&(self.name.clone(), SymbolKind::Type));
  }
}

impl Resolve for ast::Prototype {
  fn declare(&self, resolver: &mut NameResolver, cache: &mut cache::Cache) {
    for parameter in &self.parameters {
      // Create and process an anonymous definition per-parameter.
      let key = cache.create_unique_id();
      ast::Definition {
        symbol: Some((parameter.0.clone(), SymbolKind::Definition)),
        // TODO: Cloning parameter.
        node_ref_cell: cache::create_cached_node(ast::NodeKind::Parameter(parameter.clone())),
        // TODO: Will this `declare` function ever be called more than once? If so, this could be a problem.
        unique_id: key,
      }
      .declare(resolver, cache);
    }
  }

  fn resolve(&mut self, resolver: &mut NameResolver, cache: &mut cache::Cache) {
    for parameter in &mut self.parameters {
      parameter.1.resolve(resolver, cache);
    }

    // NOTE: The prototype is manually resolved after its body is resolved.
  }
}

impl Resolve for ast::StructType {
  fn declare(&self, _resolver: &mut NameResolver, _cache: &mut cache::Cache) {
    // TODO: Implement?
  }

  fn resolve(&mut self, _resolver: &mut NameResolver, _cache: &mut cache::Cache) {
    // TODO: Implement?
  }
}

impl Resolve for ast::UnaryExpr {
  fn resolve(&mut self, resolver: &mut NameResolver, cache: &mut cache::Cache) {
    self.expr.kind.resolve(resolver, cache);
  }
}

impl Resolve for ast::Enum {
  //
}

impl Resolve for ast::AssignStmt {
  fn resolve(&mut self, resolver: &mut NameResolver, cache: &mut cache::Cache) {
    self.assignee_expr.kind.resolve(resolver, cache);
    self.value.kind.resolve(resolver, cache);
  }
}

impl Resolve for ast::ContinueStmt {
  //
}

impl Resolve for ast::ArrayIndexing {
  fn resolve(&mut self, resolver: &mut NameResolver, cache: &mut cache::Cache) {
    self.index_expr.kind.resolve(resolver, cache);

    self.target_key =
      resolver.relative_lookup_or_error(&(self.name.clone(), SymbolKind::Definition));
  }
}

impl Resolve for ast::ArrayValue {
  fn resolve(&mut self, resolver: &mut NameResolver, cache: &mut cache::Cache) {
    for element in &mut self.elements {
      element.kind.resolve(resolver, cache);
    }
  }
}

impl Resolve for ast::UnsafeBlockStmt {
  fn declare(&self, resolver: &mut NameResolver, cache: &mut cache::Cache) {
    self.0.declare(resolver, cache);
  }

  fn resolve(&mut self, resolver: &mut NameResolver, cache: &mut cache::Cache) {
    self.0.resolve(resolver, cache);
  }
}

impl Resolve for ast::Parameter {
  //
}

impl Resolve for ast::Reference {
  fn resolve(&mut self, resolver: &mut NameResolver, cache: &mut cache::Cache) {
    self.0.resolve(resolver, cache);
  }
}

impl Resolve for ast::BreakStmt {
  //
}

impl Resolve for ast::LoopStmt {
  fn declare(&self, resolver: &mut NameResolver, cache: &mut cache::Cache) {
    if let Some(condition) = &self.condition {
      condition.kind.declare(resolver, cache);
    }

    self.body.declare(resolver, cache);
  }

  fn resolve(&mut self, resolver: &mut NameResolver, cache: &mut cache::Cache) {
    if let Some(condition) = &mut self.condition {
      condition.kind.resolve(resolver, cache);
    }

    self.body.resolve(resolver, cache);
  }
}

impl Resolve for ast::IfStmt {
  fn declare(&self, resolver: &mut NameResolver, cache: &mut cache::Cache) {
    self.condition.kind.declare(resolver, cache);
    self.then_block.declare(resolver, cache);

    if let Some(else_block) = &self.else_block {
      else_block.declare(resolver, cache);
    }
  }

  fn resolve(&mut self, resolver: &mut NameResolver, cache: &mut cache::Cache) {
    self.condition.kind.resolve(resolver, cache);
    self.then_block.resolve(resolver, cache);

    if let Some(else_block) = &mut self.else_block {
      else_block.resolve(resolver, cache);
    }
  }
}

impl Resolve for ast::LetStmt {
  fn declare(&self, resolver: &mut NameResolver, cache: &mut cache::Cache) {
    self.value.kind.declare(resolver, cache);
  }

  fn resolve(&mut self, resolver: &mut NameResolver, cache: &mut cache::Cache) {
    self.value.kind.resolve(resolver, cache);

    // If the type was explicitly given, proceed to resolve it.
    if let Some(ty) = &mut self.ty {
      ty.resolve(resolver, cache);
    }
  }
}

impl Resolve for ast::ReturnStmt {
  fn declare(&self, resolver: &mut NameResolver, cache: &mut cache::Cache) {
    if let Some(value) = &self.value {
      value.kind.declare(resolver, cache);
    }
  }

  fn resolve(&mut self, resolver: &mut NameResolver, cache: &mut cache::Cache) {
    if let Some(value) = &mut self.value {
      value.kind.resolve(resolver, cache);
    }
  }
}

impl Resolve for ast::Block {
  fn declare(&self, resolver: &mut NameResolver, cache: &mut cache::Cache) {
    resolver.push_scope();

    for statement in &self.statements {
      statement.kind.declare(resolver, cache);
    }

    resolver.register_scope_tree(self.unique_id);
  }

  fn resolve(&mut self, resolver: &mut NameResolver, cache: &mut cache::Cache) {
    resolver.current_block_unique_id = Some(self.unique_id);

    // TODO:
    for statement in &mut self.statements {
      statement.kind.resolve(resolver, cache);
    }
  }
}

impl Resolve for ast::Literal {
  //
}

impl Resolve for ast::Function {
  fn declare(&self, resolver: &mut NameResolver, cache: &mut cache::Cache) {
    // FIXME: [!] Revise: Ensure the order is correct (test nested parameter, references, etc.).

    // Parameter scope.
    resolver.push_scope();

    // NOTE: The scope tree won't be overwritten by the block's, nor the
    // prototype's scope tree, instead they will be merged, as expected.
    resolver.register_scope_tree(self.body.unique_id);

    self.prototype.declare(resolver, cache);
    self.body.declare(resolver, cache);
  }

  // FIXME: This resolve step may need to be repeated for closure.
  fn resolve(&mut self, resolver: &mut NameResolver, cache: &mut cache::Cache) {
    // FIXME: [!] Investigate: Do we need to resolve the prototype first?
    // If so, doesn't the prototype's inferred type depend on the body being
    // resolved first?

    // TODO: Do we need scope management here, for the prototype's parameters?
    self.prototype.resolve(resolver, cache);

    if let Some(return_type) = &mut self.prototype.return_type {
      return_type.resolve(resolver, cache);
    }

    // Finally, after both the prototype and its return type have been resolved,
    // proceed to resolve the body.
    self.body.resolve(resolver, cache);
  }
}

impl Resolve for ast::ExternFunction {
  fn resolve(&mut self, resolver: &mut NameResolver, cache: &mut cache::Cache) {
    self.prototype.resolve(resolver, cache);
  }
}

impl Resolve for ast::Definition {
  fn declare(&self, resolver: &mut NameResolver, cache: &mut cache::Cache) {
    // TODO: In the future, we'd like to warn against shadowed names. This can be accomplished simply, by using `resolver.contains_relative`.
    if let Some(symbol) = &self.symbol {
      // Check for existing definitions.
      if resolver.contains_current_scope(&symbol) {
        resolver
          .diagnostic_builder
          .error(format!("re-definition of `{}`", symbol.0));

        // TODO: What about calling the child's declare function?
        return;
      }

      // Bind the symbol to the current scope for name resolution lookup.
      resolver.bind(symbol.clone(), self.unique_id);
    }

    // Register the node on the cache for lowering lookup.
    cache.bind(self.unique_id, std::rc::Rc::clone(&self.node_ref_cell));

    self.node_ref_cell.borrow_mut().declare(resolver, cache);
  }

  fn resolve(&mut self, resolver: &mut NameResolver, cache: &mut cache::Cache) {
    self.node_ref_cell.borrow_mut().resolve(resolver, cache);
  }
}

impl Resolve for ast::CallExpr {
  fn declare(&self, resolver: &mut NameResolver, cache: &mut cache::Cache) {
    // Declare any possible `Definition` nodes in the arguments
    // (such as inline closures, etc.).
    for argument in &self.arguments {
      argument.kind.declare(resolver, cache);
    }
  }

  fn resolve(&mut self, resolver: &mut NameResolver, cache: &mut cache::Cache) {
    self.callee_expr.kind.resolve(resolver, cache);

    for argument in &mut self.arguments {
      argument.kind.resolve(resolver, cache);
    }
  }
}

impl Resolve for ast::InlineExprStmt {
  fn declare(&self, resolver: &mut NameResolver, cache: &mut cache::Cache) {
    self.expr.kind.declare(resolver, cache);
  }

  fn resolve(&mut self, resolver: &mut NameResolver, cache: &mut cache::Cache) {
    self.expr.kind.resolve(resolver, cache);
  }
}

impl Resolve for ast::BinaryExpr {
  fn declare(&self, resolver: &mut NameResolver, cache: &mut cache::Cache) {
    self.left.kind.declare(resolver, cache);
    self.right.kind.declare(resolver, cache);
  }

  fn resolve(&mut self, resolver: &mut NameResolver, cache: &mut cache::Cache) {
    self.left.kind.resolve(resolver, cache);
    self.right.kind.resolve(resolver, cache);
  }
}

pub struct NameResolver {
  pub diagnostic_builder: diagnostic::DiagnosticBuilder,
  current_module_name: Option<String>,
  /// Contains the modules with their respective top-level definitions.
  global_scopes: std::collections::HashMap<String, Scope>,
  /// Contains volatile, relative scopes. Only used during the declare step.
  /// This is reset when the module changes, although by that time all the
  /// relative scopes should have been popped automatically.
  relative_scopes: Vec<Scope>,
  /// A mapping of a block's unique key to its scope, and all visible parent
  /// relative scopes, excluding the global scope.
  scope_map: std::collections::HashMap<cache::UniqueId, Vec<Scope>>,
  /// The unique id of the current block. Used for the resolve step.
  current_block_unique_id: Option<cache::UniqueId>,
}

impl NameResolver {
  pub fn new() -> Self {
    Self {
      diagnostic_builder: diagnostic::DiagnosticBuilder::new(),
      current_module_name: None,
      global_scopes: std::collections::HashMap::new(),
      relative_scopes: Vec::new(),
      scope_map: std::collections::HashMap::new(),
      current_block_unique_id: None,
    }
  }

  /// Set per-file. A new global scope is created per-module.
  pub fn create_module(&mut self, name: String) {
    // TODO: Can the module name possibly collide with an existing one?

    self.current_module_name = Some(name.clone());

    self
      .global_scopes
      .insert(name, std::collections::HashMap::new());

    self.relative_scopes.clear();
  }

  pub fn set_active_module(&mut self, name: String) -> bool {
    // TODO: Implement checks (that module exists, etc.).
    // TODO: Shouldn't we reset buffers here? This might prevent the re-definition bug.
    self.current_module_name = Some(name.clone());

    true
  }

  /// Retrieve the last pushed relative scope, or if there are none,
  /// the global scope of the current module.
  fn get_current_scope(&mut self) -> &mut Scope {
    if self.relative_scopes.is_empty() {
      self
        .global_scopes
        .get_mut(self.current_module_name.as_ref().unwrap())
        .unwrap()
    } else {
      self.relative_scopes.last_mut().unwrap()
    }
  }

  // TODO: Consider returning the pushed scope? Unless it's not actually used.
  fn push_scope(&mut self) {
    self.relative_scopes.push(std::collections::HashMap::new());
  }

  /// Pop the last scope off the relatives scopes stack, and return it.
  ///
  /// Will panic if there are no relative scopes.
  fn force_pop_scope(&mut self) -> Scope {
    self.relative_scopes.pop().unwrap()
  }

  /// Force-pop the last scope off the relatives scopes stack, and create
  /// a scope tree. This tree will then be inserted into the scope map. If
  /// an entry with the same block id already exists, the scope tree will
  /// be appended onto the existing definition.
  fn register_scope_tree(&mut self, block_id: cache::UniqueId) {
    let mut scope_tree = vec![self.force_pop_scope()];

    // Clone the relative scope tree.
    scope_tree.extend(self.relative_scopes.iter().rev().cloned());

    let mut final_value_buffer = scope_tree;

    // Append to the existing definition, if applicable.
    if self.scope_map.contains_key(&block_id) {
      final_value_buffer.extend(self.scope_map.remove(&block_id).unwrap());
    }

    self.scope_map.insert(block_id, final_value_buffer);
  }

  /// Register a name on the last scope for name resolution lookups.
  ///
  /// If there are no relative scopes, the symbol is registered in the global scope.
  fn bind(&mut self, symbol: Symbol, definition_key: cache::UniqueId) {
    self.get_current_scope().insert(symbol, definition_key);
  }

  fn produce_lookup_error(&mut self, name: &String) {
    self
      .diagnostic_builder
      .error(format!("undefined reference to `{}`", name));
  }

  /// Lookup a symbol starting from the nearest scope, all the way to the global scope
  /// of the current module.
  fn relative_lookup(&mut self, symbol: &Symbol) -> Option<&cache::UniqueId> {
    let scope_tree = self
      .scope_map
      .get(&self.current_block_unique_id.unwrap())
      .unwrap();

    // First attempt to find the symbol in the relative scopes.
    for scope in scope_tree {
      if let Some(definition_key) = scope.get(&symbol) {
        return Some(definition_key);
      }
    }

    // Otherwise, attempt to find the symbol in the current module's global scope.
    let global_scope = self
      .global_scopes
      .get(self.current_module_name.as_ref().unwrap())
      .unwrap();

    if let Some(definition_key) = global_scope.get(&symbol) {
      return Some(definition_key);
    }

    None
  }

  fn relative_lookup_or_error(&mut self, symbol: &Symbol) -> Option<cache::UniqueId> {
    if let Some(definition_key) = self.relative_lookup(symbol) {
      return Some(definition_key.clone());
    }

    self.produce_lookup_error(&symbol.0);

    None
  }

  fn contains_current_scope(&mut self, key: &Symbol) -> bool {
    self.get_current_scope().contains_key(key)
  }
}

// TODO: Add essential tests.
#[cfg(test)]
mod tests {
  use super::*;

  #[test]
  fn proper_initial_values() {
    let name_resolver = NameResolver::new();

    assert!(name_resolver.current_module_name.is_none());
    assert!(name_resolver.relative_scopes.is_empty());
    assert!(name_resolver.global_scopes.is_empty());
  }

  #[test]
  fn push_pop_scope() {
    let mut name_resolver = NameResolver::new();

    assert!(name_resolver.relative_scopes.is_empty());
    name_resolver.push_scope();
    assert_eq!(1, name_resolver.relative_scopes.len());
    name_resolver.force_pop_scope();
    assert!(name_resolver.relative_scopes.is_empty());
  }
}
