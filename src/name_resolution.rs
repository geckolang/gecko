use crate::{ast, cache, diagnostic};

#[derive(Hash, PartialEq, Eq, Clone, Debug)]
pub enum SymbolKind {
  Definition,
  // TODO: Can be more than that. Do not lie.
  /// A global type. Can be a struct, or enum.
  Type,
}

pub type Symbol = (String, SymbolKind);

type Scope = std::collections::HashMap<Symbol, cache::BindingId>;

pub trait Resolve {
  fn declare(&self, _resolver: &mut NameResolver) {
    //
  }

  fn resolve(&mut self, _resolver: &mut NameResolver) {
    //
  }
}

impl Resolve for ast::Type {
  fn resolve(&mut self, resolver: &mut NameResolver) {
    match self {
      ast::Type::Stub(stub_type) => stub_type.resolve(resolver),
      ast::Type::Pointer(pointee_type) => pointee_type.resolve(resolver),
      ast::Type::Array(element_type, _) => element_type.resolve(resolver),
      ast::Type::This(this_type) => this_type.resolve(resolver),
      // TODO: Are there any other types that may need to be resolved?
      _ => {}
    };
  }
}

// TODO: Get rid-of. `NodeKind` is preferred. This is redundant.
impl Resolve for ast::Node {
  // REVIEW: This `dispatch` may actually only apply for top-level nodes, so there might be room for simplification.

  fn declare(&self, resolver: &mut NameResolver) {
    crate::dispatch!(&self.kind, Resolve::declare, resolver);
  }

  fn resolve(&mut self, resolver: &mut NameResolver) {
    crate::dispatch!(&mut self.kind, Resolve::resolve, resolver);
  }
}

impl Resolve for ast::NodeKind {
  // REVIEW: This `dispatch` may actually only apply for top-level nodes, so there might be room for simplification.

  fn declare(&self, resolver: &mut NameResolver) {
    crate::dispatch!(self, Resolve::declare, resolver);
  }

  fn resolve(&mut self, resolver: &mut NameResolver) {
    crate::dispatch!(self, Resolve::resolve, resolver);
  }
}

impl Resolve for ast::Trait {
  fn declare(&self, _resolver: &mut NameResolver) {
    // REVIEW: Is there a need to declare symbol here?
    // resolver.declare_symbol((self.name.clone(), SymbolKind::Type), self.binding_id);
  }
}

impl Resolve for ast::ThisType {
  fn resolve(&mut self, resolver: &mut NameResolver) {
    if let Some(this_type_id) = resolver.current_struct_type_id {
      self.target_id = Some(this_type_id);
    } else {
      resolver
        .diagnostic_builder
        .error("type `This` cannot be used outside of a struct implementation".to_string());
    }
  }
}

impl Resolve for ast::StructImpl {
  fn declare(&self, resolver: &mut NameResolver) {
    // REVIEW: Is there a need to bind this on the cache?

    resolver.push_scope();

    for method in &self.methods {
      method.declare(resolver);
    }

    resolver.force_pop_scope();
  }

  fn resolve(&mut self, resolver: &mut NameResolver) {
    self.target_struct_pattern.resolve(resolver);

    if let Some(trait_pattern) = &mut self.trait_pattern {
      trait_pattern.resolve(resolver);
    }

    // REVIEW: We can't unwrap here because the lookup might have failed.
    // ... Is this done in other parts? Certain resolve methods depend on
    // ... other things being resolved already, this could be dangerous.
    let struct_type_id_result = self.target_struct_pattern.target_id;

    if let Some(struct_type_id) = struct_type_id_result {
      resolver.current_struct_type_id = Some(struct_type_id);

      for method in &mut self.methods {
        method.resolve(resolver);
      }

      resolver.current_struct_type_id = None;
    }
  }
}

impl Resolve for ast::MemberAccess {
  fn resolve(&mut self, resolver: &mut NameResolver) {
    self.base_expr.resolve(resolver);
  }
}

impl Resolve for ast::Closure {
  fn declare(&self, resolver: &mut NameResolver) {
    // TODO: Here, captures should be force-declared.
    // FIXME: The body is resolved within a virtual environment. This means that declarations from here may not be accessible. To solve this, perhaps we may virtualize all but the last scope (this declaration's body's scope).

    self.prototype.declare(resolver);
    self.body.declare(resolver);
  }

  fn resolve(&mut self, resolver: &mut NameResolver) {
    // FIXME: Continue implementation.

    for (_index, capture) in self.captures.iter_mut().enumerate() {
      let symbol = (capture.0.clone(), SymbolKind::Definition);

      capture.1 = resolver.lookup_or_error(&symbol);

      // FIXME: Anything else needs to be done here?
    }

    // Cache the existing relative scopes, and create a new, empty
    // environment within the resolver, then restore the cached scopes
    // after the body has been resolved. This is done to encapsulate the
    // closure's environment.
    let relative_scopes_buffer = resolver.relative_scopes.clone();

    resolver.relative_scopes.clear();
    self.body.resolve(resolver);

    // REVIEW: Should this closing of relative scopes occur
    // ... before or after the return type is possibly inferred?
    resolver.relative_scopes = relative_scopes_buffer;

    self.prototype.resolve(resolver);
  }
}

impl Resolve for ast::TypeAlias {
  fn declare(&self, resolver: &mut NameResolver) {
    resolver.declare_symbol((self.name.clone(), SymbolKind::Type), self.binding_id);
  }

  fn resolve(&mut self, resolver: &mut NameResolver) {
    self.ty.resolve(resolver);
  }
}

// REVIEW: This might be getting too complicated. Maybe we should keep it simple in this case?
impl Resolve for ast::Pattern {
  fn resolve(&mut self, resolver: &mut NameResolver) {
    // REVISE: A bit misleading, since `lookup_or_error` returns `Option<>`.
    self.target_id = resolver.lookup_or_error(&(self.base_name.clone(), self.symbol_kind.clone()));
  }
}

impl Resolve for ast::IntrinsicCall {
  //
}

impl Resolve for ast::ExternStatic {
  fn declare(&self, resolver: &mut NameResolver) {
    resolver.declare_symbol((self.name.clone(), SymbolKind::Definition), self.binding_id);
  }

  fn resolve(&mut self, resolver: &mut NameResolver) {
    self.ty.resolve(resolver);
  }
}

impl Resolve for ast::StubType {
  fn resolve(&mut self, resolver: &mut NameResolver) {
    // REVISE: A bit misleading, since `lookup_or_error` returns `Option<>`.
    self.target_id = resolver.lookup_or_error(&(self.name.clone(), SymbolKind::Type));
  }
}

impl Resolve for ast::StructValue {
  fn resolve(&mut self, resolver: &mut NameResolver) {
    // REVISE: A bit misleading, since `lookup_or_error` returns `Option<>`.
    self.target_id = resolver.lookup_or_error(&(self.struct_name.clone(), SymbolKind::Type));

    if let Some(target_id) = self.target_id {
      // So, the new system will look like this:
      //  - The type is attached to the node that needs it.
      //  - We still have the cache, but only to lookup & lower nodes.
      //    - [?] When lowering nodes, we won't have any problems with cached nodes? Those nodes
      //      ... are unresolved, so they might not lower!
      //  - The cache will not be used for anything else.
      //  - Special cases such as StructValue's needing of StructType's fields will be solved,
      //  - ... because its attached type already contains such information.
      //  - There will no longer be a need for another cloned AST, nor the double visitation, instead
      //  - ... what will be cloned is certain Types, to be stored as filler for certain nodes.
      //  - The infer type's repetitive nature will be addressed. No need for a specialized container
      //  - ... to hold cached types, that only adds complexity (and unsafe retrieval). Instead, just
      //  - ... directly attach the Types to their respective nodes.
      //  - This system implies that the type-inference and determination will be done here, during the
      //  - ... resolve step.
      // TODO: Create a struct type based off the target Struct node.
      // self.ty = cache::get_node(target_id);
      ///////////////////////////////////////////////////////////////
    }

    for field in &mut self.fields {
      field.resolve(resolver);
    }
  }
}

impl Resolve for ast::Prototype {
  fn declare(&self, resolver: &mut NameResolver) {
    if self.accepts_instance {
      let _this_parameter = self.this_parameter.as_ref().unwrap();

      // TODO: Re-implement.
      // ast::Definition {
      //   symbol: Some((this_parameter.0.clone(), SymbolKind::Definition)),
      //   node_ref_cell: cache::create_cached_node(ast::Node {
      //     // TODO: Cloning parameter.
      //     kind: ast::NodeKind::Parameter(this_parameter.clone()),
      //     // TODO: Span.
      //     span: 0..0,
      //     binding_id: cache.create_binding_id(),
      //   }),
      //   // TODO: Will this `declare` function ever be called more than once? If so, this could be a problem.
      //   binding_id: cache.create_binding_id(),
      // }
      // .declare(resolver);
    }

    for parameter in &self.parameters {
      parameter.declare(resolver);
    }
  }

  fn resolve(&mut self, resolver: &mut NameResolver) {
    for parameter in &mut self.parameters {
      parameter.resolve(resolver);
    }

    if self.accepts_instance {
      self.this_parameter.as_mut().unwrap().resolve(resolver);
    }

    self.return_type.resolve(resolver);
  }
}

impl Resolve for ast::StructType {
  fn declare(&self, resolver: &mut NameResolver) {
    resolver.declare_symbol((self.name.clone(), SymbolKind::Type), self.binding_id);
  }

  fn resolve(&mut self, resolver: &mut NameResolver) {
    for field in &mut self.fields {
      field.1.resolve(resolver);
    }
  }
}

impl Resolve for ast::UnaryExpr {
  fn resolve(&mut self, resolver: &mut NameResolver) {
    self.expr.resolve(resolver);
  }
}

impl Resolve for ast::Enum {
  fn declare(&self, resolver: &mut NameResolver) {
    resolver.declare_symbol((self.name.clone(), SymbolKind::Type), self.binding_id);
  }
}

impl Resolve for ast::AssignStmt {
  fn resolve(&mut self, resolver: &mut NameResolver) {
    self.assignee_expr.resolve(resolver);
    self.value.resolve(resolver);
  }
}

impl Resolve for ast::ContinueStmt {
  //
}

impl Resolve for ast::ArrayIndexing {
  fn declare(&self, resolver: &mut NameResolver) {
    self.index_expr.declare(resolver);
  }

  fn resolve(&mut self, resolver: &mut NameResolver) {
    self.index_expr.resolve(resolver);
    self.target_id = resolver.lookup_or_error(&(self.name.clone(), SymbolKind::Definition));
  }
}

impl Resolve for ast::ArrayValue {
  // TODO: Do we need to declare the struct value's expressions?

  fn resolve(&mut self, resolver: &mut NameResolver) {
    for element in &mut self.elements {
      element.resolve(resolver);
    }
  }
}

impl Resolve for ast::UnsafeBlockStmt {
  fn declare(&self, resolver: &mut NameResolver) {
    self.0.declare(resolver);
  }

  fn resolve(&mut self, resolver: &mut NameResolver) {
    self.0.resolve(resolver);
  }
}

impl Resolve for ast::Parameter {
  fn declare(&self, resolver: &mut NameResolver) {
    resolver.declare_symbol((self.name.clone(), SymbolKind::Definition), self.binding_id);
  }

  fn resolve(&mut self, resolver: &mut NameResolver) {
    self.ty.resolve(resolver);
  }
}

impl Resolve for ast::Reference {
  fn resolve(&mut self, resolver: &mut NameResolver) {
    self.pattern.resolve(resolver);
  }
}

impl Resolve for ast::BreakStmt {
  //
}

impl Resolve for ast::LoopStmt {
  fn declare(&self, resolver: &mut NameResolver) {
    if let Some(condition) = &self.condition {
      condition.declare(resolver);
    }

    self.body.declare(resolver);
  }

  fn resolve(&mut self, resolver: &mut NameResolver) {
    if let Some(condition) = &mut self.condition {
      condition.resolve(resolver);
    }

    self.body.resolve(resolver);
  }
}

impl Resolve for ast::IfExpr {
  fn declare(&self, resolver: &mut NameResolver) {
    self.condition.declare(resolver);
    self.then_value.declare(resolver);

    if let Some(else_block) = &self.else_value {
      else_block.declare(resolver);
    }
  }

  fn resolve(&mut self, resolver: &mut NameResolver) {
    self.condition.resolve(resolver);
    self.then_value.resolve(resolver);

    if let Some(else_block) = &mut self.else_value {
      else_block.resolve(resolver);
    }
  }
}

impl Resolve for ast::LetStmt {
  fn declare(&self, resolver: &mut NameResolver) {
    resolver.declare_symbol((self.name.clone(), SymbolKind::Definition), self.binding_id);
    self.value.declare(resolver);
  }

  fn resolve(&mut self, resolver: &mut NameResolver) {
    // BUG: The problem seems to be occurring only when using let-statements. Investigate.
    // ... On the second iteration of the resolve step only! During cached nodes resolution.

    self.value.resolve(resolver);
    self.ty.resolve(resolver);
  }
}

impl Resolve for ast::ReturnStmt {
  fn declare(&self, resolver: &mut NameResolver) {
    if let Some(value) = &self.value {
      value.declare(resolver);
    }
  }

  fn resolve(&mut self, resolver: &mut NameResolver) {
    if let Some(value) = &mut self.value {
      value.resolve(resolver);
    }
  }
}

impl Resolve for ast::BlockExpr {
  fn declare(&self, resolver: &mut NameResolver) {
    resolver.push_scope();

    for statement in &self.statements {
      statement.declare(resolver);
    }

    resolver.close_scope_tree(self.binding_id);
  }

  fn resolve(&mut self, resolver: &mut NameResolver) {
    resolver.current_block_binding_id = Some(self.binding_id);

    for statement in &mut self.statements {
      statement.resolve(resolver);
    }

    resolver.current_block_binding_id = None;
  }
}

impl Resolve for ast::Literal {
  //
}

impl Resolve for ast::Function {
  fn declare(&self, resolver: &mut NameResolver) {
    // Parameter scope.
    // resolver.push_scope();

    // FIXME: Commented out temporarily. Is it needed?
    // NOTE: The scope tree won't be overwritten by the block's, nor the
    // prototype's scope tree, instead they will be merged, as expected.
    // resolver.close_scope_tree(self.value.binding_id);

    self.prototype.declare(resolver);
    self.body_value.declare(resolver);
    resolver.declare_symbol((self.name.clone(), SymbolKind::Definition), self.binding_id);
  }

  // REVIEW: This resolve step may need to be repeated for closure.
  fn resolve(&mut self, resolver: &mut NameResolver) {
    // REVIEW: Do we need scope management here, for the prototype's parameters?
    self.prototype.resolve(resolver);

    // Finally, after both the prototype and its return type have been resolved,
    // proceed to resolve the body.
    self.body_value.resolve(resolver);
  }
}

impl Resolve for ast::ExternFunction {
  fn declare(&self, resolver: &mut NameResolver) {
    resolver.declare_symbol((self.name.clone(), SymbolKind::Definition), self.binding_id);
  }

  fn resolve(&mut self, resolver: &mut NameResolver) {
    self.prototype.resolve(resolver);
  }
}

impl Resolve for ast::CallExpr {
  fn declare(&self, resolver: &mut NameResolver) {
    // Declare any possible `Definition` nodes in the arguments
    // (such as inline closures, etc.).
    for argument in &self.arguments {
      argument.declare(resolver);
    }
  }

  fn resolve(&mut self, resolver: &mut NameResolver) {
    self.callee_expr.resolve(resolver);

    for argument in &mut self.arguments {
      println!("resolve ... {:?}\n\n\n", argument);
      argument.resolve(resolver);
    }
  }
}

impl Resolve for ast::InlineExprStmt {
  fn declare(&self, resolver: &mut NameResolver) {
    self.expr.declare(resolver);
  }

  fn resolve(&mut self, resolver: &mut NameResolver) {
    self.expr.resolve(resolver);
  }
}

impl Resolve for ast::BinaryExpr {
  fn declare(&self, resolver: &mut NameResolver) {
    self.left.declare(resolver);
    self.right.declare(resolver);
  }

  fn resolve(&mut self, resolver: &mut NameResolver) {
    self.left.resolve(resolver);
    self.right.resolve(resolver);
  }
}

#[derive(Clone)]
pub struct NameResolver {
  pub diagnostic_builder: diagnostic::DiagnosticBuilder,
  current_module_name: Option<String>,
  /// Contains the modules with their respective top-level definitions.
  pub global_scopes: std::collections::HashMap<String, Scope>,
  /// Contains volatile, relative scopes. Only used during the declare step.
  /// This is reset when the module changes, although by that time, all the
  /// relative scopes should have been popped automatically.
  pub relative_scopes: Vec<Scope>,
  /// A mapping of a scope's unique key to its own scope, and all visible parent
  /// relative scopes, excluding the global scope.
  scope_map: std::collections::HashMap<cache::BindingId, Vec<Scope>>,
  /// The unique id of the current block's scope. Used in the resolve step.
  current_block_binding_id: Option<cache::BindingId>,
  current_struct_type_id: Option<cache::BindingId>,
}

impl NameResolver {
  pub fn new() -> Self {
    Self {
      diagnostic_builder: diagnostic::DiagnosticBuilder::new(),
      current_module_name: None,
      global_scopes: std::collections::HashMap::new(),
      relative_scopes: Vec::new(),
      scope_map: std::collections::HashMap::new(),
      current_block_binding_id: None,
      current_struct_type_id: None,
    }
  }

  /// Set per-file. A new global scope is created per-module.
  pub fn create_module(&mut self, name: String) {
    // REVIEW: Can the module name possibly collide with an existing one?

    self.current_module_name = Some(name.clone());

    self
      .global_scopes
      .insert(name, std::collections::HashMap::new());

    self.relative_scopes.clear();
  }

  pub fn set_active_module(&mut self, name: String) -> bool {
    // TODO: Implement checks (that module exists, etc.).
    // REVIEW: Shouldn't we reset buffers here? This might prevent the re-definition bug.
    self.current_module_name = Some(name.clone());

    true
  }

  // FIXME: What about registering on the cache? If this is implemented, there is no longer a need to register
  // ... the root nodes on the cache.
  fn declare_symbol(&mut self, symbol: Symbol, binding_id: cache::BindingId) {
    // Check for existing definitions.
    if self.contains_current_scope(&symbol) {
      self
        .diagnostic_builder
        .error(format!("re-definition of `{}`", symbol.0));

      // REVIEW: What about calling the child's declare function?
      return;
    }

    // Bind the symbol to the current scope for name resolution lookup.
    self.bind(symbol.clone(), binding_id);
  }

  /// Retrieve the last pushed relative scope, or if there are none,
  /// the global scope of the current module.
  fn get_current_scope(&mut self) -> &mut Scope {
    if self.relative_scopes.is_empty() {
      return self
        .global_scopes
        .get_mut(self.current_module_name.as_ref().unwrap())
        .unwrap();
    }

    self.relative_scopes.last_mut().unwrap()
  }

  // REVIEW: Consider returning the pushed scope? Unless it's not actually used.
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
  /// a scope tree. This tree will then be inserted into the scope map.
  ///
  /// If an entry with the same unique id already exists, the scope tree will
  /// be appended onto the existing definition.
  fn close_scope_tree(&mut self, binding_id: cache::BindingId) {
    let mut scope_tree = vec![self.force_pop_scope()];

    // Clone the relative scope tree.
    scope_tree.extend(self.relative_scopes.iter().rev().cloned());

    let mut final_value_buffer = scope_tree;

    // Append to the existing definition, if applicable.
    if self.scope_map.contains_key(&binding_id) {
      final_value_buffer.extend(self.scope_map.remove(&binding_id).unwrap());
    }

    self.scope_map.insert(binding_id, final_value_buffer);
  }

  /// Register a name on the last scope for name resolution lookups.
  ///
  /// If there are no relative scopes, the symbol is registered in the global scope.
  fn bind(&mut self, symbol: Symbol, binding_id: cache::BindingId) {
    self.get_current_scope().insert(symbol, binding_id);
  }

  /// Lookup a symbol starting from the nearest scope, all the way to the global scope
  /// of the current module.
  fn lookup(&mut self, symbol: &Symbol) -> Option<&cache::BindingId> {
    // If applicable, lookup on the relative scopes. This may not
    // be the case for when resolving global entities such as struct
    // types that reference other structs in their fields (in such case,
    // the relative scopes will be empty and the `current_block_binding_id`
    // buffer would be `None`).
    if let Some(current_block_binding_id) = self.current_block_binding_id {
      let scope_tree = self.scope_map.get(&current_block_binding_id).unwrap();

      // First, attempt to find the symbol in the relative scopes.
      for scope in scope_tree {
        if let Some(binding_id) = scope.get(&symbol) {
          return Some(binding_id);
        }
      }
    }

    // Otherwise, attempt to find the symbol in the current module's global scope.
    let global_scope = self
      .global_scopes
      .get(self.current_module_name.as_ref().unwrap())
      .unwrap();

    if let Some(binding_id) = global_scope.get(&symbol) {
      return Some(binding_id);
    }

    None
  }

  fn lookup_or_error(&mut self, symbol: &Symbol) -> Option<cache::BindingId> {
    if let Some(binding_id) = self.lookup(symbol) {
      return Some(binding_id.clone());
    }

    self
      .diagnostic_builder
      .error(format!("undefined reference to `{}`", symbol.0));

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
