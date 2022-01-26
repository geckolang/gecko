use crate::{ast, cache, diagnostic};

#[derive(Hash, PartialEq, Eq, Clone)]
pub enum SymbolKind {
  VariableOrParameter,
  FunctionOrExtern,
  // A global type. Can be a struct, or enum.
  Type,
}

type Symbol = (String, SymbolKind);

pub trait Resolvable {
  fn declare(&mut self, _resolver: &mut NameResolver, _cache: &mut cache::Cache) {
    //
  }

  fn resolve(&mut self, _resolver: &mut NameResolver, _cache: &mut cache::Cache) {
    //
  }
}

impl Resolvable for ast::Type {
  fn resolve(&mut self, resolver: &mut NameResolver, cache: &mut cache::Cache) {
    match self {
      ast::Type::UserDefined(user_defined_type) => {
        user_defined_type.resolve(resolver, cache);
      }
      _ => {}
    };
  }
}

impl Resolvable for ast::Node {
  // TODO: This `dispatch` may actually only apply for top-level nodes, so there might be room for simplification.

  fn declare(&mut self, resolver: &mut NameResolver, cache: &mut cache::Cache) {
    crate::dispatch!(self, Resolvable::declare, resolver, cache);
  }

  fn resolve(&mut self, resolver: &mut NameResolver, cache: &mut cache::Cache) {
    crate::dispatch!(self, Resolvable::resolve, resolver, cache);
  }
}

impl Resolvable for ast::UserDefinedType {
  fn resolve(&mut self, resolver: &mut NameResolver, _cache: &mut cache::Cache) {
    // TODO: A bit misleading, since `lookup_or_error` returns `Option<>`.
    self.target_key = resolver.lookup_or_error(&(self.name.clone(), SymbolKind::Type));
  }
}

impl Resolvable for ast::StructValue {
  fn resolve(&mut self, resolver: &mut NameResolver, _cache: &mut cache::Cache) {
    // TODO: A bit misleading, since `lookup_or_error` returns `Option<>`.
    self.target_key = resolver.lookup_or_error(&(self.name.clone(), SymbolKind::Type));
  }
}

impl Resolvable for ast::Prototype {
  fn declare(&mut self, _resolver: &mut NameResolver, _cache: &mut cache::Cache) {
    // FIXME: Must declare parameters (they aren't defined as `Definition`s). This issue prevents parameter usage.
  }

  fn resolve(&mut self, resolver: &mut NameResolver, cache: &mut cache::Cache) {
    if let Some(return_type) = &mut self.return_type {
      return_type.resolve(resolver, cache);
    }
  }
}

impl Resolvable for ast::StructType {
  fn declare(&mut self, _resolver: &mut NameResolver, _cache: &mut cache::Cache) {
    // TODO: Implement.
  }

  fn resolve(&mut self, _resolver: &mut NameResolver, _cache: &mut cache::Cache) {
    // TODO: Implement?
  }
}

impl Resolvable for ast::UnaryExpr {
  fn resolve(&mut self, resolver: &mut NameResolver, cache: &mut cache::Cache) {
    self.expr.resolve(resolver, cache);
  }
}

impl Resolvable for ast::Enum {
  //
}

impl Resolvable for ast::AssignStmt {
  fn resolve(&mut self, resolver: &mut NameResolver, cache: &mut cache::Cache) {
    self.assignee_expr.resolve(resolver, cache);
    self.value.resolve(resolver, cache);
  }
}

impl Resolvable for ast::ContinueStmt {
  //
}

impl Resolvable for ast::ArrayIndexing {
  fn resolve(&mut self, resolver: &mut NameResolver, cache: &mut cache::Cache) {
    self.index.resolve(resolver, cache);

    self.target_key =
      resolver.lookup_or_error(&(self.name.clone(), SymbolKind::VariableOrParameter));
  }
}

impl Resolvable for ast::ArrayValue {
  fn resolve(&mut self, resolver: &mut NameResolver, cache: &mut cache::Cache) {
    for element in &mut self.elements {
      element.resolve(resolver, cache);
    }
  }
}

impl Resolvable for ast::UnsafeBlockStmt {
  fn declare(&mut self, resolver: &mut NameResolver, cache: &mut cache::Cache) {
    self.0.declare(resolver, cache);
  }

  fn resolve(&mut self, resolver: &mut NameResolver, cache: &mut cache::Cache) {
    self.0.resolve(resolver, cache);
  }
}

impl Resolvable for ast::Parameter {
  //
}

impl Resolvable for ast::VariableRef {
  fn resolve(&mut self, resolver: &mut NameResolver, _cache: &mut cache::Cache) {
    // TODO: A bit misleading, since `lookup_or_error` returns `Option<>`.
    self.target_key =
      resolver.lookup_or_error(&(self.name.clone(), SymbolKind::VariableOrParameter));
  }
}

impl Resolvable for ast::BreakStmt {
  //
}

impl Resolvable for ast::LoopStmt {
  fn declare(&mut self, resolver: &mut NameResolver, cache: &mut cache::Cache) {
    if let Some(condition) = &mut self.condition {
      condition.declare(resolver, cache);
    }

    self.body.declare(resolver, cache);
  }

  fn resolve(&mut self, resolver: &mut NameResolver, cache: &mut cache::Cache) {
    if let Some(condition) = &mut self.condition {
      condition.resolve(resolver, cache);
    }

    self.body.resolve(resolver, cache);
  }
}

impl Resolvable for ast::IfStmt {
  fn declare(&mut self, resolver: &mut NameResolver, cache: &mut cache::Cache) {
    self.condition.declare(resolver, cache);
    self.then_block.declare(resolver, cache);
    // TODO: `else` block.
  }

  fn resolve(&mut self, resolver: &mut NameResolver, cache: &mut cache::Cache) {
    self.condition.resolve(resolver, cache);
    self.then_block.resolve(resolver, cache);
    // TODO: `else` block.
  }
}

impl Resolvable for ast::LetStmt {
  fn resolve(&mut self, resolver: &mut NameResolver, cache: &mut cache::Cache) {
    self.ty.resolve(resolver, cache);
    self.value.resolve(resolver, cache);
  }
}

impl Resolvable for ast::ReturnStmt {
  fn declare(&mut self, resolver: &mut NameResolver, cache: &mut cache::Cache) {
    if let Some(value) = &mut self.value {
      value.declare(resolver, cache);
    }
  }

  fn resolve(&mut self, resolver: &mut NameResolver, cache: &mut cache::Cache) {
    if let Some(value) = &mut self.value {
      value.resolve(resolver, cache);
    }
  }
}

impl Resolvable for ast::Block {
  fn declare(&mut self, resolver: &mut NameResolver, cache: &mut cache::Cache) {
    for statement in &mut self.statements {
      statement.declare(resolver, cache);
    }
  }

  fn resolve(&mut self, resolver: &mut NameResolver, cache: &mut cache::Cache) {
    resolver.push_scope();

    for statement in &mut self.statements {
      statement.resolve(resolver, cache);
    }

    resolver.pop_scope();
  }
}

impl Resolvable for ast::Literal {
  //
}

impl Resolvable for ast::Function {
  fn declare(&mut self, resolver: &mut NameResolver, cache: &mut cache::Cache) {
    self.prototype.declare(resolver, cache);
    self.body.declare(resolver, cache);
  }

  fn resolve(&mut self, resolver: &mut NameResolver, cache: &mut cache::Cache) {
    // TODO: Is there a need to resolve the prototype?
    self.prototype.resolve(resolver, cache);

    self.body.resolve(resolver, cache);
  }
}

impl Resolvable for ast::Extern {
  // TODO: Might need to call `resolve` on the return type (ex. for `UserDefinedType`)? Actually, invoking `resolve` on the prototype should be enough.
}

impl Resolvable for ast::Definition {
  fn declare(&mut self, resolver: &mut NameResolver, cache: &mut cache::Cache) {
    let symbol = (self.name.clone(), self.symbol_kind.clone());

    // Check for existing definitions.
    if resolver.contains(&symbol) {
      resolver
        .diagnostics
        .error(format!("re-definition of `{}`", self.name));

      return;
    }

    // Register the node on the cache for lowering lookup.
    cache.bind(self.definition_key, std::rc::Rc::clone(&self.node));

    // Bind the symbol to the current scope for name resolution lookup.
    resolver.bind(symbol, self.definition_key);

    self.node.as_ref().borrow_mut().declare(resolver, cache);
  }

  fn resolve(&mut self, resolver: &mut NameResolver, cache: &mut cache::Cache) {
    self.node.as_ref().borrow_mut().resolve(resolver, cache);
  }
}

impl Resolvable for ast::FunctionCall {
  fn resolve(&mut self, resolver: &mut NameResolver, cache: &mut cache::Cache) {
    // TODO: This might be simplified to just looking up on the global table, however, we need to take into account support for modules.
    // TODO: A bit misleading, since `lookup_or_error` returns `Option<>`.
    // TODO: Only the base name is being used from `callee_id`.
    self.target_key =
      resolver.lookup_or_error(&(self.callee_id.0.clone(), SymbolKind::FunctionOrExtern));

    for argument in &mut self.arguments {
      argument.resolve(resolver, cache);
    }
  }
}

impl Resolvable for ast::ExprStmt {
  fn resolve(&mut self, resolver: &mut NameResolver, cache: &mut cache::Cache) {
    self.expr.resolve(resolver, cache);
  }
}

impl Resolvable for ast::BinaryExpr {
  fn declare(&mut self, resolver: &mut NameResolver, cache: &mut cache::Cache) {
    self.left.declare(resolver, cache);
    self.right.declare(resolver, cache);
  }

  fn resolve(&mut self, resolver: &mut NameResolver, cache: &mut cache::Cache) {
    self.left.resolve(resolver, cache);
    self.right.resolve(resolver, cache);
  }
}

pub struct NameResolver {
  pub diagnostics: diagnostic::DiagnosticBuilder,
  // TODO: Should this be on `context::Context` instead? Something's missing. We might need to link the context to the resolver.
  scopes: Vec<std::collections::HashMap<Symbol, cache::DefinitionKey>>,
  global_scope: std::collections::HashMap<Symbol, cache::DefinitionKey>,
}

impl NameResolver {
  pub fn new() -> Self {
    Self {
      diagnostics: diagnostic::DiagnosticBuilder::new(),
      scopes: vec![std::collections::HashMap::new()],
      global_scope: std::collections::HashMap::new(),
    }
  }

  // TODO: Incomplete (cannot access `self` as `&Node`).
  fn _define(
    &mut self,
    definition_key: cache::DefinitionKey,
    symbol: Symbol,
    _cache: &mut cache::Cache,
    _node: &ast::Node,
  ) -> bool {
    // Check for existing definitions.
    if self.contains(&symbol) {
      self
        .diagnostics
        .error(format!("re-definition of `{}`", symbol.0));

      return false;
    }

    // Register the node on the context for lowering lookup.
    // TODO:
    // context
    //   .declarations
    //   .insert(definition_key, std::rc::Rc::clone(node));

    // Bind the symbol to the current scope for name resolution lookup.
    self.bind(symbol, definition_key);

    true
  }

  // TODO: Consider returning the pushed scope? Unless it's not actually used.
  fn push_scope(&mut self) {
    self.scopes.push(std::collections::HashMap::new());
  }

  fn pop_scope(&mut self) {
    self.scopes.pop();
  }

  /// Register a name on the last scope for name resolution lookups.
  ///
  /// If there are no relative scopes, the symbol is registered on the global scope.
  fn bind(&mut self, symbol: Symbol, definition_key: cache::DefinitionKey) {
    if self.scopes.is_empty() {
      self.global_scope.insert(symbol, definition_key);
    } else {
      self
        .scopes
        .last_mut()
        .unwrap()
        .insert(symbol, definition_key);
    }
  }

  /// Lookup a symbol starting from the nearest scope, all the way to the global scope.
  fn lookup(&self, symbol: &Symbol) -> Option<&cache::DefinitionKey> {
    // First, look on relative scopes.
    for scope in self.scopes.iter().rev() {
      if let Some(definition_key) = scope.get(&symbol) {
        return Some(definition_key);
      }
    }

    // Finally, look in the global scope.
    if let Some(definition_key) = self.global_scope.get(&symbol) {
      return Some(definition_key);
    }

    None
  }

  fn lookup_or_error(&mut self, symbol: &Symbol) -> Option<cache::DefinitionKey> {
    if let Some(definition_key) = self.lookup(symbol).cloned() {
      return Some(definition_key);
    }

    self
      .diagnostics
      .error(format!("undefined reference to `{}`", symbol.0));

    None
  }

  fn contains(&self, key: &Symbol) -> bool {
    self.lookup(key).is_some()
  }
}

// TODO: Add essential tests.
