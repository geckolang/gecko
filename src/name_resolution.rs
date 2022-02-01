use crate::{ast, cache, diagnostic};

#[derive(Hash, PartialEq, Eq, Clone)]
pub enum SymbolKind {
  StaticOrVariableOrParameter,
  FunctionOrExtern,
  // A global type. Can be a struct, or enum.
  Type,
}

type Symbol = (String, SymbolKind);

type Scope = std::collections::HashMap<Symbol, cache::DefinitionKey>;

pub trait Resolve {
  fn declare(&mut self, _resolver: &mut NameResolver, _cache: &mut cache::Cache) {
    //
  }

  fn resolve(&mut self, _resolver: &mut NameResolver, _cache: &mut cache::Cache) {
    //
  }
}

impl Resolve for ast::Type {
  fn resolve(&mut self, resolver: &mut NameResolver, cache: &mut cache::Cache) {
    match self {
      ast::Type::UserDefined(user_defined_type) => {
        user_defined_type.resolve(resolver, cache);
      }
      ast::Type::Pointer(pointee_type) => {
        pointee_type.resolve(resolver, cache);
      }
      // FIXME: What about arrays, etc.?
      _ => {}
    };
  }
}

impl Resolve for ast::Node {
  // TODO: This `dispatch` may actually only apply for top-level nodes, so there might be room for simplification.

  fn declare(&mut self, resolver: &mut NameResolver, cache: &mut cache::Cache) {
    crate::dispatch!(self, Resolve::declare, resolver, cache);
  }

  fn resolve(&mut self, resolver: &mut NameResolver, cache: &mut cache::Cache) {
    crate::dispatch!(self, Resolve::resolve, resolver, cache);
  }
}

impl Resolve for ast::Pattern {
  fn resolve(&mut self, resolver: &mut NameResolver, _cache: &mut cache::Cache) {
    // let mut scope_buffer = None;

    // for static_path_segment in &mut self.static_path {
    //   // TODO: What about struct static variables?
    //   // let lookup_result =
    //   //   resolver.lookup((static_path_segment, SymbolKind::StaticOrVariableOrParameter));
    // }
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

impl Resolve for ast::UserDefinedType {
  fn resolve(&mut self, resolver: &mut NameResolver, _cache: &mut cache::Cache) {
    // TODO: A bit misleading, since `lookup_or_error` returns `Option<>`.
    self.target_key = resolver.lookup_or_error(&(self.name.clone(), SymbolKind::Type));
  }
}

impl Resolve for ast::StructValue {
  fn resolve(&mut self, resolver: &mut NameResolver, _cache: &mut cache::Cache) {
    // TODO: A bit misleading, since `lookup_or_error` returns `Option<>`.
    self.target_key = resolver.lookup_or_error(&(self.name.clone(), SymbolKind::Type));
  }
}

impl Resolve for ast::Prototype {
  fn declare(&mut self, resolver: &mut NameResolver, cache: &mut cache::Cache) {
    // TODO: This is sort of a hack.
    for parameter in &mut self.parameters {
      ast::Definition {
        name: parameter.0.clone(),
        symbol_kind: SymbolKind::StaticOrVariableOrParameter,
        // TODO: Cloning parameter.
        node_ref_cell: cache::create_cached_node(ast::Node::Parameter(parameter.clone())),
        // TODO: Will this `declare` function ever be called more than once? If so, this could be a problem.
        definition_key: cache.create_definition_key(),
      }
      .declare(resolver, cache);
    }
  }

  fn resolve(&mut self, resolver: &mut NameResolver, cache: &mut cache::Cache) {
    for parameter in &mut self.parameters {
      parameter.1.resolve(resolver, cache);
    }

    if let Some(return_type) = &mut self.return_type {
      return_type.resolve(resolver, cache);
    }
  }
}

impl Resolve for ast::StructType {
  fn declare(&mut self, _resolver: &mut NameResolver, _cache: &mut cache::Cache) {
    // TODO: Implement?
  }

  fn resolve(&mut self, _resolver: &mut NameResolver, _cache: &mut cache::Cache) {
    // TODO: Implement?
  }
}

impl Resolve for ast::UnaryExpr {
  fn resolve(&mut self, resolver: &mut NameResolver, cache: &mut cache::Cache) {
    self.expr.resolve(resolver, cache);
  }
}

impl Resolve for ast::Enum {
  //
}

impl Resolve for ast::AssignStmt {
  fn resolve(&mut self, resolver: &mut NameResolver, cache: &mut cache::Cache) {
    self.assignee_expr.resolve(resolver, cache);
    self.value.resolve(resolver, cache);
  }
}

impl Resolve for ast::ContinueStmt {
  //
}

impl Resolve for ast::ArrayIndexing {
  fn resolve(&mut self, resolver: &mut NameResolver, cache: &mut cache::Cache) {
    self.index.resolve(resolver, cache);

    self.target_key =
      resolver.lookup_or_error(&(self.name.clone(), SymbolKind::StaticOrVariableOrParameter));
  }
}

impl Resolve for ast::ArrayValue {
  fn resolve(&mut self, resolver: &mut NameResolver, cache: &mut cache::Cache) {
    for element in &mut self.elements {
      element.resolve(resolver, cache);
    }
  }
}

impl Resolve for ast::UnsafeBlockStmt {
  fn declare(&mut self, resolver: &mut NameResolver, cache: &mut cache::Cache) {
    self.0.declare(resolver, cache);
  }

  fn resolve(&mut self, resolver: &mut NameResolver, cache: &mut cache::Cache) {
    self.0.resolve(resolver, cache);
  }
}

impl Resolve for ast::Parameter {
  //
}

impl Resolve for ast::VariableOrMemberRef {
  fn resolve(&mut self, resolver: &mut NameResolver, _cache: &mut cache::Cache) {
    // TODO: A bit misleading, since `lookup_or_error` returns `Option<>`.
    // FIXME: Only accessing the base name of the pattern.
    self.target_key = resolver.lookup_or_error(&(
      self.pattern.base_name.clone(),
      SymbolKind::StaticOrVariableOrParameter,
    ));
  }
}

impl Resolve for ast::BreakStmt {
  //
}

impl Resolve for ast::LoopStmt {
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

impl Resolve for ast::IfStmt {
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

impl Resolve for ast::LetStmt {
  fn resolve(&mut self, resolver: &mut NameResolver, cache: &mut cache::Cache) {
    self.ty.resolve(resolver, cache);
    self.value.resolve(resolver, cache);
  }
}

impl Resolve for ast::ReturnStmt {
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

impl Resolve for ast::Block {
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

impl Resolve for ast::Literal {
  //
}

impl Resolve for ast::Function {
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

impl Resolve for ast::ExternFunction {
  fn resolve(&mut self, resolver: &mut NameResolver, cache: &mut cache::Cache) {
    self.prototype.resolve(resolver, cache);
  }
}

impl Resolve for ast::Definition {
  fn declare(&mut self, resolver: &mut NameResolver, cache: &mut cache::Cache) {
    let symbol = (self.name.clone(), self.symbol_kind.clone());

    // Check for existing definitions.
    if resolver.contains(&symbol) {
      resolver
        .diagnostic_builder
        .error(format!("re-definition of `{}`", self.name));

      return;
    }

    // Register the node on the cache for lowering lookup.
    cache.bind(self.definition_key, std::rc::Rc::clone(&self.node_ref_cell));

    // Bind the symbol to the current scope for name resolution lookup.
    resolver.bind(symbol, self.definition_key);

    self.node_ref_cell.borrow_mut().declare(resolver, cache);
  }

  fn resolve(&mut self, resolver: &mut NameResolver, cache: &mut cache::Cache) {
    self.node_ref_cell.borrow_mut().resolve(resolver, cache);
  }
}

impl Resolve for ast::FunctionCall {
  fn resolve(&mut self, resolver: &mut NameResolver, cache: &mut cache::Cache) {
    // TODO: This might be simplified to just looking up on the global table, however, we need to take into account support for modules.
    // TODO: A bit misleading, since `lookup_or_error` returns `Option<>`.
    // TODO: Only the base name is being used from `callee_id`.
    self.target_key = resolver.lookup_or_error(&(
      self.callee_pattern.base_name.clone(),
      SymbolKind::FunctionOrExtern,
    ));

    for argument in &mut self.arguments {
      argument.resolve(resolver, cache);
    }
  }
}

impl Resolve for ast::ExprStmt {
  fn resolve(&mut self, resolver: &mut NameResolver, cache: &mut cache::Cache) {
    self.expr.resolve(resolver, cache);
  }
}

impl Resolve for ast::BinaryExpr {
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
  pub diagnostic_builder: diagnostic::DiagnosticBuilder,
  current_module_name: Option<String>,
  /// Contains all declarations of the program, including absolute
  /// (modules) and relative (variables).
  scopes: std::collections::HashMap<String, Vec<Scope>>,
}

impl NameResolver {
  pub fn new() -> Self {
    Self {
      diagnostic_builder: diagnostic::DiagnosticBuilder::new(),
      current_module_name: None,
      scopes: std::collections::HashMap::new(),
    }
  }

  /// Set per-file. A new global scope is created per-module.
  pub fn set_current_module(&mut self, name: String) {
    // TODO: Can the module name possibly collide with an existing one?

    self.current_module_name = Some(name.clone());

    self
      .scopes
      .insert(name, vec![std::collections::HashMap::new()]);
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
        .diagnostic_builder
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

  /// Retrieve the global scope of the current module.
  fn get_scope_stack(&mut self) -> &mut Vec<Scope> {
    self
      .scopes
      .get_mut(self.current_module_name.as_ref().unwrap())
      .unwrap()
  }

  fn get_current_scope(&mut self) -> &mut Scope {
    self.get_scope_stack().last_mut().unwrap()
  }

  // TODO: Consider returning the pushed scope? Unless it's not actually used.
  fn push_scope(&mut self) {
    self
      .get_scope_stack()
      .push(std::collections::HashMap::new());
  }

  fn pop_scope(&mut self) {
    self.get_scope_stack().pop();
  }

  /// Register a name on the last scope for name resolution lookups.
  ///
  /// If there are no relative scopes, the symbol is registered in the global scope.
  fn bind(&mut self, symbol: Symbol, definition_key: cache::DefinitionKey) {
    self.get_current_scope().insert(symbol, definition_key);
  }

  /// Lookup a symbol starting from the nearest scope, all the way to the global scope.
  fn lookup(&mut self, symbol: &Symbol) -> Option<&cache::DefinitionKey> {
    for scope in self.get_scope_stack().iter().rev() {
      if let Some(definition_key) = scope.get(&symbol) {
        return Some(definition_key);
      }
    }

    None
  }

  fn lookup_or_error(&mut self, symbol: &Symbol) -> Option<cache::DefinitionKey> {
    if let Some(definition_key) = self.lookup(symbol).cloned() {
      return Some(definition_key);
    }

    self
      .diagnostic_builder
      .error(format!("undefined reference to `{}`", symbol.0));

    None
  }

  // FIXME: The current design for this function might be flawed. Review.
  fn _lookup_member(
    &mut self,
    pattern: &ast::Pattern,
    cache: &cache::Cache,
  ) -> Option<&cache::DefinitionKey> {
    let definition_key = self.lookup(&(
      pattern.base_name.clone(),
      SymbolKind::StaticOrVariableOrParameter,
    ));

    if let Some(definition) = definition_key {
      let declaration = cache.declarations.get(definition).unwrap().borrow();

      match &*declaration {
        ast::Node::LetStmt(let_stmt) => {
          // TODO: Consider resolving/unboxing the type, instead of doing it manually.

          let user_defined_type_target_key = match &let_stmt.ty {
            ast::Type::UserDefined(ast::UserDefinedType {
              name: _,
              target_key,
            }) => target_key.unwrap(),
            _ => unreachable!(),
          };

          let user_defined_type_declaration = cache
            .declarations
            .get(&user_defined_type_target_key)
            .unwrap()
            .borrow();

          let _struct_type = match &*user_defined_type_declaration {
            ast::Node::StructType(struct_type) => struct_type,
            _ => unreachable!(),
          };

          // FIXME: There IS no definition key for struct members. So what should be do instead? Maybe this should be handled instead during the `declare` step for `LetStmt` (in the case that `LetStmt` declares a resolved-type of struct)?
        }
        // TODO: Add logic for parameters.
        // It must be a variable declaration or parameter, containing a struct.
        _ => unreachable!(),
      };
    }

    None
  }

  fn contains(&mut self, key: &Symbol) -> bool {
    self.lookup(key).is_some()
  }
}

// TODO: Add essential tests.
