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

// TODO: This might be getting too complicated. Maybe we should keep it simple in this case?
impl Resolve for ast::Pattern {
  fn resolve(&mut self, resolver: &mut NameResolver, _cache: &mut cache::Cache) {
    // TODO: Consider extending this as a function of `Pattern` (via `impl`).
    let symbol = (self.base_name.clone(), self.symbol_kind.clone());

    let lookup_result = match self.symbol_kind {
      SymbolKind::StaticOrVariableOrParameter => resolver.relative_lookup(&symbol),
      SymbolKind::FunctionOrExtern => resolver.absolute_lookup(self),
      // TODO: What else? Maybe `unreachable!()`?
      _ => todo!(),
    };

    if let Some(target_key) = lookup_result {
      self.target_key = Some(target_key.clone());
    } else {
      // FIXME: Report diagnostics on the driver (grip).
      println!(" !!! PRODUCED ERROR !!!!");
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
  fn resolve(&mut self, resolver: &mut NameResolver, cache: &mut cache::Cache) {
    self.0.resolve(resolver, cache);
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
    println!(" ... defining : {}", self.name);

    let symbol = (self.name.clone(), self.symbol_kind.clone());

    // Check for existing definitions.
    if resolver.contains(&symbol) {
      resolver
        .diagnostic_builder
        .error(format!("re-definition of `{}`", self.name));

      // TODO: What about calling the child's declare function?
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
    self.callee_pattern.resolve(resolver, cache);

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
  /// Contains the modules with their respective top-level definitions.
  global_scopes: std::collections::HashMap<String, Scope>,
  /// Contains volatile, relative scopes. This is reset when the module changes.
  relative_scopes: Vec<Scope>,
}

impl NameResolver {
  pub fn new() -> Self {
    Self {
      diagnostic_builder: diagnostic::DiagnosticBuilder::new(),
      current_module_name: None,
      global_scopes: std::collections::HashMap::new(),
      relative_scopes: Vec::new(),
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

  pub fn set_active_module(&mut self, name: String) {
    // TODO: Unsafe? No checks.
    self.current_module_name = Some(name.clone());
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

  fn pop_scope(&mut self) {
    self.relative_scopes.pop();
  }

  /// Register a name on the last scope for name resolution lookups.
  ///
  /// If there are no relative scopes, the symbol is registered in the global scope.
  fn bind(&mut self, symbol: Symbol, definition_key: cache::DefinitionKey) {
    self.get_current_scope().insert(symbol, definition_key);
  }

  fn produce_lookup_error(&mut self, name: &String) {
    self
      .diagnostic_builder
      .error(format!("undefined reference to `{}`", name));
  }

  // Lookup the global scope of a specific module.
  fn absolute_lookup(&mut self, pattern: &ast::Pattern) -> Option<&cache::DefinitionKey> {
    // FIXME: Something here might be taking precedence where it shouldn't (bug occurs when two functions with same name on different modules).

    // TODO: Consider whether to clone or use references.
    let module_name = pattern
      .module_name
      .clone()
      .unwrap_or(self.current_module_name.clone().unwrap());

    let global_scope = self.global_scopes.get(&module_name).unwrap();
    let symbol = (pattern.base_name.clone(), pattern.symbol_kind.clone());

    if let Some(definition_key) = global_scope.get(&symbol) {
      return Some(definition_key);
    }

    None
  }

  /// Lookup a symbol starting from the nearest scope, all the way to the global scope
  /// of the current module.
  fn relative_lookup(&mut self, symbol: &Symbol) -> Option<&cache::DefinitionKey> {
    // First attempt to find the symbol in the relative scopes.
    for scope in self.relative_scopes.iter().rev() {
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

  fn lookup_or_error(&mut self, symbol: &Symbol) -> Option<cache::DefinitionKey> {
    if let Some(definition_key) = self.relative_lookup(symbol).cloned() {
      return Some(definition_key);
    }

    self.produce_lookup_error(&symbol.0);

    None
  }

  // FIXME: The current design for this function might be flawed. Review.
  fn _lookup_member(
    &mut self,
    pattern: &ast::Pattern,
    cache: &cache::Cache,
  ) -> Option<&cache::DefinitionKey> {
    let definition_key = self.relative_lookup(&(
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
    println!("-- contains check:");
    self.relative_lookup(key).is_some()
  }
}

// TODO: Add essential tests.
