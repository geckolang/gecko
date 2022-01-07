use crate::{ast, context, diagnostic};

#[derive(Hash, PartialEq, Eq, Clone)]
pub enum SymbolKind {
  VariableOrParameter,
  FunctionOrExtern,
}

pub trait Resolvable {
  fn declare(&mut self, _resolver: &mut NameResolver, _context: &mut context::Context) {
    //
  }

  fn resolve(&mut self, _resolver: &mut NameResolver, _context: &mut context::Context) {
    //
  }
}

impl Resolvable for ast::Node {
  fn declare(&mut self, resolver: &mut NameResolver, context: &mut context::Context) {
    crate::dispatch!(self, Resolvable::declare, resolver, context);
  }

  fn resolve(&mut self, resolver: &mut NameResolver, context: &mut context::Context) {
    crate::dispatch!(self, Resolvable::resolve, resolver, context);
  }
}

impl Resolvable for ast::Enum {
  //
}

impl Resolvable for ast::VariableAssignStmt {
  fn resolve(&mut self, resolver: &mut NameResolver, context: &mut context::Context) {
    self.value.resolve(resolver, context);

    // TODO: A bit misleading, since `lookup_or_error` returns `Option<>`.
    self.definition_key =
      resolver.lookup_or_error(&(self.name.clone(), SymbolKind::VariableOrParameter));
  }
}

impl Resolvable for ast::ContinueStmt {
  //
}

impl Resolvable for ast::ArrayIndexing {
  fn resolve(&mut self, resolver: &mut NameResolver, context: &mut context::Context) {
    self.index.resolve(resolver, context);

    // TODO: A bit misleading, since `lookup_or_error` returns `Option<>`.
    self.definition_key =
      resolver.lookup_or_error(&(self.name.clone(), SymbolKind::VariableOrParameter));
  }
}

impl Resolvable for ast::ArrayValue {
  fn resolve(&mut self, resolver: &mut NameResolver, context: &mut context::Context) {
    for element in &mut self.elements {
      element.resolve(resolver, context);
    }
  }
}

impl Resolvable for ast::ArrayAssignStmt {
  fn resolve(&mut self, resolver: &mut NameResolver, context: &mut context::Context) {
    self.value.resolve(resolver, context);

    // TODO: A bit misleading, since `lookup_or_error` returns `Option<>`.
    self.definition_key =
      resolver.lookup_or_error(&(self.name.clone(), SymbolKind::VariableOrParameter));
  }
}

impl Resolvable for ast::UnsafeBlock {
  fn declare(&mut self, resolver: &mut NameResolver, context: &mut context::Context) {
    self.0.declare(resolver, context);
  }

  fn resolve(&mut self, resolver: &mut NameResolver, context: &mut context::Context) {
    self.0.resolve(resolver, context);
  }
}

impl Resolvable for ast::Parameter {
  //
}

impl Resolvable for ast::VariableRef {
  fn resolve(&mut self, resolver: &mut NameResolver, _context: &mut context::Context) {
    // TODO: A bit misleading, since `lookup_or_error` returns `Option<>`.
    self.definition_key =
      resolver.lookup_or_error(&(self.name.clone(), SymbolKind::VariableOrParameter));
  }
}

impl Resolvable for ast::BreakStmt {
  //
}

impl Resolvable for ast::WhileStmt {
  fn declare(&mut self, resolver: &mut NameResolver, context: &mut context::Context) {
    self.condition.declare(resolver, context);
    self.body.declare(resolver, context);
  }

  fn resolve(&mut self, resolver: &mut NameResolver, context: &mut context::Context) {
    self.condition.resolve(resolver, context);
    self.body.resolve(resolver, context);
  }
}

impl Resolvable for ast::IfStmt {
  fn declare(&mut self, resolver: &mut NameResolver, context: &mut context::Context) {
    self.condition.declare(resolver, context);
    self.then_block.declare(resolver, context);
    // TODO: `else` block.
  }

  fn resolve(&mut self, resolver: &mut NameResolver, context: &mut context::Context) {
    self.condition.resolve(resolver, context);
    self.then_block.resolve(resolver, context);
    // TODO: `else` block.
  }
}

impl Resolvable for ast::LetStmt {
  fn resolve(&mut self, resolver: &mut NameResolver, context: &mut context::Context) {
    self.value.resolve(resolver, context);
  }
}

impl Resolvable for ast::ReturnStmt {
  fn declare(&mut self, resolver: &mut NameResolver, context: &mut context::Context) {
    if let Some(value) = &mut self.value {
      value.declare(resolver, context);
    }
  }

  fn resolve(&mut self, resolver: &mut NameResolver, context: &mut context::Context) {
    if let Some(value) = &mut self.value {
      value.resolve(resolver, context);
    }
  }
}

impl Resolvable for ast::Block {
  fn declare(&mut self, resolver: &mut NameResolver, context: &mut context::Context) {
    for statement in &mut self.statements {
      statement.declare(resolver, context);
    }
  }

  fn resolve(&mut self, resolver: &mut NameResolver, context: &mut context::Context) {
    resolver.push_scope();

    for statement in &mut self.statements {
      statement.resolve(resolver, context);
    }

    resolver.pop_scope();
  }
}

impl Resolvable for ast::Literal {
  //
}

impl Resolvable for ast::Function {
  fn declare(&mut self, resolver: &mut NameResolver, context: &mut context::Context) {
    // TODO: Simplify this process.
    match &mut self.prototype {
      ast::Type::Prototype(parameters, _, _) => {
        for parameter in parameters {
          parameter.declare(resolver, context);
        }
      }
      _ => unreachable!(),
    };

    self.body.declare(resolver, context);
  }

  fn resolve(&mut self, resolver: &mut NameResolver, context: &mut context::Context) {
    // TODO: Simplify this process.
    match &mut self.prototype {
      ast::Type::Prototype(parameters, _, _) => {
        for parameter in parameters {
          parameter.resolve(resolver, context);
        }
      }
      _ => unreachable!(),
    };

    self.body.resolve(resolver, context);
  }
}

impl Resolvable for ast::Extern {
  //
}

impl Resolvable for ast::Definition {
  fn declare(&mut self, resolver: &mut NameResolver, context: &mut context::Context) {
    // TODO: Proper naming for keys.

    // Register the node on the context for lowering lookup.
    context
      .declarations
      .insert(self.key, std::rc::Rc::clone(&self.node));

    // Bind the symbol to the current scope for name resolution lookup.
    resolver.bind((self.name.clone(), self.symbol_kind.clone()), self.key);

    self.node.as_ref().borrow_mut().declare(resolver, context);
  }

  fn resolve(&mut self, resolver: &mut NameResolver, context: &mut context::Context) {
    self.node.as_ref().borrow_mut().resolve(resolver, context);
  }
}

impl Resolvable for ast::FunctionCall {
  fn resolve(&mut self, resolver: &mut NameResolver, context: &mut context::Context) {
    // TODO: This might be simplified to just looking up on the global table, however, we need to take into account support for modules.
    // TODO: A bit misleading, since `lookup_or_error` returns `Option<>`.
    self.callee_definition_key =
      resolver.lookup_or_error(&(self.callee_name.clone(), SymbolKind::FunctionOrExtern));

    for argument in &mut self.arguments {
      argument.resolve(resolver, context);
    }
  }
}

impl Resolvable for ast::ExprWrapperStmt {
  fn resolve(&mut self, resolver: &mut NameResolver, context: &mut context::Context) {
    self.expr.resolve(resolver, context);
  }
}

impl Resolvable for ast::BinaryExpr {
  fn declare(&mut self, resolver: &mut NameResolver, context: &mut context::Context) {
    self.left.declare(resolver, context);
    self.right.declare(resolver, context);
  }

  fn resolve(&mut self, resolver: &mut NameResolver, context: &mut context::Context) {
    self.left.resolve(resolver, context);
    self.right.resolve(resolver, context);
  }
}

pub struct NameResolver {
  pub diagnostics: diagnostic::DiagnosticBuilder,
  // TODO: Should this be on `context::Context` instead? Something's missing. We might need to link the context to the resolver.
  scopes: Vec<std::collections::HashMap<(String, SymbolKind), context::DefinitionKey>>,
  global_scope: std::collections::HashMap<(String, SymbolKind), context::DefinitionKey>,
}

impl NameResolver {
  pub fn new() -> Self {
    Self {
      diagnostics: diagnostic::DiagnosticBuilder::new(),
      scopes: vec![std::collections::HashMap::new()],
      global_scope: std::collections::HashMap::new(),
    }
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
  fn bind(&mut self, key: (String, SymbolKind), definition_key: context::DefinitionKey) {
    if self.scopes.is_empty() {
      self.global_scope.insert(key, definition_key);
    } else {
      self.scopes.last_mut().unwrap().insert(key, definition_key);
    }
  }

  /// Lookup a symbol starting from the nearest scope, all the way to the global scope.
  fn lookup(&self, key: &(String, SymbolKind)) -> Option<&context::DefinitionKey> {
    // First, look on relative scopes.
    for scope in self.scopes.iter().rev() {
      if let Some(definition_key) = scope.get(&key) {
        return Some(definition_key);
      }
    }

    // Finally, look in the global scope.
    if let Some(definition_key) = self.global_scope.get(&key) {
      return Some(definition_key);
    }

    None
  }

  fn lookup_or_error(&mut self, key: &(String, SymbolKind)) -> Option<context::DefinitionKey> {
    if let Some(definition_key) = self.lookup(key).cloned() {
      return Some(definition_key);
    }

    self
      .diagnostics
      .error(format!("undefined reference to `{}`", key.0));

    None
  }
}

// TODO: Add essential tests.
