use crate::{ast, context, diagnostic};

pub enum SymbolKind {
  LocalVariable,
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

impl Resolvable for ast::VariableRef {
  fn resolve(&mut self, resolver: &mut NameResolver, _context: &mut context::Context) {
    if let Some(definition_key) = resolver.lookup(&self.name, SymbolKind::LocalVariable) {
      self.definition_key = Some(definition_key.clone());
    } else {
      resolver
        .diagnostics
        .error(format!("undefined reference to variable `{}`", self.name));
    }
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
    self.condition.declare(resolver, context);
    self.body.declare(resolver, context);
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
  fn declare(&mut self, _resolver: &mut NameResolver, _context: &mut context::Context) {
    // TODO: Implement this.
  }

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
    // FIXME: The problem is that the scopes are destroyed before the `resolve` step.

    resolver.push_scope();

    for statement in &mut self.statements {
      statement.declare(resolver, context);
    }

    // resolver.pop_scope();
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
    // TODO: Something's wrong. Scopes may be out of sync when switching declare/resolve step.

    // FIXME: Need to call `declare` step for the body (and possibly arguments?).
    self.body.declare(resolver, context);
  }

  fn resolve(&mut self, resolver: &mut NameResolver, context: &mut context::Context) {
    // TODO: Scopes?
    self.body.resolve(resolver, context);
  }
}

impl Resolvable for ast::Extern {
  //
}

impl Resolvable for ast::Definition {
  fn declare(&mut self, resolver: &mut NameResolver, context: &mut context::Context) {
    // TODO: Proper naming for keys.
    let declaration_key = resolver.create_definition_key();

    // Register the node on the context for lowering lookup.
    context
      .declarations
      .insert(declaration_key, std::rc::Rc::clone(&self.node));

    // Register the name on the last scope for name resolution lookup.
    resolver
      .scopes
      .last_mut()
      .unwrap()
      .insert(self.name.clone(), declaration_key);

    self.node.as_ref().borrow_mut().declare(resolver, context);
  }

  fn resolve(&mut self, resolver: &mut NameResolver, context: &mut context::Context) {
    self.node.as_ref().borrow_mut().resolve(resolver, context);
  }
}

impl Resolvable for ast::FunctionCall {
  fn resolve(&mut self, resolver: &mut NameResolver, context: &mut context::Context) {
    // TODO: This might be simplified to just looking up on the global table, however, we need to take into account support for modules.
    if let Some(callee_key) = resolver.lookup(&self.callee_name, SymbolKind::FunctionOrExtern) {
      self.callee_definition_key = Some(callee_key.clone());
    } else {
      resolver.diagnostics.error(format!(
        "undefined reference to function `{}`",
        self.callee_name
      ));
    }

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

pub struct NameResolver {
  pub diagnostics: diagnostic::DiagnosticBuilder,
  definition_key_counter: usize,
  // TODO: Should this be on `context::Context` instead? Something's missing. We might need to link the context to the resolver.
  scopes: Vec<std::collections::HashMap<String, context::DefinitionKey>>,
  // TODO: Make use of the global scope.
  _global_scope: std::collections::HashMap<String, context::DefinitionKey>,
}

impl NameResolver {
  pub fn new() -> Self {
    Self {
      diagnostics: diagnostic::DiagnosticBuilder::new(),
      definition_key_counter: 0,
      scopes: vec![std::collections::HashMap::new()],
      _global_scope: std::collections::HashMap::new(),
    }
  }

  fn _is_global_scope(&self) -> bool {
    self.scopes.len() == 1
  }

  // TODO: Consider returning the pushed scope?
  fn push_scope(&mut self) {
    self.scopes.push(std::collections::HashMap::new());
  }

  fn pop_scope(&mut self) {
    self.scopes.pop();
  }

  fn _bind(&mut self, name: String, definition_key: context::DefinitionKey) {
    // TODO: What if there is no scopes? Maybe have an initial global scope set? Make use of the `global_scope` field.
    let scope = self.scopes.last_mut().unwrap();

    scope.insert(name, definition_key);
  }

  // TODO: How about taking in a tuple of (name, symbol_kind)? Then make the map have the same as its key.
  fn lookup(&self, name: &str, _symbol_kind: SymbolKind) -> Option<&usize> {
    // TODO: Make use of `symbol_kind`.

    for scope in self.scopes.iter().rev() {
      if let Some(definition_key) = scope.get(name) {
        return Some(definition_key);
      }
    }

    None
  }

  fn create_definition_key(&mut self) -> context::DefinitionKey {
    let definition_key = self.definition_key_counter;

    self.definition_key_counter += 1;

    definition_key
  }
}
