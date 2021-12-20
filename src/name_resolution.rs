use crate::{ast, context};

trait Resolvable {
  fn resolve(&mut self, _resolver: &mut NameResolver, _context: &mut context::Context) {
    //
  }
}

impl Resolvable for ast::Node {
  fn resolve(&mut self, resolver: &mut NameResolver, context: &mut context::Context) {
    crate::dispatch!(self, Resolvable::resolve, resolver, context);
  }
}

impl Resolvable for ast::BreakStmt {
  //
}

impl Resolvable for ast::WhileStmt {
  //
}

impl Resolvable for ast::IfStmt {
  //
}

impl Resolvable for ast::LetStmt {
  //
}

impl Resolvable for ast::ReturnStmt {
  //
}

impl Resolvable for ast::Block {
  fn resolve(&mut self, resolver: &mut NameResolver, context: &mut context::Context) {
    for statement in self.statements.iter_mut() {
      statement.resolve(resolver, context);
    }
  }
}

impl Resolvable for ast::Extern {
  //
}

impl Resolvable for ast::Literal {
  //
}

impl Resolvable for ast::Function {
  fn resolve(&mut self, resolver: &mut NameResolver, context: &mut context::Context) {
    resolver.push_scope();
    self.body.resolve(resolver, context);

    // TODO: Resolve arguments as well.

    resolver.pop_scope();
  }
}

impl Resolvable for ast::FunctionCall {
  fn resolve(&mut self, resolver: &mut NameResolver, context: &mut context::Context) {
    if let Some(callee_definition) = resolver.lookup(&self.callee_name) {
      // TODO: Cloning `callee_definition`.
      self.callee_definition = Some(*callee_definition);
    } else {
      // TODO: Use diagnostics.
      panic!("Callee not found");
    }

    for argument in self.arguments.iter_mut() {
      argument.resolve(resolver, context);
    }
  }
}

impl Resolvable for ast::ExprWrapperStmt {
  //
}

impl Resolvable for ast::Definition {
  fn resolve(&mut self, _resolver: &mut NameResolver, _context: &mut context::Context) {
    //
    self.node.resolve(_resolver, _context);
  }
}

pub struct NameResolver {
  scopes: Vec<std::collections::HashMap<String, context::DefinitionKey>>,
}

impl NameResolver {
  pub fn new() -> Self {
    Self { scopes: Vec::new() }
  }

  fn is_global_scope(&self) -> bool {
    self.scopes.len() == 1
  }

  fn push_scope(&mut self) {
    self.scopes.push(std::collections::HashMap::new());
  }

  fn pop_scope(&mut self) {
    self.scopes.pop();
  }

  fn bind(&mut self, name: String, definition_key: context::DefinitionKey) {
    // TODO: What if there is no scopes? Maybe have an initial global scope set.
    let scope = self.scopes.last_mut().unwrap();

    scope.insert(name, definition_key);
  }

  fn lookup(&self, name: &str) -> Option<&usize> {
    for scope in self.scopes.iter().rev() {
      if let Some(definition_key) = scope.get(name) {
        return Some(definition_key);
      }
    }

    None
  }
}
