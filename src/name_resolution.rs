use crate::{ast, context};

// FIXME: Need to implement the calling of children for the declare step.

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
  fn declare(&mut self, _resolver: &mut NameResolver, _context: &mut context::Context) {
    // TODO: Implement this.
  }

  fn resolve(&mut self, resolver: &mut NameResolver, context: &mut context::Context) {
    self.value.resolve(resolver, context);
  }
}

impl Resolvable for ast::ReturnStmt {
  //
}

impl Resolvable for ast::Block {
  fn resolve(&mut self, resolver: &mut NameResolver, context: &mut context::Context) {
    resolver.push_scope();

    for statement in &mut self.statements {
      statement.resolve(resolver, context);
    }

    resolver.pop_scope();
  }
}

impl Resolvable for ast::Extern {
  //
}

impl Resolvable for ast::Literal {
  //
}

impl Resolvable for ast::Function {
  fn declare(&mut self, _resolver: &mut NameResolver, _context: &mut context::Context) {
    // TODO: Something's wrong. Scopes may be out of sync when switching declare/resolve step.

    // FIXME: Need to call `declare` step for the body (and possibly arguments?).
  }

  fn resolve(&mut self, resolver: &mut NameResolver, context: &mut context::Context) {
    resolver.push_scope();
    self.body.resolve(resolver, context);

    // TODO: Resolve arguments as well?

    resolver.pop_scope();
  }
}

impl Resolvable for ast::Definition {
  fn declare(&mut self, resolver: &mut NameResolver, context: &mut context::Context) {
    // TODO: Move the logic to the function call, to lower functions on-demand?
    match *self.node.borrow() {
      ast::Node::Function(_) => {
        let definition_key = resolver.create_definition_key();

        // FIXME: What is being cloned, the reference, or the underlying value itself?
        context
          .declarations
          .insert(definition_key, std::rc::Rc::clone(&self.node));

        resolver
          .scopes
          .last_mut()
          .unwrap()
          .insert(self.name.clone(), definition_key);
      }
      _ => {
        //
      }
    };

    self.node.as_ref().borrow_mut().declare(resolver, context);
  }

  fn resolve(&mut self, resolver: &mut NameResolver, context: &mut context::Context) {
    self.node.as_ref().borrow_mut().resolve(resolver, context);
  }
}

impl Resolvable for ast::FunctionCall {
  fn resolve(&mut self, resolver: &mut NameResolver, context: &mut context::Context) {
    // TODO: This might be simplified to just looking up on the global table, however, we need to take into account support for modules.
    if let Some(callee_key) = resolver.lookup(&self.callee_name) {
      // TODO: Cloning `callee_key`.
      self.callee_definition_key = Some(callee_key.clone());
    } else {
      // TODO: Use diagnostics.
      panic!("Callee not found");
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
  definition_key_counter: usize,
  // TODO: Should this be on `context::Context` instead? Something's missing. We might need to link the context to the resolver.
  scopes: Vec<std::collections::HashMap<String, context::DefinitionKey>>,
}

impl NameResolver {
  pub fn new() -> Self {
    Self {
      definition_key_counter: 0,
      scopes: vec![std::collections::HashMap::new()],
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

  fn create_definition_key(&mut self) -> context::DefinitionKey {
    let definition_key = self.definition_key_counter;

    self.definition_key_counter += 1;

    definition_key
  }
}
