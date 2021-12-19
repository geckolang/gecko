use crate::ast;

trait Resolvable {
  /// Declares.
  fn declare(&mut self, _resolver: &mut NameResolver) {
    //
  }

  /// Resolves.
  fn resolve(&mut self, _resolver: &mut NameResolver) {
    //
  }
}

impl Resolvable for ast::Node {
  fn declare(&mut self, resolver: &mut NameResolver) {
    crate::dispatch!(self, Resolvable::declare, resolver);
  }

  fn resolve(&mut self, resolver: &mut NameResolver) {
    crate::dispatch!(self, Resolvable::resolve, resolver);
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

impl Resolvable for ast::BlockStmt {
  //
}

impl Resolvable for ast::Block {
  //
}

impl Resolvable for ast::Extern {
  //
}

impl Resolvable for ast::Literal {
  //
}

impl Resolvable for ast::Function {
  //
}

impl Resolvable for ast::CallExpr {
  //
}

pub struct NameResolver {
  //
}

impl<'a> NameResolver {
  pub fn begin(mut node: ast::Node) {
    let mut resolver = NameResolver {};

    node.declare(&mut resolver);
    node.resolve(&mut resolver);
  }
}
