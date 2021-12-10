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

impl Resolvable for ast::Node<'_> {
  fn declare(&mut self, resolver: &mut NameResolver) {
    crate::dispatch!(self, Resolvable::declare, resolver);
  }

  fn resolve(&mut self, resolver: &mut NameResolver) {
    crate::dispatch!(self, Resolvable::resolve, resolver);
  }
}

impl Resolvable for ast::Function<'_> {
  //
}

impl Resolvable for ast::CallExpr<'_> {
  //
}

pub struct NameResolver {
  //
}

impl<'a> NameResolver {
  pub fn begin(mut node: ast::Node<'a>) {
    let mut resolver = NameResolver {};

    node.declare(&mut resolver);
    node.resolve(&mut resolver);
  }
}
