use crate::ast;

pub trait Visitor {
  fn visit(&mut self, node: &impl Visitor) {
    //
  }

  fn visit_literal(&mut self, _node: &ast::Literal) {
    //
  }

  fn visit_extern_function(&mut self, node: &ast::ExternFunction);

  fn visit_prototype(&mut self, _node: &ast::Prototype);

  fn visit_extern_static(&mut self, _node: &ast::ExternStatic);
}

struct TypeCheckVisitor {
  state: i32,
}
