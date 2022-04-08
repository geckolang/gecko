use crate::ast;

pub struct NA;
pub struct NB;

pub enum N {
  NA(NA),
  NB(NA),
}

impl Visitable for N {
  fn accept<T>(&mut self, visitor: &mut impl Visitor<T>) -> T {
    match self {
      N::NA(na) => visitor.visit_na(na),
      N::NB(nb) => visitor.visit_na(nb),
    }
  }
}

pub trait Visitable {
  fn accept<T>(&mut self, visitor: &mut impl Visitor<T>) -> T;
}

pub trait Visitor<T> {
  fn visit(&mut self, node: &mut impl Visitable) -> T;

  fn visit_literal(&mut self, _node: &mut ast::Literal) -> T;

  fn visit_extern_function(&mut self, node: &mut ast::ExternFunction) -> T;

  fn visit_prototype(&mut self, _node: &mut ast::Prototype) -> T;

  fn visit_extern_static(&mut self, _node: &mut ast::ExternStatic) -> T;

  fn visit_na(&mut self, _node: &mut NA) -> T;
}

pub struct A;

impl Visitor<()> for A {
  fn visit(&mut self, node: &mut impl Visitable) {
    node.accept(self);
  }

  fn visit_literal(&mut self, _node: &mut ast::Literal) -> () {
    //
  }

  fn visit_extern_function(&mut self, _node: &mut ast::ExternFunction) -> () {
    //
  }

  fn visit_prototype(&mut self, _node: &mut ast::Prototype) -> () {
    //
  }

  fn visit_extern_static(&mut self, _node: &mut ast::ExternStatic) -> () {
    //
  }

  fn visit_na(&mut self, _node: &mut NA) -> () {
    //
  }
}

pub struct B {
  pub vs: Vec<i32>,
}

impl Visitor<i32> for B {
  fn visit(&mut self, node: &mut impl Visitable) -> i32 {
    node.accept(self)
  }

  fn visit_literal(&mut self, _node: &mut ast::Literal) -> i32 {
    self.vs.push(111);
    self.visit_na(&mut NA {});
    self.vs.push(3);

    1
  }

  fn visit_extern_function(&mut self, _node: &mut ast::ExternFunction) -> i32 {
    2
  }

  fn visit_prototype(&mut self, _node: &mut ast::Prototype) -> i32 {
    3
  }

  fn visit_extern_static(&mut self, _node: &mut ast::ExternStatic) -> i32 {
    4
  }

  fn visit_na(&mut self, _node: &mut NA) -> i32 {
    5
  }
}
