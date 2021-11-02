use crate::{int_kind, pass, void_kind};

#[derive(Hash, Eq, PartialEq, Debug, Copy, Clone)]
pub enum AnyKindNode {
  IntKind(int_kind::IntKind),
  VoidKind(void_kind::VoidKind),
}

#[derive(Hash, Eq, PartialEq, Debug, Copy, Clone)]
pub enum AnyLiteralNode {
  BoolLiteral(BoolLiteral),
}

pub trait Node {
  fn accept(&mut self, pass: &mut dyn pass::Pass) -> pass::PassResult;

  fn get_children(&self) -> Vec<&dyn Node> {
    vec![]
  }
}

#[derive(Hash, Eq, PartialEq, Debug)]
pub struct Identifier {
  pub name: String,
}

#[derive(Hash, Eq, PartialEq, Debug, Copy, Clone)]
pub struct BoolLiteral {
  pub value: bool,
}

impl Node for BoolLiteral {
  fn accept(&mut self, pass: &mut dyn pass::Pass) -> pass::PassResult {
    pass.visit_bool_literal(self)
  }
}
