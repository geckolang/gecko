use crate::{node, pass};

#[derive(Hash, Eq, PartialEq, Debug)]
pub struct Namespace {
  pub name: String,
  // pub args: (node::AnyKindNode<'a>, String),
  pub is_variadic: bool,
  pub return_kind: node::AnyKindNode,
}

impl node::Node for Namespace {
  fn accept(&mut self, pass: &dyn pass::Pass) {
    // pass.visit_prototype(self);
  }
}
