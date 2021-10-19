use crate::node;
use crate::pass;

#[derive(Hash, Eq, PartialEq, Debug)]
pub struct Prototype {
  pub name: String,
  // pub args: (node::AnyKindNode<'a>, String),
  pub is_variadic: bool,
  pub return_kind: node::AnyKindNode,
}

impl node::Node for Prototype {
  fn accept(&mut self, pass: &dyn pass::Pass) {
    // pass.visit_prototype(self);
  }
}
