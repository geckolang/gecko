use crate::node;
use crate::pass;

#[derive(Hash, Eq, PartialEq, Debug)]
pub struct Prototype<'a> {
  pub name: String,
  pub args: (node::AnyKindNode<'a>, String),
  pub is_variadic: bool,
}

impl<'a> node::Node for Prototype<'a> {
  fn accept(&mut self, pass: &dyn pass::Pass) {
    // pass.visit_prototype(self);
  }
}
