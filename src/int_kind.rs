use crate::node;
use crate::pass;

#[derive(Hash, Eq, PartialEq, Debug)]
pub struct IntKind {
  pub name: String,
}

impl node::Node for IntKind {
  fn accept(&self, pass: &dyn pass::Pass) {
    // pass.visit_int_kind(self);
  }
}
