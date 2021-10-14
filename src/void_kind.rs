use crate::node;
use crate::pass;

#[derive(Hash, Eq, PartialEq, Debug)]
pub struct VoidKind {
  //
}

impl node::Node for VoidKind {
  fn accept(&self, pass: &dyn pass::Pass) {
    // pass.visit_void_kind(self);
  }
}
