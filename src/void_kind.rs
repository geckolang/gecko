use crate::node;
use crate::pass;

#[derive(Hash, Eq, PartialEq, Debug)]
pub struct VoidKind {
  //
}

impl node::Node for VoidKind {
  fn accept(&mut self, pass: &dyn pass::Pass) {
    // TODO:
    // pass.visit_void_kind(self);
  }
}
