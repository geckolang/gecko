use crate::node;
use crate::pass;

#[derive(Hash, Eq, PartialEq, Debug)]
pub enum IntSize {
  Signed8,
  Signed16,
  Signed32,
  Signed64,
  Signed128,
}

#[derive(Hash, Eq, PartialEq, Debug)]
pub struct IntKind {
  pub size: IntSize,
}

impl node::Node for IntKind {
  fn accept(&mut self, pass: &dyn pass::Pass) {
    // TODO:
    // pass.visit_int_kind(self);
  }
}
