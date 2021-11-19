use crate::{node, pass};

#[derive(Hash, Eq, PartialEq, Debug, Copy, Clone)]
pub struct VoidKind;

impl node::Node for VoidKind {
  fn accept<'b>(&'b self, pass: &mut dyn pass::Pass<'b>) -> pass::PassResult {
    pass.visit_void_kind(self)
  }
}
