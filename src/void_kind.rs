use crate::node;
use crate::pass;

#[derive(Hash, Eq, PartialEq, Debug, Copy, Clone)]
pub struct VoidKind;

impl<'a> node::Node<'a> for VoidKind {
  fn accept(&'a mut self, pass: &'a mut dyn pass::Pass<'a>) -> pass::PassResult {
    pass.visit_void_kind(self)
  }
}
