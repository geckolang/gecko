use crate::{node, pass};

#[derive(Hash, Eq, PartialEq, Debug)]
pub struct Namespace {
  // TODO: Fields.
}

impl node::Node for Namespace {
  fn accept(&mut self, pass: &dyn pass::Pass) {
    // pass.visit_prototype(self);
  }
}
