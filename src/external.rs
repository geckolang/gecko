use crate::{node, pass, prototype};

#[derive(Hash, Eq, PartialEq, Debug)]
pub struct External {
  pub prototype: prototype::Prototype,
}

impl node::Node for External {
  fn accept(&mut self, pass: &dyn pass::Pass) {
    // TODO:
    // pass.visit_function(self);
  }
}
