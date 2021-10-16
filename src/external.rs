use crate::{node, pass, prototype};

#[derive(Hash, Eq, PartialEq, Debug)]
pub struct External<'a> {
  prototype: prototype::Prototype<'a>,
}

impl<'a> node::Node for External<'a> {
  fn accept(&mut self, pass: &dyn pass::Pass) {
    // TODO:
    // pass.visit_function(self);
  }
}
