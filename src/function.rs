use crate::node;
use crate::pass;

#[derive(Hash, Eq, PartialEq, Debug)]
pub struct Function {
  //
}

impl node::Node for Function {
  fn accept(&mut self, pass: &dyn pass::Pass) {
    // TODO:
    // pass.visit_function(self);
  }
}
