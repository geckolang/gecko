use crate::node;
use crate::pass;

#[derive(Hash, Eq, PartialEq, Debug)]
pub struct Function {
  //
}

impl node::Node for Function {
  fn accept(&self, pass: &dyn pass::Pass) {
    pass.visit_function(self);
  }
}
