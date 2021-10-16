use crate::node;
use crate::pass;

#[derive(Hash, Eq, PartialEq, Debug)]
pub struct Block {
  //
}

impl node::Node for Block {
  fn accept(&mut self, pass: &dyn pass::Pass) {
    // TODO:
    // pass.visit_block(self);
  }
}
