use crate::node;
use crate::pass;

#[derive(Hash, Eq, PartialEq, Debug)]
pub struct Block {
  //
}

impl node::Node for Block {
  fn accept(&self, pass: &dyn pass::Pass) {
    // pass.visit_block(self);
  }
}
