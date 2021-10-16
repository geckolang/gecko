use crate::node;
use crate::pass;

#[derive(Hash, Eq, PartialEq, Debug)]
pub struct Prototype {
  pub name: String,
  pub is_variadic: bool,
}

impl node::Node for Prototype {
  fn accept(&mut self, pass: &dyn pass::Pass) {
    // pass.visit_prototype(self);
  }
}
