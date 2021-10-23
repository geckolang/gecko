use crate::{block, node, pass, prototype};

#[derive(Hash, Eq, PartialEq, Debug)]
pub struct Function {
  pub is_public: bool,
  pub prototype: prototype::Prototype,
  pub body: block::Block,
}

impl node::Node for Function {
  fn accept(&mut self, pass: &mut dyn pass::Pass) -> pass::PassResult {
    pass.visit_function(self)?;

    Ok(())
  }
}
