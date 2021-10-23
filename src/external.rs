use crate::{node, pass, prototype};

#[derive(Hash, Eq, PartialEq, Debug)]
pub struct External {
  pub prototype: prototype::Prototype,
}

impl node::Node for External {
  fn accept(&mut self, pass: &mut dyn pass::Pass) -> pass::PassResult {
    pass.visit_external(self)?;

    Ok(())
  }
}
