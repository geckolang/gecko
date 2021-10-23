use crate::node;
use crate::pass;

#[derive(Hash, Eq, PartialEq, Debug, Copy, Clone)]
pub enum IntSize {
  Signed8,
  Signed16,
  Signed32,
  Signed64,
  Signed128,
}

#[derive(Hash, Eq, PartialEq, Debug, Copy, Clone)]
pub struct IntKind {
  pub size: IntSize,
}

impl node::Node for IntKind {
  fn accept(&mut self, pass: &mut dyn pass::Pass) -> pass::PassResult {
    pass.visit_int_kind(self)?;

    Ok(())
  }
}
