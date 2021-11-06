use crate::node;
use crate::pass;

#[derive(Hash, Eq, PartialEq, Debug, Copy, Clone)]
pub enum IntSize {
  Bit8,
  Bit16,
  Bit32,
  Bit64,
  Bit128,
}

#[derive(Hash, Eq, PartialEq, Debug, Copy, Clone)]
pub struct IntKind {
  pub size: IntSize,
  pub signed: bool,
}

impl node::Node for IntKind {
  fn accept(&mut self, pass: &mut dyn pass::Pass) -> pass::PassResult {
    pass.visit_int_kind(self)?;

    Ok(())
  }
}

#[derive(Hash, Eq, PartialEq, Debug, Copy, Clone)]
pub struct BoolKind;

impl node::Node for BoolKind {
  fn accept(&mut self, pass: &mut dyn pass::Pass) -> pass::PassResult {
    pass.visit_bool_kind(self)?;

    Ok(())
  }
}
