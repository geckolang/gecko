use crate::node;
use crate::pass;

#[derive(Hash, Eq, PartialEq, Debug, Copy, Clone)]
pub struct VoidKind {
  //
}

impl node::Node for VoidKind {
  fn accept(&mut self, pass: &mut dyn pass::Pass) -> pass::PassResult {
    pass.visit_void_kind(self)?;

    Ok(())
  }
}
