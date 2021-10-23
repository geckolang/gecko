use crate::{int_kind, pass, void_kind};

#[derive(Hash, Eq, PartialEq, Debug, Copy, Clone)]
pub enum AnyKindNode {
  IntKind(int_kind::IntKind),
  VoidKind(void_kind::VoidKind),
}

pub trait Node {
  fn accept(&mut self, pass: &mut dyn pass::Pass) -> pass::PassResult;

  fn get_children(&self) -> Vec<&dyn Node> {
    vec![]
  }
}

#[derive(Hash, Eq, PartialEq, Debug)]
pub struct Identifier {
  pub name: String,
}
