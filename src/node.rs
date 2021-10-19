use crate::{int_kind, pass, void_kind};

#[derive(Hash, Eq, PartialEq, Debug)]
pub enum AnyKindNode {
  IntKind(int_kind::IntKind),
  VoidKind(void_kind::VoidKind),
}

pub trait Node {
  fn accept(&mut self, ps: &dyn pass::Pass);

  fn get_children(&self) -> Vec<&dyn Node> {
    vec![]
  }
}

#[derive(Hash, Eq, PartialEq, Debug)]
pub struct Identifier {
  name: String,
}
