use crate::{int_kind, pass, void_kind};

#[derive(Hash, Eq, PartialEq, Debug)]
pub enum AnyKindNode<'a> {
  IntKind(&'a int_kind::IntKind),
  VoidKind(&'a void_kind::VoidKind),
}

impl AnyKindNode<'_> {
  pub fn into_int_kind(&self) -> Option<&int_kind::IntKind> {
    if let AnyKindNode::IntKind(t) = self {
      return Some(t);
    }

    None
  }

  pub fn into_void_kind(&self) -> Option<&void_kind::VoidKind> {
    if let AnyKindNode::VoidKind(t) = self {
      return Some(t);
    }

    None
  }
}

pub trait Node {
  fn accept(&self, ps: &dyn pass::Pass);

  fn get_children(&self) -> Vec<&dyn Node> {
    vec![]
  }
}
