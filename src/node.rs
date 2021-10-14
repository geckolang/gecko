use crate::{pass, int_kind, void_kind, prototype, function, block};

#[derive(Hash, Eq, PartialEq, Debug)]
pub enum AnyKindNode {
  IntKind(int_kind::IntKind),
  VoidKind(void_kind::VoidKind),
}

impl AnyKindNode {
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

impl From<int_kind::IntKind> for AnyKindNode {
  fn from(int_kind: int_kind::IntKind) -> Self {
    AnyKindNode::IntKind(int_kind)
  }
}

impl From<void_kind::VoidKind> for AnyKindNode {
  fn from(void_kind: void_kind::VoidKind) -> Self {
    AnyKindNode::VoidKind(void_kind)
  }
}

pub trait Node {
  fn accept(&self, ps: &dyn pass::Pass);

  fn get_children(&self, ) -> Vec<&dyn Node> {
    vec![]
  }
}
