use crate::node;

#[derive(Hash, Eq, PartialEq, Debug, Copy, Clone)]
pub struct VoidKind;

impl<'a> node::Node<'a> for VoidKind {
  //
}
