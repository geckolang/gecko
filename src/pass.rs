use crate::node;
use crate::block;
use crate::function;
use crate::prototype;
use crate::int_kind;
use crate::void_kind;

pub trait Pass {
  fn visit(&self, node: &dyn node::Node) {
    // TODO:
    // node.accept(self);
    self.visit_children(node);
  }

  fn visit_children(&self, node: &dyn node::Node) {
    for child in node.get_children() {
      self.visit(child);
    }
  }

  fn visit_block(&self, _block: &block::Block) {
    //
  }

  fn visit_function(&self, _function: &function::Function) {
    //
  }

  fn visit_prototype(&self, _prototype: &prototype::Prototype) {
    //
  }

  fn visit_int_kind(&self, _int_kind: &int_kind::IntKind) {
    //
  }

  fn visit_void_kind(&self, _void_kind: &void_kind::VoidKind) {
    //
  }
}
