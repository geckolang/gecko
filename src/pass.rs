use crate::block;
use crate::function;
use crate::int_kind;
use crate::node;
use crate::prototype;
use crate::void_kind;

pub trait Pass<'a> {
  fn visit(&mut self, node: &'a dyn node::Node) {
    // TODO:
    // node.accept(self);
    self.visit_children(node);
  }

  fn visit_children(&mut self, node: &'a dyn node::Node) {
    for child in node.get_children() {
      self.visit(child);
    }
  }

  fn visit_block(&mut self, _block: &block::Block) {
    //
  }

  fn visit_function(&mut self, _function: &function::Function) {
    //
  }

  fn visit_prototype(&mut self, _prototype: &prototype::Prototype) {
    //
  }

  fn visit_int_kind(&mut self, _int_kind: &'a int_kind::IntKind) {
    //
  }

  fn visit_void_kind(&mut self, _void_kind: &'a void_kind::VoidKind) {
    //
  }
}
