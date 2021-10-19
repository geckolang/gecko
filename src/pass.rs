use crate::{block, diagnostic, function, int_kind, node, prototype, void_kind};

pub struct PassContext {
  diagnostics: Vec<diagnostic::Diagnostic>,
}

pub trait Pass {
  fn register(&self) -> bool {
    return true;
  }

  fn visit(&mut self, node: &dyn node::Node) {
    // TODO:
    // node.accept(self);
    self.visit_children(node);
  }

  fn visit_children(&mut self, node: &dyn node::Node) {
    for child in node.get_children() {
      self.visit(child);
    }
  }

  fn visit_block(&mut self, _: &block::Block) {
    //
  }

  fn visit_function(&mut self, _: &function::Function) {
    //
  }

  fn visit_prototype(&mut self, _: &prototype::Prototype) {
    //
  }

  fn visit_int_kind(&mut self, _: &int_kind::IntKind) {
    //
  }

  fn visit_void_kind(&mut self, _: &void_kind::VoidKind) {
    //
  }
}
