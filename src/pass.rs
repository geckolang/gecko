use crate::{block, diagnostic, function, int_kind, node, prototype, void_kind};

pub struct PassContext {
  diagnostics: Vec<diagnostic::Diagnostic>,
}

pub type PassResult = Result<(), diagnostic::Diagnostic>;

pub trait Pass {
  // TODO:
  fn register(&self) -> bool {
    return true;
  }

  fn visit(&mut self, node: &dyn node::Node) -> PassResult {
    // TODO:
    // node.accept(self);
    self.visit_children(node)?;

    Ok(())
  }

  fn visit_children(&mut self, node: &dyn node::Node) -> PassResult {
    for child in node.get_children() {
      self.visit(child)?;
    }

    Ok(())
  }

  fn visit_block(&mut self, _: &block::Block) -> PassResult {
    Ok(())
  }

  fn visit_function(&mut self, _: &function::Function) -> PassResult {
    Ok(())
  }

  fn visit_prototype(&mut self, _: &prototype::Prototype) -> PassResult {
    Ok(())
  }

  fn visit_int_kind(&mut self, _: &int_kind::IntKind) -> PassResult {
    Ok(())
  }

  fn visit_void_kind(&mut self, _: &void_kind::VoidKind) -> PassResult {
    Ok(())
  }
}
