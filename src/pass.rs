use crate::{
  block, diagnostic, external, function, int_kind, namespace, node, pass_manager, prototype,
  void_kind,
};

pub struct PassContext {
  diagnostics: Vec<diagnostic::Diagnostic>,
}

pub type PassResult = Result<(), diagnostic::Diagnostic>;

pub trait Pass<'a> {
  // TODO:
  fn register(&self, _: &pass_manager::PassManager) -> bool {
    return true;
  }

  fn get_diagnostics(&self) -> Vec<diagnostic::Diagnostic> {
    vec![]
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

  fn visit_namespace(&mut self, _: &namespace::Namespace) -> PassResult {
    Ok(())
  }

  fn visit_external(&mut self, _: &external::External) -> PassResult {
    Ok(())
  }
}
