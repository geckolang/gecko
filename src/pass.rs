use crate::{diagnostic, int_kind, node, pass_manager, void_kind};

pub type PassResult = Result<(), diagnostic::Diagnostic>;

pub trait Pass {
  // TODO:
  fn register(&self, _: &pass_manager::PassManager) -> bool {
    return true;
  }

  fn get_diagnostics(&self) -> Vec<diagnostic::Diagnostic> {
    vec![]
  }

  fn visit(&mut self, node: &dyn node::Node) -> PassResult {
    // TODO:
    // node.accept(&self);
    self.visit_children(node)?;

    Ok(())
  }

  fn visit_children(&mut self, node: &dyn node::Node) -> PassResult {
    for child in node.get_children() {
      self.visit(child)?;
    }

    Ok(())
  }

  fn visit_block(&mut self, _: &node::Block) -> PassResult {
    Ok(())
  }

  fn visit_function(&mut self, _: &node::Function) -> PassResult {
    Ok(())
  }

  fn visit_prototype(&mut self, _: &node::Prototype) -> PassResult {
    Ok(())
  }

  fn visit_int_kind(&mut self, _: &int_kind::IntKind) -> PassResult {
    Ok(())
  }

  fn visit_void_kind(&mut self, _: &void_kind::VoidKind) -> PassResult {
    Ok(())
  }

  fn visit_bool_kind(&mut self, _: &int_kind::BoolKind) -> PassResult {
    Ok(())
  }

  fn visit_package(&mut self, _: &node::Package) -> PassResult {
    Ok(())
  }

  fn visit_external(&mut self, _: &node::External) -> PassResult {
    Ok(())
  }

  fn visit_return_stmt(&mut self, _: &node::ReturnStmt) -> PassResult {
    Ok(())
  }

  fn visit_bool_literal(&mut self, _: &node::BoolLiteral) -> PassResult {
    Ok(())
  }
}
