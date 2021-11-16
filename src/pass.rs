use crate::{diagnostic, int_kind, node, pass_manager, void_kind};

#[macro_export]
macro_rules! pass_assert {
  ($condition:expr) => {
    match $condition {
      true => true,
      false => {
        return Err(diagnostic::Diagnostic {
          message: format!("assertion failed: `{}`", stringify!($condition)),
          severity: diagnostic::DiagnosticSeverity::Internal,
        });
      }
    }
  };
}

macro_rules! pass_add_diagnostic {
  ($diagnostics_vector:expr, $diagnostic:expr) => {
    $diagnostics_vector.push($diagnostic);
  };
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
    // TODO: Address error.
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

  fn visit_stub(&mut self, _: &mut node::Stub<'a>) -> PassResult {
    Ok(())
  }

  fn visit_block(&mut self, _: &'a node::Block) -> PassResult {
    Ok(())
  }

  fn visit_function(&mut self, _: &'a node::Function) -> PassResult {
    Ok(())
  }

  fn visit_prototype(&mut self, _: &'a node::Prototype) -> PassResult {
    Ok(())
  }

  fn visit_int_kind(&mut self, _: &'a int_kind::IntKind) -> PassResult {
    Ok(())
  }

  fn visit_void_kind(&mut self, _: &'a void_kind::VoidKind) -> PassResult {
    Ok(())
  }

  fn visit_bool_kind(&mut self, _: &'a int_kind::BoolKind) -> PassResult {
    Ok(())
  }

  fn visit_module(&mut self, _: &'a node::Module) -> PassResult {
    Ok(())
  }

  fn visit_external(&mut self, _: &'a node::External) -> PassResult {
    Ok(())
  }

  fn visit_return_stmt(&mut self, _: &'a node::ReturnStmt) -> PassResult {
    Ok(())
  }

  fn visit_bool_literal(&mut self, _: &'a node::BoolLiteral) -> PassResult {
    Ok(())
  }

  fn visit_int_literal(&mut self, _: &'a node::IntLiteral) -> PassResult {
    Ok(())
  }

  fn visit_call_expr(&mut self, _: &'a node::CallExpr) -> PassResult {
    Ok(())
  }

  fn visit_let_stmt(&mut self, _: &'a node::LetStmt) -> PassResult {
    Ok(())
  }
}
