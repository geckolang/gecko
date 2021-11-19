use crate::{diagnostic, int_kind, node, pass_manager, void_kind};

#[macro_export]
macro_rules! pass_assert {
  ($condition:expr) => {
    match $condition {
      true => true,
      false => {
        return Err(diagnostic::Diagnostic {
          message: format!("assertion failed: `{}`", stringify!($condition)),
          severity: diagnostic::Severity::Internal,
        });
      }
    }
  };
}

pub struct PassRequirements {
  pub ignore_previous_errors: bool,
}

impl PassRequirements {
  pub fn default() -> Self {
    Self {
      ignore_previous_errors: false,
    }
  }
}

pub type PassResult = Result<(), diagnostic::Diagnostic>;

pub trait Pass<'a> {
  // TODO:
  fn register(&self, _: &pass_manager::PassManager<'a>) -> bool {
    return true;
  }

  fn get_requirements(&self) -> PassRequirements {
    PassRequirements::default()
  }

  fn get_diagnostics(&self) -> Vec<diagnostic::Diagnostic> {
    vec![]
  }

  fn visit(&mut self, node: &'a dyn node::Node) -> PassResult;

  fn visit_children(&mut self, node: &'a dyn node::Node) -> PassResult {
    for child in node.get_children() {
      self.visit(child)?;
    }

    Ok(())
  }

  fn visit_stub(&mut self, _: &'a node::Stub<'a>) -> PassResult {
    Ok(())
  }

  fn visit_block(&mut self, _: &'a node::Block<'a>) -> PassResult {
    Ok(())
  }

  fn visit_function(&mut self, _: &'a node::Function<'a>) -> PassResult {
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

  fn visit_module(&mut self, _: &'a node::Module<'a>) -> PassResult {
    Ok(())
  }

  fn visit_external(&mut self, _: &'a node::External) -> PassResult {
    Ok(())
  }

  fn visit_return_stmt(&mut self, _: &'a node::ReturnStmt<'a>) -> PassResult {
    Ok(())
  }

  fn visit_bool_literal(&mut self, _: &'a node::BoolLiteral) -> PassResult {
    Ok(())
  }

  fn visit_int_literal(&mut self, _: &'a node::IntLiteral) -> PassResult {
    Ok(())
  }

  fn visit_call_expr(&mut self, _: &'a node::CallExpr<'a>) -> PassResult {
    Ok(())
  }

  fn visit_let_stmt(&mut self, _: &'a node::LetStmt<'a>) -> PassResult {
    Ok(())
  }

  fn visit_if_stmt(&mut self, _: &'a node::IfStmt<'a>) -> PassResult {
    Ok(())
  }
}
