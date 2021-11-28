use crate::{diagnostic, int_kind, node, pass_manager};

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
    true
  }

  /// Retrieve the list of flags and options denoting the pass's
  /// requirements which must be satisfied before the pass is run.
  fn get_requirements(&self) -> PassRequirements {
    PassRequirements::default()
  }

  /// Retrieve the diagnostics generated by the pass. If none, an
  /// empty vector will be returned.
  fn get_diagnostics(&self) -> Vec<diagnostic::Diagnostic> {
    vec![]
  }

  /// Visit a node. Implementation varies depending on the pass.
  ///
  /// The appropriate visitation of the node method may or may not
  /// be invoked by the pass.
  fn visit(&mut self, node: &'a dyn node::Node) -> PassResult;

  /// Visit the node's children by invoking its [`get_children`] method.
  fn visit_children(&mut self, node: &'a dyn node::Node) -> PassResult {
    for child in node.get_children() {
      self.visit(child)?;
    }

    Ok(())
  }

  /// Visit the node's children and all of its subsequent siblings.
  ///
  /// This will walk the tree of children in a depth-first manner.
  fn visit_tree_of(&mut self, node: &'a dyn node::Node) -> PassResult {
    let mut child_queue = node.get_children();

    while let Some(child) = child_queue.pop() {
      self.visit(child)?;
      child_queue.append(&mut child.get_children());
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

  fn visit_while_stmt(&mut self, _: &'a node::WhileStmt<'a>) -> PassResult {
    Ok(())
  }

  fn visit_block_stmt(&mut self, _: &'a node::BlockStmt<'a>) -> PassResult {
    Ok(())
  }
}
