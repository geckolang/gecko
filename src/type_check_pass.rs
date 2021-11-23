use crate::{diagnostic, node, pass};

pub struct TypeCheckPass;

impl<'a> TypeCheckPass {
  fn find_blocks_of(statement: &'a node::AnyStmtNode<'a>) -> Vec<&'a node::Block<'a>> {
    let mut blocks = Vec::new();

    match &statement {
      // TODO: Awaiting addition of more statements such as `while` and `for`.
      node::AnyStmtNode::IfStmt(ref if_stmt) => {
        blocks.push(&if_stmt.then_block);

        if let Some(else_block) = &if_stmt.else_block {
          blocks.push(&else_block);
        }
      }
      _ => {}
    };

    blocks
  }
}

impl<'a> pass::Pass<'a> for TypeCheckPass {
  fn visit(&mut self, node: &'a dyn node::Node) -> pass::PassResult {
    node.accept(self)?;

    self.visit_tree_of(node)
  }

  fn visit_function<'x>(&mut self, function: &node::Function<'x>) -> pass::PassResult {
    let mut block_queue = vec![&function.body];
    let should_return_value = function.prototype.return_kind_group.is_some();
    let mut is_value_returned = false;

    while let Some(block) = block_queue.pop() {
      for statement in &block.statements {
        match statement {
          node::AnyStmtNode::ReturnStmt(return_stmt) => {
            if !should_return_value && return_stmt.value.is_some() {
              return Err(diagnostic::Diagnostic {
                message: format!(
                  "function `{}` should return `void` but returns a value",
                  function.prototype.name
                ),
                severity: diagnostic::Severity::Error,
              });
            } else if return_stmt.value.is_some() {
              is_value_returned = true;

              // TODO: Compare the types of the returned value and the function's return type.
              // let return_value = return_stmt.value.unwrap();

              // if return_value.kind != function.prototype.return_kind_group.kind {
              //   return Err(diagnostic::Diagnostic {
              //     message: format!(
              //       "function `{}` should return `{}` but returns `{}`",
              //       function.prototype.name,
              //       function.prototype.return_kind_group.kind,
              //       return_value
              //     ),
              //     severity: diagnostic::Severity::Error,
              //   });
              // }
            }
          }
          _ => {}
        };

        block_queue.append(&mut Self::find_blocks_of(statement));
      }
    }

    if should_return_value && !is_value_returned {
      return Err(diagnostic::Diagnostic {
        message: format!(
          "function `{}` should return a value but does not",
          function.prototype.name
        ),
        severity: diagnostic::Severity::Error,
      });
    }

    Ok(())
  }

  fn visit_let_stmt(&mut self, let_stmt: &'a node::LetStmt<'a>) -> pass::PassResult {
    match let_stmt.kind_group.kind {
      node::KindHolder::IntKind(_) => {
        if let node::ExprHolder::IntLiteral(_) = &let_stmt.value {
          Ok(())
        } else {
          Err(diagnostic::Diagnostic {
            message: format!(
              // TODO: Displaying object in error message.
              "expected value of `{}` to be an integer, but got `{:?}`",
              let_stmt.name, let_stmt.value
            ),
            severity: diagnostic::Severity::Error,
          })
        }
      }
      node::KindHolder::BoolKind(_) => {
        if let node::ExprHolder::BoolLiteral(_) = &let_stmt.value {
          Ok(())
        } else {
          Err(diagnostic::Diagnostic {
            message: format!(
              // TODO: Displaying object in error message.
              "expected value of `{}` to be a boolean, but got `{:?}`",
              let_stmt.name, let_stmt.value
            ),
            severity: diagnostic::Severity::Error,
          })
        }
      }
    }
  }
}

#[cfg(test)]
mod tests {
  use super::*;
  use crate::{int_kind, pass::Pass};

  fn make_dummy_function<'a>() -> node::Function<'a> {
    node::Function {
      is_public: false,
      prototype: node::Prototype {
        name: "test".to_string(),
        parameters: vec![],
        is_variadic: false,
        return_kind_group: None,
      },
      body: node::Block {
        llvm_name: "entry".to_string(),
        statements: vec![],
      },
    }
  }

  #[test]
  fn visit_function() {
    let mut pass = TypeCheckPass;
    let function = make_dummy_function();

    assert_eq!(true, pass.visit_function(&function).is_ok());
  }

  #[test]
  fn visit_function_empty_return_stmt() {
    let mut pass = TypeCheckPass;
    let mut function = make_dummy_function();

    function
      .body
      .statements
      .push(node::AnyStmtNode::ReturnStmt(node::ReturnStmt {
        value: None,
      }));

    assert_eq!(true, pass.visit_function(&function).is_ok());
  }

  #[test]
  fn visit_function_void_return_with_value() {
    let mut pass = TypeCheckPass;
    let mut function = make_dummy_function();

    function
      .body
      .statements
      .push(node::AnyStmtNode::ReturnStmt(node::ReturnStmt {
        value: Some(node::ExprHolder::BoolLiteral(node::BoolLiteral {
          value: true,
        })),
      }));

    assert_eq!(false, pass.visit_function(&function).is_ok());

    // TODO: Need a way to identify diagnostics (code field?).
  }

  #[test]
  fn function_function_with_return_no_value() {
    let mut pass = TypeCheckPass;
    let mut function = make_dummy_function();

    function.prototype.return_kind_group = Some(node::KindGroup {
      kind: node::KindHolder::BoolKind(int_kind::BoolKind),
      is_mutable: false,
      is_reference: false,
    });

    assert_eq!(false, pass.visit_function(&function).is_ok());

    // TODO: Need a way to identify diagnostics (code field?).
  }
}
