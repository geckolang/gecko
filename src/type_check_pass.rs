use crate::{diagnostic, node, pass};

pub struct TypeCheckPass {}

impl TypeCheckPass {
  fn find_blocks_of(statement: &node::AnyStatementNode) -> Vec<&node::Block> {
    let mut blocks = vec![];

    match statement {
      // TODO: Awaiting addition of statements such as `if` and `while`.
      _ => {}
    };

    blocks
  }
}

impl pass::Pass for TypeCheckPass {
  fn visit_function(&mut self, function: &node::Function) -> pass::PassResult {
    let mut block_queue = vec![&function.body];

    let should_return_value = match function.prototype.return_kind_group.kind {
      node::AnyKindNode::VoidKind(_) => false,
      _ => true,
    };

    let mut is_value_returned = false;

    while let Some(block) = block_queue.pop() {
      for statement in &block.statements {
        match statement {
          node::AnyStatementNode::ReturnStmt(return_stmt) => {
            if !should_return_value && return_stmt.value.is_some() {
              return Err(diagnostic::Diagnostic {
                message: format!(
                  "function `{}` should return `void` but returns a value",
                  function.prototype.name
                ),
                severity: diagnostic::DiagnosticSeverity::Error,
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
              //     severity: diagnostic::DiagnosticSeverity::Error,
              //   });
              // }
            }
          }
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
        severity: diagnostic::DiagnosticSeverity::Error,
      });
    }

    Ok(())
  }
}
