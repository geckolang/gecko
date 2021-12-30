use crate::{ast, context, diagnostic};
use std::borrow::Borrow;

pub type TypeCheckResult = Option<Vec<diagnostic::Diagnostic>>;

pub trait TypeCheck {
  fn type_check<'ctx>(&self, context: &mut context::Context) -> Vec<diagnostic::Diagnostic>;
}

impl TypeCheck for ast::Function {
  fn type_check<'ctx>(&self, _context: &mut context::Context) -> Vec<diagnostic::Diagnostic> {
    let mut diagnostics = diagnostic::DiagnosticBuilder::new();
    let mut is_value_returned = false;
    let mut block_queue = vec![&self.body];

    while let Some(next_block) = block_queue.pop() {
      for statement in &next_block.statements {
        // TODO: Implement.
        // block_queue.extend(match statement {
        //   // TODO:
        //   _ => vec![],
        // });

        if let ast::Node::ReturnStmt(return_stmt) = statement.borrow() {
          if return_stmt.value.is_some() {
            is_value_returned = true;

            break;
          }
        }
      }
    }

    match &self.prototype {
      ast::Type::Prototype(_parameters, return_type, _is_variadic) => {
        // Ensure function returns a value if its return type is defined.
        if return_type.is_some() && !is_value_returned {
          diagnostics.error(format!("function `{}` must return a value", self.name));
        }
      }
      _ => unreachable!(),
    };

    diagnostics.into()
  }
}

// pub fn type_check_function<'a>(function: &ast::Function<'a>) -> TypeCheckResult {
//   // FIXME: Need proper implementation of walking the tree for return values.
//   let mut block_queue = vec![&function.body];
//   let mut values_returned = Vec::new();

//   while let Some(block) = block_queue.pop() {
//     if let Some(return_stmt) = block.find_terminator() {
//       if let Some(return_value) = &return_stmt.value {
//         values_returned.push(return_value);
//       }
//     }

//     for statement in &block.statements {
//       // TODO: What about recursive/child statements? Is this handled already?
//       if let Some(child_blocks) = find_blocks_of(&statement) {
//         block_queue.extend(child_blocks);
//       }
//     }
//   }

//   let mut diagnostics = Vec::new();

//   if let Some(return_kind_group) = &function.prototype.return_kind_group {
//     for return_value in values_returned {
//       let return_value_kind = find_kind_of(return_value);

//       // FIXME: Kind group is being ignored.
//       if return_value_kind.is_none() || return_value_kind.unwrap() != return_kind_group.kind {
//         diagnostics.push(diagnostic::Diagnostic {
//           message: format!(
//             // FIXME: Using temporary display value, also dumping objects.
//             "function return value type mismatch; expected `{:?}`, but got `{}`",
//             return_kind_group.kind, "temp"
//           ),
//           severity: diagnostic::Severity::Error,
//         });
//       }
//     }
//   } else if !values_returned.is_empty() {
//     diagnostics.push(diagnostic::Diagnostic {
//       message: format!(
//         "function `{}` may not return a value because its signature does not specify a return type",
//         function.prototype.name
//       ),
//       severity: diagnostic::Severity::Error,
//     });
//   }

//   if diagnostics.is_empty() {
//     None
//   } else {
//     Some(diagnostics)
//   }
// }
