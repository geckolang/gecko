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

impl TypeCheck for ast::FunctionCall {
  fn type_check<'ctx>(&self, context: &mut context::Context) -> Vec<diagnostic::Diagnostic> {
    // TODO: Need access to the current function.
    // TODO: Ensure externs and unsafe function are only called from unsafe functions.

    let callee = context
      .declarations
      .get(self.callee_definition_key.as_ref().unwrap())
      .unwrap();

    match *callee.as_ref().borrow() {
      ast::Node::Extern(_) => todo!(),
      _ => todo!(),
    };

    vec![]
  }
}
