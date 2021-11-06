use crate::{diagnostic, int_kind, node, pass};

pub struct EntryPointCheckPass {}

impl pass::Pass for EntryPointCheckPass {
  fn visit_function(&mut self, function: &node::Function) -> pass::PassResult {
    if function.prototype.name != "main" || !function.is_public {
      return Ok(());
    }

    match function.prototype.return_kind_group.kind {
      node::AnyKindNode::IntKind(int_kind) => {
        if int_kind.size != int_kind::IntSize::Bit32 {
          return Err(diagnostic::Diagnostic {
            message: format!(
              "main function must return `i32`, but found integer size `{:?}`",
              int_kind.size
            ),
            severity: diagnostic::DiagnosticSeverity::Error,
          });
        }
      }
      _ => {
        return Err(diagnostic::Diagnostic {
          message: format!(
            "main function must return `i32`, but returns `{:?}`",
            function.prototype.return_kind_group.kind
          ),
          severity: diagnostic::DiagnosticSeverity::Error,
        });
      }
    };

    if function.prototype.parameters.len() != 2 {
      return Err(diagnostic::Diagnostic {
        message: format!(
          "main function must have 2 parameters, but found `{}`",
          function.prototype.parameters.len()
        ),
        severity: diagnostic::DiagnosticSeverity::Error,
      });
    } else if function.prototype.parameters[0].1.kind
      != node::AnyKindNode::IntKind(int_kind::IntKind {
        size: int_kind::IntSize::Bit32,
        signed: true,
      })
    {
      return Err(diagnostic::Diagnostic {
        message: format!(
          "main function's first parameter must be `i32`, but found `{:?}`",
          function.prototype.parameters[0].1.kind
        ),
        severity: diagnostic::DiagnosticSeverity::Error,
      });
    } else if function.prototype.parameters[1].1.kind
      != node::AnyKindNode::IntKind(int_kind::IntKind {
        size: int_kind::IntSize::Bit32,
        signed: true,
      })
    {
      return Err(diagnostic::Diagnostic {
        message: format!(
          // TODO: Should be an array of i32 instead of i32.
          "main function's second parameter must be `i32`, but found `{:?}`",
          function.prototype.parameters[1].1.kind
        ),
        severity: diagnostic::DiagnosticSeverity::Error,
      });
    }

    Ok(())
  }
}
