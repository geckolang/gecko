use crate::{function, node, pass};

pub struct EntryPointCheckPass {}

impl pass::Pass for EntryPointCheckPass {
  fn visit_function(function: &function::Function) {
    if function.prototype.name != "main" {
      return Ok(());
    }

    match function.prototype.return_kind_group.kind {
      node::AnyKindNode::IntKind(int_kind) => {
        if int_kind.size != int_kind::IntSize::Bit32 {
          return Err(format!(
            "main function must return `i32`, but found integer size `{}`",
            int_kind.size
          ));
        }
      }
      _ => {
        return Err(format!(
          "main function must return `i32`, but returns `{:?}`",
          function.prototype.return_kind_group.kind
        ));
      }
    };

    if function.prototype.parameters.len() != 2 {
      return Err(format!(
        "main function must have 2 parameters, but found `{}`",
        function.prototype.parameters.len()
      ));
    } else if function.prototype.parameters[0].kind
      != node::AnyKindNode::IntKind(int_kind::IntKind {
        size: int_kind::IntSize::Bit32,
        signed: true,
      })
    {
      return Err(format!(
        "main function's first parameter must be `i32`, but found `{:?}`",
        function.prototype.parameters[0].kind
      ));
    } else if function.prototype.parameters[1].kind
      != node::AnyKindNode::IntKind(int_kind::IntKind {
        size: int_kind::IntSize::Bit32,
        signed: true,
      })
    {
      return Err(format!(
        // TODO: Should be an array of i32 instead of i32.
        "main function's second parameter must be `i32`, but found `{:?}`",
        function.prototype.parameters[1].kind
      ));
    }

    Ok(())
  }
}
