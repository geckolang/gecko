use crate::{diagnostic, node, pass};

pub struct NameResolutionPass {
  module_buffer: Option<node::Module>,
}

impl<'a> pass::Pass<'a> for NameResolutionPass {
  // TODO: Generated by Github code-pilot.
  // fn visit_stub(stub: &node::Stub) {
  //   let mut scope = stub.scope();
  //   let mut name = stub.name();
  //   let mut found = false;
  //   while let Some(parent) = scope.parent() {
  //     if let Some(decl) = scope.find_decl(name) {
  //       found = true;
  //       stub.set_decl(decl);
  //       break;
  //     }
  //     scope = parent;
  //   }
  //   if !found {
  //     let mut diag =
  //       diagnostic::Diagnostic::error(&format!("unresolved name `{}`", name), stub.span());
  //     diag.emit();
  //   }
  // }

  fn visit_module(&mut self, module: node::Module) -> pass::PassResult {
    // FIXME:
    // self.module_buffer = Some(module);

    Ok(())
  }

  fn visit_stub(&mut self, stub: &mut node::Stub) -> pass::PassResult {
    match stub {
      node::Stub::Callable { name, value } => {
        if value.is_some() {
          return Ok(());
        }

        crate::assert!(self.module_buffer.is_some());

        if !&self
          .module_buffer
          .as_ref()
          .unwrap()
          .symbol_table
          .contains_key(name)
        {
          return Err(diagnostic::Diagnostic {
            message: format!("unresolved callee `{}`", name),
            severity: diagnostic::DiagnosticSeverity::Error,
          });
        }

        // TODO: Resolve stubs here.
      }
    };

    Ok(())
  }
}

#[cfg(test)]
mod tests {
  use super::*;
  use crate::pass::Pass;
}
