use crate::{diagnostic, node, pass};

pub struct NameResolutionPass<'a> {
  module_buffer: Option<&'a node::Module<'a>>,
}

impl<'a> NameResolutionPass<'a> {
  pub fn new() -> Self {
    Self {
      module_buffer: None,
    }
  }
}

impl<'a> pass::Pass<'a> for NameResolutionPass<'a> {
  fn visit(&mut self, node: &'a dyn node::Node) -> pass::PassResult {
    node.accept(self)
  }

  fn visit_module(&mut self, module: &'a node::Module<'a>) -> pass::PassResult {
    self.module_buffer = Some(module);

    Ok(())
  }

  fn visit_stub(&mut self, stub: &'a node::Stub<'a>) -> pass::PassResult {
    // FIXME:
    match stub {
      node::Stub::Callable { name, value } => {
        if value.is_some() {
          return Ok(());
        }

        crate::pass_assert!(self.module_buffer.is_some());

        if let Some(_) = self.module_buffer.unwrap().symbol_table.get(name) {
          // FIXME:
          // *value = Some(match target {
          //   node::TopLevelNodeHolder::Function(function) => {
          //     node::StubValueTransport::Function(function)
          //   }
          //   node::TopLevelNodeHolder::External(external) => {
          //     node::StubValueTransport::External(external)
          //   }
          // });
        } else {
          return Err(diagnostic::Diagnostic {
            message: format!("unresolved callee `{}`", name),
            severity: diagnostic::Severity::Error,
          });
        }
      }
    };

    Ok(())
  }
}

#[cfg(test)]
mod tests {
  use super::*;
  use crate::{pass::Pass, void_kind};

  #[test]
  fn visit_module() {
    let mut name_resolution_pass = NameResolutionPass::new();
    let module = node::Module::new("test");

    assert_eq!(true, name_resolution_pass.visit_module(&module).is_ok());
    assert_eq!(true, name_resolution_pass.module_buffer.is_some());
  }

  #[test]
  fn visit_stub() {
    let mut name_resolution_pass = NameResolutionPass::new();
    let mut module = node::Module::new("test");
    const DUMMY_FUNCTION_NAME: &str = "foo";

    let dummy_function = node::Function {
      is_public: false,
      prototype: node::Prototype {
        name: DUMMY_FUNCTION_NAME.into(),
        return_kind_group: node::KindGroup {
          kind: node::KindHolder::VoidKind(void_kind::VoidKind),
          is_reference: false,
          is_mutable: false,
        },
        parameters: vec![],
        is_variadic: false,
      },
      body: node::Block { statements: vec![] },
    };

    module.symbol_table.insert(
      DUMMY_FUNCTION_NAME.into(),
      node::TopLevelNodeHolder::Function(dummy_function),
    );

    assert_eq!(true, name_resolution_pass.visit_module(&module).is_ok());

    let mut stub = node::Stub::Callable {
      name: DUMMY_FUNCTION_NAME.into(),
      value: None,
    };

    assert_eq!(true, name_resolution_pass.visit_stub(&mut stub).is_ok());

    assert_eq!(
      true,
      match stub {
        node::Stub::Callable { name: _, value } => value.is_some(),
      }
    );
  }
}
