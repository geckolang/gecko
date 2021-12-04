use crate::{node, pass};

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

impl<'a> pass::TransformPass<'a> for NameResolutionPass<'a> {
  fn visit(&mut self, node: &'a mut dyn node::Node) -> pass::PassResult {
    node.accept_transform_pass(self)?;

    self.visit_children(node)
  }

  fn visit_module(&mut self, module: &'a mut node::Module<'a>) -> pass::PassResult {
    self.module_buffer = Some(module);

    Ok(())
  }
}

// TODO
// fn visit_stub(&mut self, stub: &'a node::Stub) -> pass::PassResult {
//   match stub {
//     node::Stub::Callable(name) => {
//       crate::pass_assert!(self.module_buffer.is_some());

//       if !self.module_buffer.unwrap().symbol_table.contains_key(name) {
//         return Err(diagnostic::Diagnostic {
//           message: format!("unresolved callee `{}`", name),
//           severity: diagnostic::Severity::Error,
//         });
//       }
//     }
//   };

//   Ok(())
// }

#[cfg(test)]
mod tests {
  use super::*;
  use crate::pass::TransformPass;

  #[test]
  fn visit_module() {
    let mut name_resolution_pass = NameResolutionPass::new();
    let mut module = node::Module::new("test");

    assert_eq!(true, name_resolution_pass.visit_module(&mut module).is_ok());
    assert_eq!(true, name_resolution_pass.module_buffer.is_some());
  }

  // TODO:
  // #[test]
  // fn visit_stub() {
  //   let mut name_resolution_pass = NameResolutionPass::new();
  //   let mut module = node::Module::new("test");
  //   const DUMMY_FUNCTION_NAME: &str = "foo";

  //   let dummy_function = node::Function {
  //     is_public: false,
  //     prototype: node::Prototype {
  //       name: DUMMY_FUNCTION_NAME.to_string(),
  //       return_kind_group: node::KindGroup {
  //         kind: node::KindHolder::VoidKind(void_kind::VoidKind),
  //         is_reference: false,
  //         is_mutable: false,
  //       },
  //       parameters: vec![],
  //       is_variadic: false,
  //     },
  //     body: node::Block { statements: vec![] },
  //   };

  //   module.symbol_table.insert(
  //     DUMMY_FUNCTION_NAME.to_string(),
  //     node::TopLevelNodeHolder::Function(dummy_function),
  //   );

  //   assert_eq!(true, name_resolution_pass.visit_module(&module).is_ok());

  //   let mut stub = node::Stub::Callable {
  //     name: DUMMY_FUNCTION_NAME.to_string(),
  //     value: None,
  //   };

  //   assert_eq!(true, name_resolution_pass.visit_stub(&mut stub).is_ok());

  //   assert_eq!(
  //     true,
  //     match stub {
  //       node::Stub::Callable { name: _, value } => value.is_some(),
  //     }
  //   );
  // }
}
