use crate::{diagnostic, node, pass};

pub struct PassManager<'a> {
  passes: Vec<Box<dyn pass::Pass<'a>>>,
}

impl<'a> PassManager<'a> {
  pub fn new() -> Self {
    Self { passes: Vec::new() }
  }

  /// Register a pass to be run. Returns `true` if the pass'
  /// restrictions are met.
  pub fn add_pass(&mut self, pass: Box<dyn pass::Pass<'a>>) -> bool {
    if !pass.register(self) {
      return false;
    }

    self.passes.push(pass);

    true
  }

  /// Execute all registered passes in a sequential order, over
  /// the provided root node.
  pub fn run(&'a mut self, root_node: &'a mut dyn node::Node<'a>) -> Vec<diagnostic::Diagnostic> {
    // TODO: Better structure/organization of diagnostics.

    let mut diagnostics = vec![];

    for pass in &mut self.passes {
      // root_node.accept(pass);
      let visitation_result = pass.visit(root_node);

      diagnostics.extend(pass.get_diagnostics());

      if let Err(diagnostic) = visitation_result {
        diagnostics.push(diagnostic);
      }
    }

    diagnostics
  }
}

#[cfg(test)]
mod tests {
  use super::*;

  struct TestPassEmpty;

  impl<'a> pass::Pass<'a> for TestPassEmpty {
    //
  }

  struct TestPassNoRegister;

  impl<'a> pass::Pass<'a> for TestPassNoRegister {
    fn register(&self, _: &PassManager<'a>) -> bool {
      return false;
    }
  }

  struct TestNode;

  #[test]
  fn pass_manager_proper_initial_values() {
    assert_eq!(true, PassManager::new().passes.is_empty());
  }

  #[test]
  fn pass_manager_add_pass() {
    let mut pass_manager = PassManager::new();
    let test_pass = TestPassEmpty;

    pass_manager.add_pass(Box::new(test_pass));
    assert_eq!(1, pass_manager.passes.len());
  }

  #[test]
  fn pass_manager_add_pass_no_register() {
    let mut pass_manager = PassManager::new();
    let mut test_pass_no_register = TestPassNoRegister;

    pass_manager.add_pass(Box::new(test_pass_no_register));
    assert_eq!(true, pass_manager.passes.is_empty());
  }
}
