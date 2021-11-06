use crate::{diagnostic, node, pass};

pub struct PassManager {
  passes: Vec<Box<dyn pass::Pass>>,
}

impl PassManager {
  pub fn new() -> Self {
    Self { passes: vec![] }
  }

  /// Register a pass to be run. Returns `true` if the pass'
  /// restrictions are met.
  pub fn add_pass(&mut self, pass: Box<dyn pass::Pass>) -> bool {
    if !pass.register(self) {
      return false;
    }

    self.passes.push(pass);

    true
  }

  /// Execute all registered passes in a sequential order, over
  /// the provided root node.
  pub fn run(&mut self, root_node: &dyn node::Node) -> Vec<diagnostic::Diagnostic> {
    // TODO: Better structure/organization of diagnostics.

    let mut diagnostics = vec![];

    for pass in &mut self.passes {
      let visitation_result = pass.visit(root_node);

      for diagnostic in pass.get_diagnostics().iter() {
        diagnostics.push(diagnostic.clone());
      }

      if visitation_result.is_err() {
        diagnostics.push(visitation_result.err().unwrap());
      }
    }

    diagnostics
  }
}

#[cfg(test)]
mod tests {
  use super::*;

  struct TestPassEmpty {
    //
  }

  impl pass::Pass for TestPassEmpty {
    //
  }

  struct TestPassNoRegister {
    //
  }

  impl pass::Pass for TestPassNoRegister {
    fn register(&self, _: &PassManager) -> bool {
      return false;
    }
  }

  struct TestNode {
    //
  }

  impl node::Node for TestNode {
    // TODO: Isn't this redundant?
    fn accept(&mut self, _: &mut dyn pass::Pass) -> pass::PassResult {
      Ok(())
    }
  }

  #[test]
  fn pass_manager_proper_initial_values() {
    assert_eq!(true, PassManager::new().passes.is_empty());
  }

  #[test]
  fn pass_manager_add_pass() {
    let mut pass_manager = PassManager::new();

    pass_manager.add_pass(Box::new(TestPassEmpty {}));

    assert_eq!(1, pass_manager.passes.len());
  }

  #[test]
  fn pass_manager_add_pass_no_register() {
    let mut pass_manager = PassManager::new();

    pass_manager.add_pass(Box::new(TestPassNoRegister {}));

    assert_eq!(true, pass_manager.passes.is_empty());
  }
}
