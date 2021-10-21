use crate::{diagnostic, node, pass};

pub struct PassManager<'a> {
  passes: Vec<&'a dyn pass::Pass>,
}

impl<'a> PassManager<'a> {
  pub fn new() -> Self {
    Self { passes: vec![] }
  }

  pub fn add_pass(&mut self, pass: &'a dyn pass::Pass) -> bool {
    if !pass.register(self) {
      return false;
    }

    self.passes.push(pass);

    true
  }

  pub fn run(&self, root_node: &dyn node::Node) -> Vec<diagnostic::Diagnostic> {
    // TODO: Better structure/organization of diagnostics.

    let mut diagnostics = vec![];

    for pass in self.passes.iter() {
      let visitation_result = pass.visit(root_node);

      for diagnostic in pass.get_diagnostics().iter() {
        diagnostics.push(*diagnostic);
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

  struct TestPassEmpty {}

  impl pass::Pass for TestPassEmpty {}

  struct TestPassNoRegister {}

  impl pass::Pass for TestPassNoRegister {
    fn register(&self, pass_manager: &PassManager) -> bool {
      return false;
    }
  }

  struct TestPassWithVisit {
    pub is_visit_invoked: bool,
  }

  impl TestPassWithVisit {
    pub fn new() -> Self {
      Self {
        is_visit_invoked: false,
      }
    }
  }

  struct TestNode {}

  impl node::Node for TestNode {
    fn accept(&mut self, pass: &dyn pass::Pass) {}
  }

  impl pass::Pass for TestPassWithVisit {
    fn visit(&mut self, node: &dyn node::Node) -> pass::PassResult {
      self.is_visit_invoked = true;

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

    pass_manager.add_pass(&TestPassEmpty {});

    assert_eq!(1, pass_manager.passes.len());
  }

  #[test]
  fn pass_manager_add_pass_no_register() {
    let mut pass_manager = PassManager::new();

    pass_manager.add_pass(&TestPassNoRegister {});

    assert_eq!(true, pass_manager.passes.is_empty());
  }

  #[test]
  fn pass_manager_run_invoke_visit() {
    let mut pass_manager = PassManager::new();
    let mut test_pass = TestPassWithVisit::new();

    pass_manager.add_pass(&test_pass);
    pass_manager.run(&TestNode {});

    assert_eq!(true, test_pass.is_visit_invoked);
  }
}
