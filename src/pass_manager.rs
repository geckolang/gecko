use crate::{diagnostic, node, pass};

pub struct PassManager<'a> {
  passes: Vec<Box<dyn pass::Pass<'a>>>,
}

impl<'a> PassManager<'a> {
  pub fn new() -> Self {
    Self { passes: vec![] }
  }

  pub fn add_pass(&mut self, pass: Box<dyn pass::Pass<'a>>) -> bool {
    if !pass.register(self) {
      return false;
    }

    self.passes.push(pass);

    true
  }

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

  impl pass::Pass<'_> for TestPassEmpty {
    //
  }

  struct TestPassNoRegister {
    //
  }

  impl pass::Pass<'_> for TestPassNoRegister {
    fn register(&self, _: &PassManager) -> bool {
      return false;
    }
  }

  struct TestPassWithVisit<'a> {
    pub is_visit_invoked: &'a bool,
  }

  impl<'a> TestPassWithVisit<'a> {
    pub fn new(is_visit_invoked: &'a bool) -> Self {
      Self { is_visit_invoked }
    }
  }

  impl<'a> pass::Pass<'_> for TestPassWithVisit<'a> {
    fn visit(&mut self, _: &dyn node::Node) -> pass::PassResult {
      self.is_visit_invoked = &true;

      Ok(())
    }
  }

  struct TestNode {
    //
  }

  impl node::Node for TestNode {
    fn accept(&mut self, _: &dyn pass::Pass) {
      //
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

  #[test]
  fn pass_manager_run_invoke_visit() {
    let mut pass_manager = PassManager::new();
    let mut is_visit_invoked = &false;
    let test_pass = Box::new(TestPassWithVisit::new(&mut is_visit_invoked));

    pass_manager.add_pass(test_pass);
    pass_manager.run(&TestNode {});

    assert_eq!(true, *is_visit_invoked);
  }
}
