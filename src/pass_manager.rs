use crate::{diagnostic, node, pass};

pub struct PassManager<'a> {
  passes: Vec<&'a mut dyn pass::Pass<'a>>,
}

pub enum TopLevelNodeTransport<'a> {
  Function(node::Function<'a>),
  External(node::External),
  Module(node::Module<'a>),
}

impl<'a> PassManager<'a> {
  pub fn new() -> Self {
    Self { passes: Vec::new() }
  }

  /// Register a pass to be run. Returns `true` if the pass'
  /// restrictions are met.
  pub fn add_pass(&mut self, pass: &'a mut dyn pass::Pass<'a>) -> bool {
    if !pass.register(self) {
      return false;
    }

    self.passes.push(pass);

    true
  }

  // TODO: Improve documentation.
  /// Execute all registered passes in a sequential order.
  pub fn run(
    &'a mut self,
    top_level_nodes: &'a Vec<TopLevelNodeTransport<'a>>,
  ) -> Vec<diagnostic::Diagnostic> {
    // TODO: Improve structure/organization of diagnostics?

    let mut diagnostics = Vec::<diagnostic::Diagnostic>::new();
    let mut is_there_previous_error = false;

    for pass in &mut self.passes {
      let requirements = pass.get_requirements();

      // TODO: Ensure logic is correct.
      if !requirements.ignore_previous_errors && !diagnostics.is_empty() {
        if is_there_previous_error {
          continue;
        }

        for diagnostic in &diagnostics {
          if diagnostic.severity == diagnostic::Severity::Error
            || diagnostic.severity == diagnostic::Severity::Internal
          {
            is_there_previous_error = true;

            continue;
          }
        }
      }

      for top_level_node in top_level_nodes {
        let visitation_result = match top_level_node {
          TopLevelNodeTransport::External(external) => pass.visit(external),
          TopLevelNodeTransport::Function(function) => pass.visit(function),
          TopLevelNodeTransport::Module(module) => pass.visit(module),
        };

        diagnostics.extend(pass.get_diagnostics());

        if let Err(diagnostic) = visitation_result {
          diagnostics.push(diagnostic);
        }
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
    fn visit(&mut self, _: &'a dyn node::Node) -> pass::PassResult {
      Ok(())
    }
  }

  struct TestPassNoRegister;

  impl<'a> pass::Pass<'a> for TestPassNoRegister {
    fn visit(&mut self, _: &'a dyn node::Node) -> pass::PassResult {
      Ok(())
    }

    fn register(&self, _: &PassManager<'_>) -> bool {
      return false;
    }
  }

  #[test]
  fn pass_manager_proper_initial_values() {
    assert_eq!(true, PassManager::new().passes.is_empty());
  }

  #[test]
  fn pass_manager_add_pass() {
    let mut pass_manager = PassManager::new();
    let mut test_pass = TestPassEmpty;

    pass_manager.add_pass(&mut test_pass);
    assert_eq!(1, pass_manager.passes.len());
  }

  #[test]
  fn pass_manager_add_pass_no_register() {
    let mut pass_manager = PassManager::new();
    let mut test_pass_no_register = TestPassNoRegister;

    pass_manager.add_pass(&mut test_pass_no_register);
    assert_eq!(true, pass_manager.passes.is_empty());
  }

  // TODO: Run test.
}
