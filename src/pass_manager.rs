use crate::{diagnostic, node, pass, pass::Pass};

pub struct PassManager<'a> {
  analysis_passes: Vec<Box<dyn pass::AnalysisPass<'a>>>,
  transform_passes: Vec<Box<dyn pass::TransformPass<'a>>>,
}

impl<'a> PassManager<'a> {
  pub fn new() -> Self {
    Self {
      analysis_passes: Vec::new(),
      transform_passes: Vec::new(),
    }
  }

  /// Register an analysis pass to be run in sequential order.
  pub fn add_analysis_pass(&mut self, analysis_pass: Box<dyn pass::AnalysisPass<'a>>) {
    self.analysis_passes.push(analysis_pass);
  }

  /// Register a transform pass to be run in sequential order.
  pub fn add_transform_pass(&mut self, transform_pass: Box<dyn pass::TransformPass<'a>>) {
    self.transform_passes.push(transform_pass);
  }

  /// Execute all registered analysis passes in a sequential order.
  ///
  /// Before a pass is executed, its requirements will be gathered
  /// and checked. All top-level nodes will be run over pass-by-pass.
  /// If there are any diagnostics generated, they will be gathered
  /// and returned as a vector, otherwise an empty vector will be
  /// returned.
  pub fn run(&mut self, nodes: &'a Vec<&'a mut dyn node::Node>) -> Vec<diagnostic::Diagnostic> {
    // TODO: Improve structure/organization of diagnostics?

    let mut diagnostics = Vec::<diagnostic::Diagnostic>::new();
    let mut error_found = false;

    // FIXME: Run in order between both analysis and transform passes.

    for analysis_pass in &mut self.analysis_passes {
      let requirements = analysis_pass.get_requirements();

      // TODO: Abstract logic to a function.
      // TODO: Ensure logic is correct.
      if !requirements.ignore_previous_errors && !diagnostics.is_empty() {
        if error_found {
          continue;
        }

        for diagnostic in &diagnostics {
          if diagnostic.is_error_like() {
            error_found = true;

            continue;
          }
        }
      }

      for node in nodes {
        // TODO: Dereferencing node. Is this copying something, or is it correct?
        let visitation_result = analysis_pass.visit(*node);

        if let Err(diagnostic) = visitation_result {
          diagnostics.push(diagnostic);
        }
      }

      diagnostics.extend(analysis_pass.get_diagnostics());
    }

    diagnostics
  }
}

#[cfg(test)]
mod tests {
  use super::*;

  struct TestPassEmpty;

  impl<'a> pass::AnalysisPass<'a> for TestPassEmpty {
    fn visit(&mut self, _: &'a dyn node::Node) -> pass::PassResult {
      Ok(())
    }
  }

  struct TestPassNoRegister;

  impl<'a> pass::AnalysisPass<'a> for TestPassNoRegister {
    fn visit(&mut self, _: &'a dyn node::Node) -> pass::PassResult {
      Ok(())
    }
  }

  #[test]
  fn pass_manager_proper_initial_values() {
    assert_eq!(true, PassManager::new().analysis_passes.is_empty());
  }

  #[test]
  fn pass_manager_add_pass() {
    let mut pass_manager = PassManager::new();
    let test_pass = TestPassEmpty;

    pass_manager.add_analysis_pass(Box::new(test_pass));
    assert_eq!(1, pass_manager.analysis_passes.len());
  }

  #[test]
  fn pass_manager_add_pass_no_register() {
    let mut pass_manager = PassManager::new();
    let test_pass_no_register = TestPassNoRegister;

    pass_manager.add_analysis_pass(Box::new(test_pass_no_register));
    assert_eq!(true, pass_manager.analysis_passes.is_empty());
  }

  // TODO: Run test.
}
