use crate::{function, node, pass};

pub struct TypeCheckPass {}

impl pass::Pass for TypeCheckPass {
  fn visit_function(&mut self, function: &function::Function) -> pass::PassResult {
    todo!();
  }
}
