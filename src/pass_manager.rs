use crate::pass;

pub struct PassManager {
  passes: Vec<pass::Pass>,
}

impl PassManager {
  pub fn add_pass(&self, pass: pass::Pass) -> bool {}
}
