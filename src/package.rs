use crate::{external, function, node, pass};

#[derive(Hash, Eq, PartialEq, Debug)]
pub enum TopLevelNode {
  Function(function::Function),
  External(external::External),
}

pub struct Package {
  pub name: String,
  pub symbol_table: std::collections::HashMap<String, TopLevelNode>,
}

impl Package {
  pub fn new(name: String) -> Self {
    Self {
      name,
      symbol_table: std::collections::HashMap::new(),
    }
  }
}

impl node::Node for Package {
  fn accept(&mut self, pass: &mut dyn pass::Pass) -> pass::PassResult {
    pass.visit_package(self)?;

    Ok(())
  }
}
