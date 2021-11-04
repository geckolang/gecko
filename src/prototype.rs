use crate::node;
use crate::pass;

pub type Parameter = (String, node::KindGroup);

#[derive(Hash, Eq, PartialEq, Debug)]
pub struct Prototype {
  pub name: String,
  pub parameters: Vec<Parameter>,
  pub is_variadic: bool,
  pub return_kind_group: node::KindGroup,
}

impl node::Node for Prototype {
  fn accept(&mut self, pass: &mut dyn pass::Pass) -> pass::PassResult {
    pass.visit_prototype(self)?;

    Ok(())
  }
}
