use crate::{node, pass};

#[derive(Hash, Eq, PartialEq, Debug)]
pub enum AnyStatementNode {
  ReturnStmt(ReturnStmt),
}

#[derive(Hash, Eq, PartialEq, Debug)]
pub enum AnyExprNode {}

#[derive(Hash, Eq, PartialEq, Debug)]
pub struct Block {
  pub statements: Vec<AnyStatementNode>,
}

impl node::Node for Block {
  fn accept(&mut self, pass: &mut dyn pass::Pass) -> pass::PassResult {
    pass.visit_block(self)?;

    Ok(())
  }
}

#[derive(Hash, Eq, PartialEq, Debug)]
pub struct ReturnStmt {
  pub value: Option<AnyExprNode>,
}

impl node::Node for ReturnStmt {
  fn accept(&mut self, pass: &mut dyn pass::Pass) -> pass::PassResult {
    pass.visit_return_stmt(self)?;

    Ok(())
  }
}
