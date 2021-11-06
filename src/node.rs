use crate::{int_kind, pass, void_kind};

#[derive(Hash, Eq, PartialEq, Debug, Copy, Clone)]
pub enum AnyKindNode {
  IntKind(int_kind::IntKind),
  VoidKind(void_kind::VoidKind),
  BoolKind(int_kind::BoolKind),
}

#[derive(Hash, Eq, PartialEq, Debug, Copy, Clone)]
pub enum AnyLiteralNode {
  BoolLiteral(BoolLiteral),
}

pub trait Node {
  fn accept(&mut self, pass: &mut dyn pass::Pass) -> pass::PassResult;

  fn get_children(&self) -> Vec<&dyn Node> {
    vec![]
  }
}

#[derive(Hash, Eq, PartialEq, Debug)]
pub struct Identifier {
  pub name: String,
}

#[derive(Hash, Eq, PartialEq, Debug, Copy, Clone)]
pub struct BoolLiteral {
  pub value: bool,
}

impl Node for BoolLiteral {
  fn accept(&mut self, pass: &mut dyn pass::Pass) -> pass::PassResult {
    pass.visit_bool_literal(self)
  }
}

#[derive(Hash, Eq, PartialEq, Debug)]
pub struct KindGroup {
  pub kind: AnyKindNode,
  pub is_reference: bool,
  pub is_mutable: bool,
}

#[derive(Hash, Eq, PartialEq, Debug)]
pub struct External {
  pub prototype: Prototype,
}

impl Node for External {
  fn accept(&mut self, pass: &mut dyn pass::Pass) -> pass::PassResult {
    pass.visit_external(self)?;

    Ok(())
  }
}

#[derive(Hash, Eq, PartialEq, Debug)]
pub struct Function {
  pub is_public: bool,
  pub prototype: Prototype,
  pub body: Block,
}

impl Node for Function {
  fn accept(&mut self, pass: &mut dyn pass::Pass) -> pass::PassResult {
    pass.visit_function(self)?;

    Ok(())
  }
}

pub type Parameter = (String, KindGroup);

#[derive(Hash, Eq, PartialEq, Debug)]
pub struct Prototype {
  pub name: String,
  pub parameters: Vec<Parameter>,
  pub is_variadic: bool,
  pub return_kind_group: KindGroup,
}

impl Node for Prototype {
  fn accept(&mut self, pass: &mut dyn pass::Pass) -> pass::PassResult {
    pass.visit_prototype(self)?;

    Ok(())
  }
}

#[derive(Hash, Eq, PartialEq, Debug)]
pub enum TopLevelNode {
  Function(Function),
  External(External),
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

impl Node for Package {
  fn accept(&mut self, pass: &mut dyn pass::Pass) -> pass::PassResult {
    pass.visit_package(self)?;

    Ok(())
  }
}

#[derive(Hash, Eq, PartialEq, Debug)]
pub enum AnyStatementNode {
  ReturnStmt(ReturnStmt),
}

#[derive(Hash, Eq, PartialEq, Debug)]
pub struct Block {
  pub statements: Vec<AnyStatementNode>,
}

impl Node for Block {
  fn accept(&mut self, pass: &mut dyn pass::Pass) -> pass::PassResult {
    pass.visit_block(self)?;

    Ok(())
  }
}

#[derive(Hash, Eq, PartialEq, Debug)]
pub struct ReturnStmt {
  pub value: Option<AnyLiteralNode>,
}

impl Node for ReturnStmt {
  fn accept(&mut self, pass: &mut dyn pass::Pass) -> pass::PassResult {
    pass.visit_return_stmt(self)?;

    Ok(())
  }
}
