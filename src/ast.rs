use crate::context::{self, DefinitionKey};

#[macro_export]
macro_rules! dispatch {
  ($node:expr, $target_fn:expr $(, $($args:expr),* )? ) => {
    match $node {
      $crate::ast::Node::Literal(inner) => $target_fn(inner $(, $($args),* )?),
      $crate::ast::Node::Extern(inner) => $target_fn(inner $(, $($args),* )?),
      $crate::ast::Node::Function(inner) => $target_fn(inner $(, $($args),* )?),
      $crate::ast::Node::Block(inner) => $target_fn(inner $(, $($args),* )?),
      $crate::ast::Node::ReturnStmt(inner) => $target_fn(inner $(, $($args),* )?),
      $crate::ast::Node::LetStmt(inner) => $target_fn(inner $(, $($args),* )?),
      $crate::ast::Node::IfStmt(inner) => $target_fn(inner $(, $($args),* )?),
      $crate::ast::Node::WhileStmt(inner) => $target_fn(inner $(, $($args),* )?),
      $crate::ast::Node::FunctionCall(inner) => $target_fn(inner $(, $($args),* )?),
      $crate::ast::Node::BreakStmt(inner) => $target_fn(inner $(, $($args),* )?),
      $crate::ast::Node::ExprWrapperStmt(inner) => $target_fn(inner $(, $($args),* )?),
      $crate::ast::Node::Definition(inner) => $target_fn(inner $(, $($args),* )?),
      $crate::ast::Node::VariableRef(inner) => $target_fn(inner $(, $($args),* )?),
    }
  };
}

pub type Parameter = (String, Type);

#[derive(PartialEq, PartialOrd)]
pub enum IntSize {
  U8,
  U16,
  U32,
  U64,
  Usize,
  I8,
  I16,
  I32,
  I64,
  Isize,
}

pub enum PrimitiveType {
  Int(IntSize),
  Bool,
  Char,
}

pub enum Type {
  PrimitiveType(PrimitiveType),
  Prototype(Vec<(String, Type)>, Option<Box<Type>>, bool),
}

pub enum Node {
  Literal(Literal),
  Extern(Extern),
  Function(Function),
  Block(Block),
  ReturnStmt(ReturnStmt),
  LetStmt(LetStmt),
  IfStmt(IfStmt),
  WhileStmt(WhileStmt),
  FunctionCall(FunctionCall),
  BreakStmt(BreakStmt),
  ExprWrapperStmt(ExprWrapperStmt),
  Definition(Definition),
  VariableRef(VariableRef),
}

pub struct VariableRef {
  pub name: String,
  pub definition_key: Option<DefinitionKey>,
}

pub enum Literal {
  Bool(bool),
  Int(u64, IntSize),
  Char(char),
  String(String),
}

pub struct Extern {
  pub name: String,
  pub prototype: Type,
}

pub struct Function {
  pub name: String,
  pub prototype: Type,
  pub body: Block,
}

pub struct Block {
  pub statements: Vec<Box<Node>>,
}

pub struct BreakStmt {
  //
}

pub struct ReturnStmt {
  pub value: Option<Box<Node>>,
}

pub struct LetStmt {
  pub name: String,
  pub ty: Type,
  pub value: Box<Node>,
}

pub struct IfStmt {
  pub condition: Box<Node>,
  pub then_block: Block,
  pub else_block: Option<Block>,
}

pub struct WhileStmt {
  pub condition: Box<Node>,
  pub body: Block,
}

pub struct ExprWrapperStmt {
  pub expr: Box<Node>,
}

pub struct FunctionCall {
  pub callee_name: String,
  pub callee_definition_key: Option<DefinitionKey>,
  pub arguments: Vec<Node>,
}

pub struct Definition {
  pub name: String,
  pub node: std::rc::Rc<std::cell::RefCell<Node>>,
  pub key: context::DefinitionKey,
}
