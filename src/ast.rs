use crate::{context, name_resolution};

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
      $crate::ast::Node::ContinueStmt(inner) => $target_fn(inner $(, $($args),* )?),
      $crate::ast::Node::ExprWrapperStmt(inner) => $target_fn(inner $(, $($args),* )?),
      $crate::ast::Node::ArrayAssignStmt(inner) => $target_fn(inner $(, $($args),* )?),
      $crate::ast::Node::Definition(inner) => $target_fn(inner $(, $($args),* )?),
      $crate::ast::Node::VariableRef(inner) => $target_fn(inner $(, $($args),* )?),
      $crate::ast::Node::VariableAssignStmt(inner) => $target_fn(inner $(, $($args),* )?),
      $crate::ast::Node::BinaryExpr(inner) => $target_fn(inner $(, $($args),* )?),
      $crate::ast::Node::Parameter(inner) => $target_fn(inner $(, $($args),* )?),
      $crate::ast::Node::UnsafeBlock(inner) => $target_fn(inner $(, $($args),* )?),
      $crate::ast::Node::ArrayValue(inner) => $target_fn(inner $(, $($args),* )?),
      $crate::ast::Node::ArrayIndexing(inner) => $target_fn(inner $(, $($args),* )?),
      $crate::ast::Node::Enum(inner) => $target_fn(inner $(, $($args),* )?),
    }
  };
}

/// A parameter containing its name, type, and index position.
pub type Parameter = (String, Type, u32);

#[derive(PartialEq, PartialOrd, Clone)]
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

#[derive(PartialEq, Clone)]
pub enum PrimitiveType {
  Int(IntSize),
  Bool,
  Char,
  String,
}

#[derive(Clone)]
pub enum Type {
  Array(Box<Type>, u32),
  PrimitiveType(PrimitiveType),
  Prototype(Vec<Definition>, Option<Box<Type>>, bool),
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
  ContinueStmt(ContinueStmt),
  ExprWrapperStmt(ExprWrapperStmt),
  ArrayAssignStmt(ArrayAssignStmt),
  Definition(Definition),
  VariableRef(VariableRef),
  VariableAssignStmt(VariableAssignStmt),
  BinaryExpr(BinaryExpr),
  Parameter(Parameter),
  UnsafeBlock(UnsafeBlock),
  ArrayValue(ArrayValue),
  ArrayIndexing(ArrayIndexing),
  Enum(Enum),
}

pub struct Enum {
  pub name: String,
  pub variants: Vec<String>,
}

pub struct ContinueStmt;

pub struct ArrayIndexing {
  pub name: String,
  pub index: Box<Node>,
  pub definition_key: Option<context::DefinitionKey>,
}

pub struct ArrayAssignStmt {
  pub name: String,
  pub index: Box<Node>,
  pub value: Box<Node>,
  pub definition_key: Option<context::DefinitionKey>,
}

pub struct ArrayValue {
  pub elements: Vec<Node>,
  pub explicit_type: Option<Type>,
}

pub struct UnsafeBlock(pub Block);

pub struct VariableRef {
  pub name: String,
  pub definition_key: Option<context::DefinitionKey>,
}

pub struct VariableAssignStmt {
  pub name: String,
  pub value: Box<Node>,
  pub definition_key: Option<context::DefinitionKey>,
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
  pub callee_definition_key: Option<context::DefinitionKey>,
  pub arguments: Vec<Node>,
}

pub enum OperatorKind {
  Not,
  Add,
  Subtract,
  Multiply,
  Divide,
  LessThan,
  GreaterThan,
  LessThanOrEqual,
  GreaterThanOrEqual,
  Equal,
}

pub struct BinaryExpr {
  pub left: Box<Node>,
  pub right: Box<Node>,
  pub operator: OperatorKind,
}

#[derive(Clone)]
pub struct Definition {
  pub name: String,
  pub symbol_kind: name_resolution::SymbolKind,
  pub node: std::rc::Rc<std::cell::RefCell<Node>>,
  pub key: context::DefinitionKey,
}
