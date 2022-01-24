use crate::{cache, name_resolution};

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
      $crate::ast::Node::Definition(inner) => $target_fn(inner $(, $($args),* )?),
      $crate::ast::Node::VariableRef(inner) => $target_fn(inner $(, $($args),* )?),
      $crate::ast::Node::VariableAssignStmt(inner) => $target_fn(inner $(, $($args),* )?),
      $crate::ast::Node::BinaryExpr(inner) => $target_fn(inner $(, $($args),* )?),
      $crate::ast::Node::UnaryExpr(inner) => $target_fn(inner $(, $($args),* )?),
      $crate::ast::Node::Parameter(inner) => $target_fn(inner $(, $($args),* )?),
      $crate::ast::Node::UnsafeBlock(inner) => $target_fn(inner $(, $($args),* )?),
      $crate::ast::Node::ArrayValue(inner) => $target_fn(inner $(, $($args),* )?),
      $crate::ast::Node::ArrayIndexing(inner) => $target_fn(inner $(, $($args),* )?),
      $crate::ast::Node::Enum(inner) => $target_fn(inner $(, $($args),* )?),
      $crate::ast::Node::StructType(inner) => $target_fn(inner $(, $($args),* )?),
      $crate::ast::Node::Prototype(inner) => $target_fn(inner $(, $($args),* )?),
      $crate::ast::Node::StructValue(inner) => $target_fn(inner $(, $($args),* )?),
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

#[derive(PartialEq, Clone)]
pub enum Type {
  Array(Box<Type>, u32),
  Primitive(PrimitiveType),
  Pointer(Box<Type>),
  // TODO: Isn't this incompatible with `UserDefined`?
  Struct(StructType),
  UserDefined(UserDefinedType),
}

// TODO: Write a macro that both defines this and `as_x_node()` (which alternatively yields `unreachable!()`) methods.
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
  ExprWrapperStmt(ExprStmt),
  Definition(Definition),
  VariableRef(VariableRef),
  VariableAssignStmt(AssignStmt),
  BinaryExpr(BinaryExpr),
  UnaryExpr(UnaryExpr),
  Parameter(Parameter),
  UnsafeBlock(UnsafeBlockStmt),
  ArrayValue(ArrayValue),
  ArrayIndexing(ArrayIndexing),
  Enum(Enum),
  StructType(StructType),
  Prototype(Prototype),
  StructValue(StructValue),
}

pub struct ScopeQualifier(pub String, pub Vec<String>);

impl ScopeQualifier {
  pub fn new(name: String) -> Self {
    ScopeQualifier(name, vec![])
  }
}

impl ToString for ScopeQualifier {
  fn to_string(&self) -> String {
    let mut result = self.0.clone();

    for scope in &self.1 {
      // FIXME: Verify that these characters are allowed in LLVM identifiers.
      result.push_str("::");
      result.push_str(scope);
    }

    result
  }
}

#[derive(PartialEq, Clone)]
pub struct UserDefinedType {
  pub name: String,
  pub target_key: Option<cache::DefinitionKey>,
}

pub struct StructValue {
  pub name: String,
  pub fields: Vec<Node>,
  pub target_key: Option<cache::DefinitionKey>,
}

pub struct Enum {
  pub name: String,
  pub variants: Vec<String>,
}

pub struct ContinueStmt;

pub struct ArrayIndexing {
  pub name: String,
  pub index: Box<Node>,
  pub target_key: Option<cache::DefinitionKey>,
}

pub struct ArrayValue {
  pub elements: Vec<Node>,
  /// Holds the type of the array, in case it is an empty array.
  pub explicit_type: Option<Type>,
}

pub struct UnsafeBlockStmt(pub Block);

pub struct VariableRef {
  pub name: String,
  pub target_key: Option<cache::DefinitionKey>,
}

pub struct AssignStmt {
  pub assignee_expr: Box<Node>,
  pub value: Box<Node>,
}

pub enum Literal {
  Bool(bool),
  Int(u64, IntSize),
  Char(char),
  String(String),
}

pub struct Prototype {
  pub parameters: Vec<Parameter>,
  pub return_type: Option<Type>,
  pub is_variadic: bool,
}

pub struct Extern {
  pub name: String,
  pub prototype: Prototype,
}

pub struct Function {
  pub name: String,
  pub prototype: Prototype,
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
  pub is_mutable: bool,
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

pub struct ExprStmt {
  pub expr: Box<Node>,
}

pub struct FunctionCall {
  pub callee_id: ScopeQualifier,
  pub target_key: Option<cache::DefinitionKey>,
  pub arguments: Vec<Node>,
}

#[derive(PartialEq, Clone)]
pub struct StructType {
  pub name: String,
  pub fields: std::collections::HashMap<String, Type>,
}

pub enum OperatorKind {
  Not,
  AddressOf,
  Add,
  SubtractOrNegate,
  MultiplyOrDereference,
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

pub struct UnaryExpr {
  pub expr: Box<Node>,
  pub operator: OperatorKind,
}

#[derive(Clone)]
pub struct Definition {
  pub name: String,
  pub symbol_kind: name_resolution::SymbolKind,
  pub node: std::rc::Rc<std::cell::RefCell<Node>>,
  pub definition_key: cache::DefinitionKey,
}
