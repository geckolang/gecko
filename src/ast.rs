use crate::{cache, name_resolution};

#[macro_export]
macro_rules! dispatch {
  ($node:expr, $target_fn:expr $(, $($args:expr),* )? ) => {
    match $node {
      $crate::ast::Node::Literal(inner) => $target_fn(inner $(, $($args),* )?),
      $crate::ast::Node::ExternFunction(inner) => $target_fn(inner $(, $($args),* )?),
      $crate::ast::Node::ExternStatic(inner) => $target_fn(inner $(, $($args),* )?),
      $crate::ast::Node::Function(inner) => $target_fn(inner $(, $($args),* )?),
      $crate::ast::Node::Block(inner) => $target_fn(inner $(, $($args),* )?),
      $crate::ast::Node::ReturnStmt(inner) => $target_fn(inner $(, $($args),* )?),
      $crate::ast::Node::LetStmt(inner) => $target_fn(inner $(, $($args),* )?),
      $crate::ast::Node::IfStmt(inner) => $target_fn(inner $(, $($args),* )?),
      $crate::ast::Node::LoopStmt(inner) => $target_fn(inner $(, $($args),* )?),
      $crate::ast::Node::FunctionCall(inner) => $target_fn(inner $(, $($args),* )?),
      $crate::ast::Node::IntrinsicCall(inner) => $target_fn(inner $(, $($args),* )?),
      $crate::ast::Node::BreakStmt(inner) => $target_fn(inner $(, $($args),* )?),
      $crate::ast::Node::ContinueStmt(inner) => $target_fn(inner $(, $($args),* )?),
      $crate::ast::Node::InlineExprStmt(inner) => $target_fn(inner $(, $($args),* )?),
      $crate::ast::Node::Definition(inner) => $target_fn(inner $(, $($args),* )?),
      $crate::ast::Node::VariableOrMemberRef(inner) => $target_fn(inner $(, $($args),* )?),
      $crate::ast::Node::AssignStmt(inner) => $target_fn(inner $(, $($args),* )?),
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
      $crate::ast::Node::Pattern(inner) => $target_fn(inner $(, $($args),* )?),
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
  Null,
}

#[derive(PartialEq, Clone)]
pub enum Type {
  Array(Box<Type>, u32),
  Primitive(PrimitiveType),
  Pointer(Box<Type>),
  // TODO: Isn't this incompatible with `UserDefined`?
  Struct(StructType),
  UserDefined(UserDefinedType),
  Unit,
}

impl Type {
  pub fn is_unit(&self) -> bool {
    matches!(self, Type::Unit)
  }
}

// TODO: Write a macro that both defines this and `as_x_node()` (which alternatively yields `unreachable!()`) methods.
pub enum Node {
  Literal(Literal),
  ExternFunction(ExternFunction),
  ExternStatic(ExternStatic),
  Function(Function),
  Block(Block),
  ReturnStmt(ReturnStmt),
  LetStmt(LetStmt),
  IfStmt(IfStmt),
  LoopStmt(LoopStmt),
  FunctionCall(FunctionCall),
  IntrinsicCall(IntrinsicCall),
  BreakStmt(BreakStmt),
  ContinueStmt(ContinueStmt),
  InlineExprStmt(InlineExprStmt),
  Definition(Definition),
  VariableOrMemberRef(VariableOrMemberRef),
  AssignStmt(AssignStmt),
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
  Pattern(Pattern),
}

// TODO: If it's never boxed under `ast::Node`, then there might not be a need for it to be included under `ast::Node`?
pub struct Pattern {
  pub module_name: Option<String>,
  pub base_name: String,
  pub member_path: Vec<String>,
  pub symbol_kind: name_resolution::SymbolKind,
  pub target_key: Option<cache::DefinitionKey>,
}

impl Pattern {
  pub fn new(base_name: String, symbol_kind: name_resolution::SymbolKind) -> Self {
    Pattern {
      module_name: None,
      base_name,
      member_path: Vec::new(),
      symbol_kind,
      target_key: None,
    }
  }
}

impl ToString for Pattern {
  fn to_string(&self) -> String {
    // TODO: Missing the module name.
    // TODO: Hard-coded character.
    self.base_name.clone() + self.member_path.join(".").as_str()
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

pub struct VariableOrMemberRef(pub Pattern);

pub struct AssignStmt {
  pub assignee_expr: Box<Node>,
  pub value: Box<Node>,
}

pub enum Literal {
  Bool(bool),
  Int(u64, IntSize),
  Char(char),
  String(String),
  Nullptr,
}

pub struct Prototype {
  pub parameters: Vec<Parameter>,
  pub return_type: Type,
  pub is_variadic: bool,
}

pub struct ExternFunction {
  pub name: String,
  pub prototype: Prototype,
  pub attributes: Vec<Attribute>,
}

// TODO: What about the type? Do we care?
pub struct ExternStatic(pub String, pub Type);

pub struct Attribute {
  pub name: String,
  pub values: Vec<Literal>,
}

pub struct Function {
  pub name: String,
  pub prototype: Prototype,
  pub body: Block,
  pub attributes: Vec<Attribute>,
}

pub struct Block {
  pub statements: Vec<Box<Node>>,
  pub yield_last_expr: bool,
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

pub struct LoopStmt {
  pub condition: Option<Box<Node>>,
  pub body: Block,
}

pub struct InlineExprStmt {
  pub expr: Box<Node>,
}

pub struct FunctionCall {
  pub callee_pattern: Pattern,
  pub arguments: Vec<Node>,
}

pub enum IntrinsicKind {
  Panic,
}

pub struct IntrinsicCall {
  pub kind: IntrinsicKind,
  pub arguments: Vec<Node>,
}

#[derive(PartialEq, Clone)]
pub struct StructType {
  pub name: String,
  pub fields: std::collections::HashMap<String, Type>,
}

pub enum OperatorKind {
  And,
  Or,
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
  Equality,
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

pub struct Definition {
  pub name: String,
  pub symbol_kind: name_resolution::SymbolKind,
  pub node_ref_cell: cache::CachedNode,
  pub definition_key: cache::DefinitionKey,
}

pub struct MemberAccess {
  pub scope_qualifier: Pattern,
  pub target_key: Option<cache::DefinitionKey>,
}
