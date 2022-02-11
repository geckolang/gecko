use crate::{cache, diagnostic, name_resolution};

#[macro_export]
macro_rules! dispatch {
  ($node:expr, $target_fn:expr $(, $($args:expr),* )? ) => {
    match $node {
      $crate::ast::NodeKind::Literal(inner) => $target_fn(inner $(, $($args),* )?),
      $crate::ast::NodeKind::ExternFunction(inner) => $target_fn(inner $(, $($args),* )?),
      $crate::ast::NodeKind::ExternStatic(inner) => $target_fn(inner $(, $($args),* )?),
      $crate::ast::NodeKind::Function(inner) => $target_fn(inner $(, $($args),* )?),
      $crate::ast::NodeKind::Block(inner) => $target_fn(inner $(, $($args),* )?),
      $crate::ast::NodeKind::ReturnStmt(inner) => $target_fn(inner $(, $($args),* )?),
      $crate::ast::NodeKind::LetStmt(inner) => $target_fn(inner $(, $($args),* )?),
      $crate::ast::NodeKind::IfStmt(inner) => $target_fn(inner $(, $($args),* )?),
      $crate::ast::NodeKind::LoopStmt(inner) => $target_fn(inner $(, $($args),* )?),
      $crate::ast::NodeKind::FunctionCall(inner) => $target_fn(inner $(, $($args),* )?),
      $crate::ast::NodeKind::IntrinsicCall(inner) => $target_fn(inner $(, $($args),* )?),
      $crate::ast::NodeKind::BreakStmt(inner) => $target_fn(inner $(, $($args),* )?),
      $crate::ast::NodeKind::ContinueStmt(inner) => $target_fn(inner $(, $($args),* )?),
      $crate::ast::NodeKind::InlineExprStmt(inner) => $target_fn(inner $(, $($args),* )?),
      $crate::ast::NodeKind::Definition(inner) => $target_fn(inner $(, $($args),* )?),
      $crate::ast::NodeKind::VariableOrMemberRef(inner) => $target_fn(inner $(, $($args),* )?),
      $crate::ast::NodeKind::AssignStmt(inner) => $target_fn(inner $(, $($args),* )?),
      $crate::ast::NodeKind::BinaryExpr(inner) => $target_fn(inner $(, $($args),* )?),
      $crate::ast::NodeKind::UnaryExpr(inner) => $target_fn(inner $(, $($args),* )?),
      $crate::ast::NodeKind::Parameter(inner) => $target_fn(inner $(, $($args),* )?),
      $crate::ast::NodeKind::UnsafeBlock(inner) => $target_fn(inner $(, $($args),* )?),
      $crate::ast::NodeKind::ArrayValue(inner) => $target_fn(inner $(, $($args),* )?),
      $crate::ast::NodeKind::ArrayIndexing(inner) => $target_fn(inner $(, $($args),* )?),
      $crate::ast::NodeKind::Enum(inner) => $target_fn(inner $(, $($args),* )?),
      $crate::ast::NodeKind::StructType(inner) => $target_fn(inner $(, $($args),* )?),
      $crate::ast::NodeKind::Prototype(inner) => $target_fn(inner $(, $($args),* )?),
      $crate::ast::NodeKind::StructValue(inner) => $target_fn(inner $(, $($args),* )?),
      $crate::ast::NodeKind::Pattern(inner) => $target_fn(inner $(, $($args),* )?),
      $crate::ast::NodeKind::TypeAlias(inner) => $target_fn(inner $(, $($args),* )?),
      $crate::ast::NodeKind::Closure(inner) => $target_fn(inner $(, $($args),* )?),
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
  // TODO: Consider merging with `Pointer` type, since they have common functionality. Ensure all cases conform if so.
  Reference(Box<Type>),
  // TODO: Isn't this incompatible with `UserDefined`?
  Struct(StructType),
  /// A type that may need to be resolved.
  Stub(StubType),
  Callable(CallableType),
  Unit,
}

impl Type {
  pub fn is_unit(&self) -> bool {
    matches!(self, Type::Unit)
  }
}

// TODO: Write a macro that both defines this and `as_x_node()` (which alternatively yields `unreachable!()`) methods.
pub enum NodeKind {
  Literal(Literal),
  ExternFunction(ExternFunction),
  ExternStatic(ExternStatic),
  Function(Function),
  Block(Block),
  ReturnStmt(ReturnStmt),
  LetStmt(LetStmt),
  IfStmt(IfStmt),
  LoopStmt(LoopStmt),
  FunctionCall(CallExpr),
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
  TypeAlias(TypeAlias),
  Closure(Closure),
}

pub struct Node {
  pub kind: NodeKind,
  // FIXME: The visitation methods receive node kinds, but the spans are attached to the `Node` struct.
  pub span: diagnostic::Span,
}

pub struct Closure {
  pub captures: Vec<(String, Option<cache::DefinitionKey>)>,
  pub prototype: Prototype,
  pub body: Block,
}

#[derive(PartialEq, Clone)]
pub struct CallableType {
  pub return_type: Box<Type>,
  pub parameters: Vec<Type>,
  pub is_variadic: bool,
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

// TODO: Consider switching to `Display` trait.
impl ToString for Pattern {
  fn to_string(&self) -> String {
    // TODO: Missing the module name.
    // TODO: Hard-coded character.
    self.base_name.clone() + self.member_path.join(".").as_str()
  }
}

#[derive(PartialEq, Clone)]
pub struct StubType {
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

#[derive(Clone)]
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
  pub statements: Vec<Node>,
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
  pub ty: Option<Type>,
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

pub struct CallExpr {
  pub callee_expr: Box<Node>,
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

pub struct TypeAlias {
  pub name: String,
  pub ty: Type,
}

#[derive(PartialEq)]
pub enum OperatorKind {
  And,
  Or,
  Nand,
  Nor,
  Xor,
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
  Cast,
}

pub struct BinaryExpr {
  pub left: Box<Node>,
  pub right: Box<Node>,
  pub operator: OperatorKind,
}

pub struct UnaryExpr {
  pub expr: Box<Node>,
  pub operator: OperatorKind,
  /// Represents the type being casted to.
  ///
  /// Only available when the unary expression is a cast.
  pub cast_type: Option<Type>,
}

/// Represents an accessible definition. Acts as a transient
/// value helper.
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
