use crate::{cache, diagnostic, name_resolution};

#[macro_export]
macro_rules! dispatch {
  ($node:expr, $target_fn:expr $(, $($args:expr),* )? ) => {
    match $node {
      ast::NodeKind::Literal(inner) => $target_fn(inner $(, $($args),* )?),
      ast::NodeKind::ExternFunction(inner) => $target_fn(inner $(, $($args),* )?),
      ast::NodeKind::ExternStatic(inner) => $target_fn(inner $(, $($args),* )?),
      ast::NodeKind::Function(inner) => $target_fn(inner $(, $($args),* )?),
      ast::NodeKind::Block(inner) => $target_fn(inner $(, $($args),* )?),
      ast::NodeKind::ReturnStmt(inner) => $target_fn(inner $(, $($args),* )?),
      ast::NodeKind::LetStmt(inner) => $target_fn(inner $(, $($args),* )?),
      ast::NodeKind::IfStmt(inner) => $target_fn(inner $(, $($args),* )?),
      ast::NodeKind::LoopStmt(inner) => $target_fn(inner $(, $($args),* )?),
      ast::NodeKind::CallExpr(inner) => $target_fn(inner $(, $($args),* )?),
      ast::NodeKind::IntrinsicCall(inner) => $target_fn(inner $(, $($args),* )?),
      ast::NodeKind::BreakStmt(inner) => $target_fn(inner $(, $($args),* )?),
      ast::NodeKind::ContinueStmt(inner) => $target_fn(inner $(, $($args),* )?),
      ast::NodeKind::InlineExprStmt(inner) => $target_fn(inner $(, $($args),* )?),
      ast::NodeKind::Definition(inner) => $target_fn(inner $(, $($args),* )?),
      ast::NodeKind::Reference(inner) => $target_fn(inner $(, $($args),* )?),
      ast::NodeKind::AssignStmt(inner) => $target_fn(inner $(, $($args),* )?),
      ast::NodeKind::BinaryExpr(inner) => $target_fn(inner $(, $($args),* )?),
      ast::NodeKind::UnaryExpr(inner) => $target_fn(inner $(, $($args),* )?),
      ast::NodeKind::Parameter(inner) => $target_fn(inner $(, $($args),* )?),
      ast::NodeKind::UnsafeBlock(inner) => $target_fn(inner $(, $($args),* )?),
      ast::NodeKind::ArrayValue(inner) => $target_fn(inner $(, $($args),* )?),
      ast::NodeKind::ArrayIndexing(inner) => $target_fn(inner $(, $($args),* )?),
      ast::NodeKind::Enum(inner) => $target_fn(inner $(, $($args),* )?),
      ast::NodeKind::StructType(inner) => $target_fn(inner $(, $($args),* )?),
      ast::NodeKind::Prototype(inner) => $target_fn(inner $(, $($args),* )?),
      ast::NodeKind::StructValue(inner) => $target_fn(inner $(, $($args),* )?),
      ast::NodeKind::Pattern(inner) => $target_fn(inner $(, $($args),* )?),
      ast::NodeKind::TypeAlias(inner) => $target_fn(inner $(, $($args),* )?),
      ast::NodeKind::Closure(inner) => $target_fn(inner $(, $($args),* )?),
    }
  };
}

/// A parameter containing its name, type, and index position.
pub type Parameter = (String, Type, u32);

#[derive(PartialEq, PartialOrd, Clone, Debug)]
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

#[derive(PartialEq, Clone, Debug)]
pub enum PrimitiveType {
  Int(IntSize),
  Bool,
  Char,
  String,
  Null,
}

#[derive(PartialEq, Clone, Debug)]
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
#[derive(Debug)]
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
  CallExpr(CallExpr),
  IntrinsicCall(IntrinsicCall),
  BreakStmt(BreakStmt),
  ContinueStmt(ContinueStmt),
  InlineExprStmt(InlineExprStmt),
  Definition(Definition),
  Reference(Reference),
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

#[derive(Debug)]
pub struct Node {
  pub kind: NodeKind,
  // FIXME: The visitation methods receive node kinds, but the spans are attached to the `Node` struct.
  pub span: diagnostic::Span,
}

#[derive(Debug)]
pub struct Closure {
  pub captures: Vec<(String, Option<cache::UniqueId>)>,
  pub prototype: Prototype,
  pub body: Block,
}

#[derive(PartialEq, Clone, Debug)]
pub struct CallableType {
  pub return_type: Box<Type>,
  pub parameters: Vec<Type>,
  pub is_variadic: bool,
}

// TODO: If it's never boxed under `ast::Node`, then there might not be a need for it to be included under `ast::Node`?
#[derive(Debug)]
pub struct Pattern {
  pub module_name: Option<String>,
  pub base_name: String,
  /// A list of pairs containing both the name and index
  /// position of nested structs and their fields. These
  /// indexes will be filled out during name resolution.
  pub member_path: Vec<(String, Option<u32>)>,
  pub symbol_kind: name_resolution::SymbolKind,
  pub unique_id: Option<cache::UniqueId>,
}

impl Pattern {
  pub fn new(base_name: String, symbol_kind: name_resolution::SymbolKind) -> Self {
    Pattern {
      module_name: None,
      base_name,
      member_path: Vec::new(),
      symbol_kind,
      unique_id: None,
    }
  }
}

// TODO: Consider switching to `Display` trait.
impl ToString for Pattern {
  fn to_string(&self) -> String {
    // TODO: Missing the module name.
    // TODO: Hard-coded character.
    // TODO: Properly implement. Also, consider if this is even needed. If not, remove this trait implementation.
    // self.base_name.clone() + self.member_path.join(".").as_str()
    String::from("pending_implementation")
  }
}

#[derive(PartialEq, Clone, Debug)]
pub struct StubType {
  pub name: String,
  pub target_key: Option<cache::UniqueId>,
}

#[derive(Debug)]
pub struct StructValue {
  pub name: String,
  pub fields: Vec<Node>,
  /// A unique id targeting the struct value's type. Resolved
  /// during name resolution.
  pub target_key: Option<cache::UniqueId>,
}

#[derive(Debug)]
pub struct Enum {
  pub name: String,
  pub variants: Vec<String>,
}

#[derive(Debug)]
pub struct ContinueStmt;

#[derive(Debug)]
pub struct ArrayIndexing {
  pub name: String,
  pub index_expr: Box<Node>,
  pub target_key: Option<cache::UniqueId>,
}

#[derive(Debug)]
pub struct ArrayValue {
  pub elements: Vec<Node>,
  /// Holds the type of the array, in case it is an empty array.
  pub explicit_type: Option<Type>,
}

#[derive(Debug)]
pub struct UnsafeBlockStmt(pub Block);

#[derive(Debug)]
pub struct Reference(pub Pattern);

#[derive(Debug)]
pub struct AssignStmt {
  pub assignee_expr: Box<Node>,
  pub value: Box<Node>,
}

#[derive(Debug)]
pub enum Literal {
  Bool(bool),
  Int(u64, IntSize),
  Char(char),
  String(String),
  Nullptr,
}

#[derive(Clone, Debug, PartialEq)]
pub struct Prototype {
  pub parameters: Vec<Parameter>,
  pub return_type: Option<Type>,
  pub is_variadic: bool,
}

#[derive(Debug)]
pub struct ExternFunction {
  pub name: String,
  pub prototype: Prototype,
  pub attributes: Vec<Attribute>,
}

#[derive(Debug)]
pub struct ExternStatic(pub String, pub Type);

#[derive(Debug)]
pub struct Attribute {
  pub name: String,
  pub values: Vec<Literal>,
}

#[derive(Debug)]
pub struct Function {
  pub name: String,
  pub prototype: Prototype,
  pub body: Block,
  pub attributes: Vec<Attribute>,
}

#[derive(Debug)]
pub struct Block {
  pub statements: Vec<Node>,
  /// Whether the last expression is yielded by the block.
  pub yield_last_expr: bool,
  pub unique_id: cache::UniqueId,
}

#[derive(Debug)]
pub struct BreakStmt {
  //
}

#[derive(Debug)]
pub struct ReturnStmt {
  pub value: Option<Box<Node>>,
}

#[derive(Debug)]
pub struct LetStmt {
  pub name: String,
  pub ty: Option<Type>,
  pub value: Box<Node>,
  pub is_mutable: bool,
}

#[derive(Debug)]
pub struct IfStmt {
  pub condition: Box<Node>,
  pub then_block: Block,
  pub else_block: Option<Block>,
}

#[derive(Debug)]
pub struct LoopStmt {
  pub condition: Option<Box<Node>>,
  pub body: Block,
}

#[derive(Debug)]
pub struct InlineExprStmt {
  pub expr: Box<Node>,
}

#[derive(Debug)]
pub struct CallExpr {
  pub callee_expr: Box<Node>,
  pub arguments: Vec<Node>,
}

#[derive(Debug)]
pub enum IntrinsicKind {
  Panic,
}

#[derive(Debug)]
pub struct IntrinsicCall {
  pub kind: IntrinsicKind,
  pub arguments: Vec<Node>,
}

#[derive(PartialEq, Clone, Debug)]
pub struct StructType {
  pub name: String,
  pub fields: Vec<(String, Type)>,
}

#[derive(Debug)]
pub struct TypeAlias {
  pub name: String,
  pub ty: Type,
}

#[derive(PartialEq, Debug)]
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

#[derive(Debug)]
pub struct BinaryExpr {
  pub left: Box<Node>,
  pub right: Box<Node>,
  pub operator: OperatorKind,
}

#[derive(Debug)]
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
#[derive(Debug)]
pub struct Definition {
  pub symbol: Option<name_resolution::Symbol>,
  pub node_ref_cell: cache::CachedNode,
  pub unique_id: cache::UniqueId,
}

#[derive(Debug)]
pub struct MemberAccess {
  pub scope_qualifier: Pattern,
  pub target_key: Option<cache::UniqueId>,
}
