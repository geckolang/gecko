use crate::{cache, diagnostic, name_resolution};

#[macro_export]
macro_rules! dispatch {
  ($node:expr, $target_fn:expr $(, $($args:expr),* )? ) => {
    match $node {
      ast::NodeKind::Literal(inner) => $target_fn(inner $(, $($args),* )?),
      ast::NodeKind::ExternFunction(inner) => $target_fn(inner $(, $($args),* )?),
      ast::NodeKind::ExternStatic(inner) => $target_fn(inner $(, $($args),* )?),
      ast::NodeKind::Function(inner) => $target_fn(inner $(, $($args),* )?),
      ast::NodeKind::BlockExpr(inner) => $target_fn(inner $(, $($args),* )?),
      ast::NodeKind::ReturnStmt(inner) => $target_fn(inner $(, $($args),* )?),
      ast::NodeKind::LetStmt(inner) => $target_fn(inner $(, $($args),* )?),
      ast::NodeKind::IfStmt(inner) => $target_fn(inner $(, $($args),* )?),
      ast::NodeKind::LoopStmt(inner) => $target_fn(inner $(, $($args),* )?),
      ast::NodeKind::CallExpr(inner) => $target_fn(inner $(, $($args),* )?),
      ast::NodeKind::IntrinsicCall(inner) => $target_fn(inner $(, $($args),* )?),
      ast::NodeKind::BreakStmt(inner) => $target_fn(inner $(, $($args),* )?),
      ast::NodeKind::ContinueStmt(inner) => $target_fn(inner $(, $($args),* )?),
      ast::NodeKind::InlineExprStmt(inner) => $target_fn(inner $(, $($args),* )?),
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
      ast::NodeKind::MemberAccess(inner) => $target_fn(inner $(, $($args),* )?),
      ast::NodeKind::StructImpl(inner) => $target_fn(inner $(, $($args),* )?),
      ast::NodeKind::Trait(inner) => $target_fn(inner $(, $($args),* )?),
    }
  };
}

/// A parameter containing its name, type, and index position.
#[derive(Clone, Debug, PartialEq)]
pub struct Parameter {
  pub name: String,
  pub ty: Type,
  pub position: u32,
  // FIXME: [!!] Bug: Parameters are not registered on the cache, because they aren't top-level nodes.
  pub unique_id: cache::UniqueId,
}

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
pub enum BasicType {
  Int(IntSize),
  Bool,
  Char,
  String,
  Null,
}

#[derive(PartialEq, Clone, Debug)]
pub enum Type {
  Array(Box<Type>, u32),
  Basic(BasicType),
  Pointer(Box<Type>),
  // TODO: Consider merging with `Pointer` type, since they have common functionality. Ensure all cases conform if so.
  Reference(Box<Type>),
  // TODO: Isn't this incompatible with `UserDefined`?
  Struct(StructType),
  /// A type that may need to be resolved.
  Stub(StubType),
  Function(FunctionType),
  This(ThisType),
  Unit,
  // FIXME: [!!] Investigate: Is this actually needed? It's only used in the infer methods, but doesn't that mean that there's simply a hole in our type-checking?
  Error,
}

impl Type {
  pub fn is_unit(&self) -> bool {
    matches!(self, Type::Unit)
  }

  pub fn is_stub(&self) -> bool {
    matches!(self, Type::Stub(_))
  }
}

// TODO: Write a macro that both defines this and `as_x_node()` (which alternatively yields `unreachable!()`) methods.
#[derive(Debug, Clone)]
pub enum NodeKind {
  Literal(Literal),
  ExternFunction(ExternFunction),
  ExternStatic(ExternStatic),
  Function(Function),
  BlockExpr(BlockExpr),
  ReturnStmt(ReturnStmt),
  LetStmt(LetStmt),
  IfStmt(IfExpr),
  LoopStmt(LoopStmt),
  CallExpr(CallExpr),
  IntrinsicCall(IntrinsicCall),
  BreakStmt(BreakStmt),
  ContinueStmt(ContinueStmt),
  InlineExprStmt(InlineExprStmt),
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
  MemberAccess(MemberAccess),
  StructImpl(StructImpl),
  Trait(Trait),
}

#[derive(Debug, Clone)]
pub struct Node {
  pub kind: NodeKind,
  // FIXME: The visitation methods receive node kinds, but the spans are attached to the `Node` struct.
  pub span: diagnostic::Span,
}

#[derive(Debug, Clone)]
pub struct Closure {
  pub captures: Vec<(String, Option<cache::UniqueId>)>,
  pub prototype: Prototype,
  pub body: BlockExpr,
}

#[derive(PartialEq, Clone, Debug)]
pub struct FunctionType {
  pub return_type: Box<Type>,
  pub parameter_types: Vec<Type>,
  pub is_variadic: bool,
}

#[derive(PartialEq, Clone, Debug)]
pub struct ThisType {
  pub target_id: Option<cache::UniqueId>,
  pub ty: Option<Box<Type>>,
}

// FIXME: This will no longer have the `member_path` field. It will be replaced by the implementation of `MemberAccess`.
// TODO: If it's never boxed under `ast::Node`, then there might not be a need for it to be included under `ast::Node`?
#[derive(Debug, Clone)]
pub struct Pattern {
  pub module_name: Option<String>,
  pub base_name: String,
  pub symbol_kind: name_resolution::SymbolKind,
  pub target_id: Option<cache::UniqueId>,
}

#[derive(PartialEq, Clone, Debug)]
pub struct StubType {
  pub name: String,
  pub target_id: Option<cache::UniqueId>,
}

#[derive(Debug, Clone)]
pub struct StructValue {
  pub struct_name: String,
  pub fields: Vec<Node>,
  /// A unique id targeting the struct value's type. Resolved
  /// during name resolution.
  pub target_id: Option<cache::UniqueId>,
}

#[derive(Debug, Clone)]
pub struct StructImpl {
  pub is_default: bool,
  pub target_struct_pattern: Pattern,
  pub trait_pattern: Option<Pattern>,
  pub methods: Vec<Function>,
}

#[derive(Debug, Clone)]
pub struct Trait {
  pub name: String,
  pub methods: Vec<(String, Prototype)>,
  pub unique_id: cache::UniqueId,
}

#[derive(Debug, Clone)]
pub struct Enum {
  pub name: String,
  pub variants: Vec<String>,
  pub unique_id: cache::UniqueId,
}

#[derive(Debug, Clone)]
pub struct ContinueStmt;

#[derive(Debug, Clone)]
pub struct ArrayIndexing {
  pub name: String,
  pub index_expr: Box<Node>,
  pub target_id: Option<cache::UniqueId>,
}

#[derive(Debug, Clone)]
pub struct ArrayValue {
  pub elements: Vec<Node>,
  /// Holds the type of the array, in case it is an empty array.
  pub explicit_type: Option<Type>,
}

#[derive(Debug, Clone)]
pub struct UnsafeBlockStmt(pub BlockExpr);

#[derive(Debug, Clone)]
pub struct Reference(pub Pattern);

#[derive(Debug, Clone)]
pub struct AssignStmt {
  pub assignee_expr: Box<Node>,
  pub value: Box<Node>,
}

#[derive(Debug, Clone)]
pub enum Literal {
  Bool(bool),
  Int(u64, IntSize),
  Char(char),
  String(String),
  Nullptr,
}

#[derive(Debug, Clone)]
pub struct Prototype {
  pub parameters: Vec<Parameter>,
  pub return_type: Type,
  pub is_variadic: bool,
  pub accepts_instance: bool,
  pub instance_type_id: Option<cache::UniqueId>,
  pub this_parameter: Option<Parameter>,
}

#[derive(Debug, Clone)]
pub struct ExternFunction {
  pub name: String,
  pub prototype: Prototype,
  pub attributes: Vec<Attribute>,
  pub unique_id: cache::UniqueId,
}

#[derive(Debug, Clone)]
pub struct ExternStatic {
  pub name: String,
  pub ty: Type,
  pub unique_id: cache::UniqueId,
}

#[derive(Debug, Clone)]
pub struct Attribute {
  pub name: String,
  pub values: Vec<Literal>,
}

#[derive(Debug, Clone)]
pub struct Function {
  pub name: String,
  pub prototype: Prototype,
  pub body_value: Box<Node>,
  pub attributes: Vec<Attribute>,
  pub unique_id: cache::UniqueId,
}

#[derive(Debug, Clone)]
pub struct BlockExpr {
  pub statements: Vec<Node>,
  pub yields_last_expr: bool,
  pub unique_id: cache::UniqueId,
}

#[derive(Debug, Clone)]
pub struct BreakStmt;

#[derive(Debug, Clone)]
pub struct ReturnStmt {
  pub value: Option<Box<Node>>,
}

#[derive(Debug, Clone)]
pub struct LetStmt {
  pub name: String,
  // TODO: Since there's no use for explicit type, simply assume its always inferred. Same for prototype return types.
  pub ty: Type,
  pub value: Box<Node>,
  pub is_mutable: bool,
  // FIXME: [!!] Bug: Let statements are not registered on the cache, because they are not top-level nodes.
  pub unique_id: cache::UniqueId,
}

#[derive(Debug, Clone)]
pub struct IfExpr {
  pub condition: Box<Node>,
  pub then_value: Box<Node>,
  pub else_value: Option<Box<Node>>,
}

#[derive(Debug, Clone)]
pub struct LoopStmt {
  pub condition: Option<Box<Node>>,
  pub body: BlockExpr,
}

#[derive(Debug, Clone)]
pub struct InlineExprStmt {
  pub expr: Box<Node>,
}

#[derive(Debug, Clone)]
pub struct CallExpr {
  pub callee_expr: Box<Node>,
  pub arguments: Vec<Node>,
}

#[derive(Debug, Clone)]
pub enum IntrinsicKind {
  Panic,
}

#[derive(Debug, Clone)]
pub struct IntrinsicCall {
  pub kind: IntrinsicKind,
  pub arguments: Vec<Node>,
}

#[derive(PartialEq, Clone, Debug)]
pub struct StructType {
  pub unique_id: cache::UniqueId,
  pub name: String,
  pub fields: Vec<(String, Type)>,
}

#[derive(Debug, Clone)]
pub struct TypeAlias {
  pub name: String,
  pub ty: Type,
  pub unique_id: cache::UniqueId,
}

#[derive(PartialEq, Debug, Clone)]
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

#[derive(Debug, Clone)]
pub struct BinaryExpr {
  pub left: Box<Node>,
  pub right: Box<Node>,
  pub operator: OperatorKind,
}

#[derive(Debug, Clone)]
pub struct UnaryExpr {
  pub expr: Box<Node>,
  pub operator: OperatorKind,
  /// Represents the type being casted to.
  ///
  /// Only available when the unary expression is a cast.
  pub cast_type: Option<Type>,
}

#[derive(Debug, Clone)]
pub struct MemberAccess {
  pub base_expr: Box<Node>,
  pub member_name: String,
}
