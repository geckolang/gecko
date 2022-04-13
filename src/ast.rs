use crate::{cache, diagnostic, name_resolution, visitor};

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
      ast::NodeKind::IfExpr(inner) => $target_fn(inner $(, $($args),* )?),
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
      ast::NodeKind::UnsafeExpr(inner) => $target_fn(inner $(, $($args),* )?),
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
      ast::NodeKind::ParenthesesExpr(inner) => $target_fn(inner $(, $($args),* )?),
      ast::NodeKind::Import(inner) => $target_fn(inner $(, $($args),* )?),
      ast::NodeKind::SizeofIntrinsic(inner) => $target_fn(inner $(, $($args),* )?),
    }
  };
}

#[derive(Debug, Clone)]
pub struct ParenthesesExpr {
  pub expr: Box<Node>,
}

#[derive(Clone, Debug, PartialEq)]
pub struct Parameter {
  pub name: String,
  pub ty: Type,
  pub position: u32,
  pub binding_id: cache::BindingId,
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
  Reference(Box<Type>),
  // TODO: Isn't this incompatible with `StubType`?
  Struct(StructType),
  /// A type that needs to be resolved.
  Stub(StubType),
  Function(FunctionType),
  This(ThisType),
  Unit,
  // REVIEW: Is this actually needed? It's only used in the infer methods, but doesn't that mean that there's simply a hole in our type-checking?
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
  IfExpr(IfExpr),
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
  UnsafeExpr(UnsafeExpr),
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
  ParenthesesExpr(ParenthesesExpr),
  Import(Import),
  SizeofIntrinsic(SizeofIntrinsic),
}

#[derive(Debug, Clone)]
pub struct Node {
  pub kind: NodeKind,
  // FIXME: The visitation methods receive node kinds, but the spans are attached to the `Node` struct.
  pub span: diagnostic::Span,
}

#[derive(Debug, Clone)]
pub struct Import {
  pub package_name: String,
  pub module_name: String,
}

#[derive(Debug, Clone)]
pub struct Closure {
  pub captures: Vec<(String, Option<cache::BindingId>)>,
  pub prototype: Prototype,
  pub body: BlockExpr,
}

#[derive(PartialEq, Clone, Debug)]
pub struct FunctionType {
  pub return_type: Box<Type>,
  pub parameter_types: Vec<Type>,
  pub is_variadic: bool,
  pub is_extern: bool,
}

#[derive(PartialEq, Clone, Debug)]
pub struct ThisType {
  pub target_id: Option<cache::BindingId>,
  pub ty: Option<Box<Type>>,
}

// FIXME: This will no longer have the `member_path` field. It will be replaced by the implementation of `MemberAccess`.
// TODO: If it's never boxed under `ast::Node`, then there might not be a need for it to be included under `ast::Node`?
#[derive(Debug, Clone, PartialEq)]
pub struct Pattern {
  pub global_qualifier: Option<name_resolution::GlobalQualifier>,
  pub base_name: String,
  pub symbol_kind: name_resolution::SymbolKind,
  pub target_id: Option<cache::BindingId>,
}

#[derive(PartialEq, Clone, Debug)]
pub struct StubType {
  pub pattern: Pattern,
}

#[derive(Debug, Clone)]
pub struct StructValue {
  pub struct_name: String,
  pub fields: Vec<Node>,
  /// A unique id targeting the struct value's type. Resolved
  /// during name resolution.
  pub target_id: Option<cache::BindingId>,
  pub ty: Option<Type>,
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
  pub binding_id: cache::BindingId,
}

#[derive(Debug, Clone)]
pub struct Enum {
  pub name: String,
  pub variants: Vec<String>,
  pub binding_id: cache::BindingId,
}

#[derive(Debug, Clone)]
pub struct ContinueStmt;

#[derive(Debug, Clone)]
pub struct ArrayIndexing {
  pub name: String,
  pub index_expr: Box<Node>,
  pub target_id: Option<cache::BindingId>,
}

#[derive(Debug, Clone)]
pub struct ArrayValue {
  pub elements: Vec<Node>,
  /// Holds the type of the array, in case it is an empty array.
  pub explicit_type: Option<Type>,
}

#[derive(Debug, Clone)]
pub struct UnsafeExpr(pub Box<Node>);

#[derive(Debug, Clone)]
pub struct Reference {
  pub pattern: Pattern,
  // TODO: Why not have the reference have a `Rc<>` to the target? This would remove dependence
  // ... on the cache, and would be filled during name resolution. We should note that the cache
  // ... isn't available during the `resolve()` name resolution step (that's not a such a big problem,
  // ... however). The question is, where would these `Rc<>`s be sourced from? (they must consume `T`).
  // TODO: What about having an auxiliary mapping from `UniqueId` to `Type`?
}

#[derive(Debug, Clone)]
pub struct AssignStmt {
  pub assignee_expr: Box<Node>,
  pub value: Box<Node>,
}

#[derive(Debug, Clone)]
pub struct SizeofIntrinsic {
  pub ty: Type,
}

#[derive(Debug, Clone)]
pub enum Literal {
  Bool(bool),
  Int(u64, IntSize),
  Char(char),
  String(String),
  Nullptr(Type),
}

#[derive(Debug, Clone)]
pub struct Prototype {
  pub parameters: Vec<Parameter>,
  pub return_type: Type,
  pub is_variadic: bool,
  pub is_extern: bool,
  pub accepts_instance: bool,
  pub instance_type_id: Option<cache::BindingId>,
  pub this_parameter: Option<Parameter>,
}

impl visitor::Visitable for Prototype {
  fn accept<T>(&mut self, visitor: &mut impl visitor::Visitor<T>) -> T {
    visitor.visit_prototype(self)
  }
}

#[derive(Debug, Clone)]
pub struct ExternFunction {
  pub name: String,
  pub prototype: Prototype,
  pub attributes: Vec<Attribute>,
  pub binding_id: cache::BindingId,
}

impl visitor::Visitable for ExternFunction {
  fn accept<T>(&mut self, visitor: &mut impl visitor::Visitor<T>) -> T {
    visitor.visit_extern_function(self)
  }
}

#[derive(Debug, Clone)]
pub struct ExternStatic {
  pub name: String,
  pub ty: Type,
  pub binding_id: cache::BindingId,
}

impl visitor::Visitable for ExternStatic {
  fn accept<T>(&mut self, visitor: &mut impl visitor::Visitor<T>) -> T {
    visitor.visit_extern_static(self)
  }
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
  pub binding_id: cache::BindingId,
}

#[derive(Debug, Clone)]
pub struct BlockExpr {
  pub statements: Vec<Node>,
  pub yields_last_expr: bool,
  pub binding_id: cache::BindingId,
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
  pub ty: Type,
  pub value: Box<Node>,
  pub is_mutable: bool,
  pub binding_id: cache::BindingId,
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
  pub binding_id: cache::BindingId,
  pub name: String,
  pub fields: Vec<(String, Type)>,
}

#[derive(Debug, Clone)]
pub struct TypeAlias {
  pub name: String,
  pub ty: Type,
  pub binding_id: cache::BindingId,
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
