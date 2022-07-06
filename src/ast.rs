use std::collections::VecDeque;

use crate::{cache, name_resolution, type_system::Check, visitor};

#[macro_export]
macro_rules! force_match {
  ($e:expr, $t:path) => {
    match $e {
      $t(inner) => inner,
      _ => unreachable!(),
    }
  };
}

// TODO: We can simplify this with path type on the macro arguments.
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
      ast::NodeKind::BindingStmt(inner) => $target_fn(inner $(, $($args),* )?),
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
      ast::NodeKind::StaticArrayValue(inner) => $target_fn(inner $(, $($args),* )?),
      ast::NodeKind::IndexingExpr(inner) => $target_fn(inner $(, $($args),* )?),
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
      ast::NodeKind::Range(inner) => $target_fn(inner $(, $($args),* )?),
    }
  };
}

#[derive(Debug, Clone)]
pub enum GenericConstraintKind {
  Implements,
  Is,
}

#[derive(Debug, Clone)]
pub struct GenericConstraint {
  pub kind: GenericConstraintKind,
}

#[derive(Debug, Clone)]
pub struct Generics {
  pub parameters: Vec<String>,
  pub constraints: Option<Vec<GenericConstraint>>,
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
  Struct(StructType),
  /// A type that needs to be resolved.
  Stub(StubType),
  Function(FunctionType),
  This(ThisType),
  /// A type variable to be used during unification.
  Variable(usize),
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
  BindingStmt(BindingStmt),
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
  StaticArrayValue(StaticArrayValue),
  IndexingExpr(IndexingExpr),
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
  Import(Using),
  SizeofIntrinsic(SizeofIntrinsic),
  Range(Range),
}

impl NodeKind {
  // TODO: Can this be made a Rust Iterator? This way we get all of Iterator's features.
  pub fn traverse<'a>(&'a self, mut visitor: impl FnMut(&'a NodeKind) -> bool) {
    let map_children = |children: &'a Vec<Node>| children.iter().map(|child_node| &child_node.kind);

    let dispatcher = |node: &'a NodeKind| -> Vec<&NodeKind> {
      match node {
        NodeKind::InlineExprStmt(inline_expr_stmt) => vec![&inline_expr_stmt.expr.kind],
        NodeKind::AssignStmt(assign_stmt) => vec![&assign_stmt.assignee_expr.kind],
        NodeKind::BinaryExpr(binary_expr) => {
          vec![&binary_expr.left.kind, &binary_expr.right.kind]
        }
        NodeKind::BlockExpr(block_expr) => map_children(&block_expr.statements).collect(),
        NodeKind::UnaryExpr(unary_expr) => vec![&unary_expr.expr.kind],
        NodeKind::UnsafeExpr(unsafe_expr) => vec![&unsafe_expr.0.kind],
        NodeKind::ParenthesesExpr(parentheses_expr) => vec![&parentheses_expr.expr.kind],
        NodeKind::CallExpr(call_expr) => vec![&call_expr.callee_expr.kind]
          .into_iter()
          .chain(map_children(&call_expr.arguments))
          .collect(),
        // TODO: Include `else` expression, and alternative branches.
        NodeKind::IfExpr(if_expr) => vec![&if_expr.condition.kind, &if_expr.then_expr.kind],
        // TODO: Missing condition.
        NodeKind::LoopStmt(loop_stmt) => map_children(&loop_stmt.body.statements).collect(),
        // TODO: Implement all other nodes with visitable children.
        // REVIEW: Not all nodes can be processed like this: What about prototype, externs, and functions?
        _ => vec![],
      }
    };

    let mut queue = VecDeque::from([self]);

    while let Some(node) = queue.pop_front() {
      if !visitor(node) {
        return;
      }

      let children = dispatcher(node);

      queue.reserve(children.len());

      for child in children {
        queue.push_back(child);
      }
    }
  }

  /// Traverse the AST of the provided node until the provided
  /// predicate returns `true`, at which point the last node is
  /// returned.
  ///
  /// If the predicate returns `false` for all nodes, `None` is
  /// returned instead, indicating that no node was found matching the
  /// given predicate.
  ///
  /// The time complexity of this method is `O(n)`, where `n` is the number of
  /// nodes in the AST.
  pub fn find_node<'a>(
    &'a self,
    mut predicate: impl FnMut(&NodeKind) -> bool,
  ) -> Option<&NodeKind> {
    let mut result = None;

    self.traverse(|node| {
      if predicate(node) {
        result = Some(node);

        return false;
      }

      true
    });

    result
  }

  pub fn any(&self, mut predicate: impl FnMut(&NodeKind) -> bool) -> bool {
    let mut contained = false;

    self.traverse(|node| {
      if predicate(&node) {
        contained = true;

        return false;
      }

      true
    });

    contained
  }

  pub fn all(&self, predicate: impl FnMut(&NodeKind) -> bool) -> bool {
    !self.any(predicate)
  }

  pub fn is_constant_expr(&self) -> bool {
    let is_const_node = |node: &NodeKind| {
      matches!(
        node,
        NodeKind::Literal(_)
          | NodeKind::ParenthesesExpr(_)
          | NodeKind::UnaryExpr(_)
          | NodeKind::SizeofIntrinsic(_)
      )
    };

    self.all(is_const_node)
  }

  pub fn flatten<'a>(&'a self) -> &'a NodeKind {
    let mut buffer = self;

    // REVIEW: Anything else that may encapsulate a node? What about nested unary expressions?
    while let NodeKind::ParenthesesExpr(parentheses_expr) = buffer {
      buffer = &parentheses_expr.expr.kind;
    }

    buffer
  }
}

#[derive(Debug, Clone)]
pub struct Node {
  pub kind: NodeKind,
  // REVIEW: This might be problematic: What if the node was retrieved from the cache?
  pub cached_type: Option<Type>,
}

impl Node {
  // TODO: Since this mutates the instance, it cannot be used anywhere pretty much.
  pub fn mem_infer_type(&mut self, cache: &cache::Cache) -> Type {
    if let Some(cached_type) = &self.cached_type {
      return cached_type.clone();
    }

    let inferred_type = self.kind.infer_type(cache);

    self.cached_type = Some(inferred_type.clone());

    inferred_type
  }
}

#[derive(Debug, Clone)]
pub struct Range {
  pub start: Box<Node>,
  pub end: Box<Node>,
}

#[derive(Debug, Clone)]
pub struct Using {
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
}

// FIXME: This will no longer have the `member_path` field. It will be replaced by the implementation of `MemberAccess`.
// TODO: If it's never boxed under `ast::Node`, then there might not be a need for it to be included under `ast::Node`?
#[derive(Debug, Clone, PartialEq)]
pub struct Pattern {
  pub qualifier: Option<name_resolution::Qualifier>,
  pub base_name: String,
  pub sub_name: Option<String>,
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
  pub member_methods: Vec<Function>,
  pub static_methods: Vec<Function>,
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
  pub variants: Vec<(String, cache::BindingId)>,
  pub binding_id: cache::BindingId,
  pub ty: BasicType,
}

#[derive(Debug, Clone)]
pub struct ContinueStmt;

#[derive(Debug, Clone)]
pub struct IndexingExpr {
  pub name: String,
  pub index_expr: Box<Node>,
  pub target_id: Option<cache::BindingId>,
}

#[derive(Debug, Clone)]
pub struct StaticArrayValue {
  pub elements: Vec<Node>,
  /// Holds the type of the array, in case it is an empty array.
  pub explicit_type: Option<Type>,
}

#[derive(Debug, Clone)]
pub struct UnsafeExpr(pub Box<Node>);

#[derive(Debug, Clone)]
pub struct Reference {
  pub pattern: Pattern,
  // REVIEW: Why not have the reference have a `Rc<>` to the target? This would remove dependence
  // ... on the cache, and would be filled during name resolution. We should note that the cache
  // ... isn't available during the `resolve()` name resolution step (that's not a such a big problem,
  // ... however). The question is, where would these `Rc<>`s be sourced from? (they must consume `T`).
  // REVIEW: What about having an auxiliary mapping from `UniqueId` to `Type`?
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
  pub return_type_annotation: Type,
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

// TODO: Change this into a macro.
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
  pub static_owner_name: Option<String>,
  pub name: String,
  pub prototype: Prototype,
  pub body: Box<BlockExpr>,
  pub attributes: Vec<Attribute>,
  pub binding_id: cache::BindingId,
  pub generics: Option<Generics>,
}

#[derive(Debug, Clone)]
pub struct BlockExpr {
  pub statements: Vec<Node>,
  pub yields: Option<Box<Node>>,
  pub binding_id: cache::BindingId,
}

#[derive(Debug, Clone)]
pub struct BreakStmt;

#[derive(Debug, Clone)]
pub struct ReturnStmt {
  pub value: Option<Box<Node>>,
}

#[derive(Debug, Clone, PartialEq)]
pub enum BindingModifier {
  ConstExpr,
  Immutable,
  Mutable,
}

#[derive(Debug, Clone)]
pub struct BindingStmt {
  pub name: String,
  pub value: Box<Node>,
  pub modifier: BindingModifier,
  pub binding_id: cache::BindingId,
  pub ty: Type,
}

#[derive(Debug, Clone)]
pub struct IfExpr {
  pub condition: Box<Node>,
  pub then_expr: Box<Node>,
  pub alternative_branches: Vec<(Node, Node)>,
  pub else_expr: Option<Box<Node>>,
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

#[cfg(test)]
mod tests {
  use super::*;

  #[test]
  fn traverse_ast() {
    let node = NodeKind::BlockExpr(BlockExpr {
      binding_id: 0,
      statements: Vec::new(),
      yields: None,
    });

    let mut visitations = 0;

    node.traverse(|_| {
      visitations += 1;

      true
    });

    assert_eq!(1, visitations);
  }

  #[test]
  fn find_node_in_ast() {
    let target_node = NodeKind::BreakStmt(BreakStmt);

    let block = NodeKind::BlockExpr(BlockExpr {
      binding_id: 0,
      statements: vec![Node {
        cached_type: None,
        kind: target_node.clone(),
      }],
      yields: None,
    });

    let search_result = block.find_node(|node| matches!(node, NodeKind::BreakStmt(_)));

    assert!(matches!(search_result, Some(NodeKind::BreakStmt(_))));
  }
}
