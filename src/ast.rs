use crate::{cache, name_resolution, type_system::Check};

#[macro_export]
macro_rules! force_match {
  ($subject:expr, $path:path) => {
    match $subject {
      $path(inner) => inner,
      _ => unreachable!(),
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
pub struct ParenthesesExpr(pub std::rc::Rc<NodeKind>);

#[derive(Clone, Debug, PartialEq)]
pub struct Parameter {
  pub name: String,
  pub type_hint: Option<std::rc::Rc<Type>>,
  pub position: u32,
  pub id: cache::Id,
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
  /// A static array type.
  ///
  /// Its type and length are always known at compile-time.
  Array(std::rc::Rc<Type>, u32),
  Basic(BasicType),
  Pointer(std::rc::Rc<Type>),
  Reference(std::rc::Rc<Type>),
  Struct(Struct),
  /// A type that needs to be resolved.
  Stub(StubType),
  Function(FunctionType),
  This(ThisType),
  /// A meta type to be used during unification.
  Variable(usize),
  /// A super-type of all integer types.
  MetaInteger,
  /// A meta type that represents the lack of a value.
  Unit,
  // TODO: To implement sub-typing, we may just need to create/extend a generalized compare function, where super-types bind with subtypes?
  /// A meta type that implies a computation that will
  /// never evaluate to a value.
  ///
  /// This type is a subtype of all other types.
  Never,
  /// A meta type that coerces to any type.
  ///
  /// To be used exclusively during type-checking for comparisons;
  /// meta types may not be lowered.
  Any,
}

impl Type {
  /// Determine whether the type is a unit type.
  ///
  /// This determination will not perform flattening.
  pub fn is_a_unit(&self) -> bool {
    matches!(self, Type::Unit)
  }

  /// Determine whether the type is a never type.
  ///
  /// This determination will not perform flattening.
  pub fn is_a_never(&self) -> bool {
    matches!(self, Type::Never)
  }

  // TODO: Clarify comment.
  /// Determine whether the type is a meta type, implying that
  /// it is not lowerable.
  ///
  /// The result of this computation also indicates whether
  /// this type can be lowered or not.
  ///
  /// In the case of the unit type, during lowering this may
  /// indirectly lower to LLVM's `void` type if type is a return
  /// type, and under certain conditions.
  ///
  /// This determination will not perform flattening.
  pub fn is_a_meta(&self) -> bool {
    self.is_a_unit() || self.is_a_never() || matches!(self, Type::Any)
  }

  /// Determine whether the type is a stub type.
  ///
  /// This determination will not perform flattening.
  pub fn is_a_stub(&self) -> bool {
    matches!(self, Type::Stub(_))
  }

  // REVIEW: Consider moving this to be part of `Type` itself.
  /// Determine whether the type is a null pointer type.
  ///
  /// This determination will not perform flattening.
  fn is_a_null_pointer_type(&self) -> bool {
    if let Type::Pointer(ty) = self {
      return matches!(ty.as_ref(), Type::Basic(BasicType::Null));
    }

    false
  }

  pub fn flat_is(&self, other: &Type, cache: &cache::Cache) -> bool {
    self.flatten(cache).is(&other.flatten(cache))
  }

  // FIXME: Every type comparison should be using this function.
  /// Compare two types for compatibility.
  ///
  /// If one of the types is a subtype or supertype of another, this will
  /// return `true`. This determination will not perform flattening.
  pub fn is(&self, other: &Type) -> bool {
    // The never type is a supertype of everything.
    if matches!(self, Type::Never) || matches!(other, Type::Never) {
      return true;
    }
    // At this point, any any type is a supertype of any other type.
    else if matches!(self, Type::Any) || matches!(other, Type::Any) {
      return true;
    }
    // If both types are pointers, and at least one is a null pointer type, then always coerce.
    // This is because null pointers coerce into any pointer type (any pointer can be null).
    else if matches!(self, Type::Pointer(_))
      && matches!(other, Type::Pointer(_))
      && (self.is_a_null_pointer_type() || other.is_a_null_pointer_type())
    {
      return true;
    }

    // BUG: Is this actually true? What if we compare a Stub type with a Basic type (defined by the user)?
    // NOTE: Stub types will also work, because their target ids will be compared.
    self == other
  }

  // FIXME: Ensure this logic is correct.
  /// Determine the type that takes precedence in a comparison.
  ///
  /// This can be used to determine which type takes precedence in a coercion.
  /// This determination will not perform flattening.
  pub fn coercion(&self, other: &Type) -> Option<Type> {
    // If both types are the same, simply return.
    if self == other {
      return Some(self.clone());
    }
    // Unit type takes precedence over everything.
    else if self.is_a_unit() || other.is_a_unit() {
      return Some(Type::Unit);
    }
    // If exactly one type is a never, the other type takes precedence.
    // This is because the never type can be coerced into anything except
    // unit, implies it is a supertype of everything except unit.
    else if self.is_a_never() && !other.is_a_never() {
      return Some(other.clone());
    } else if !self.is_a_never() && other.is_a_never() {
      return Some(self.clone());
    }

    // Otherwise, the types are incompatible.
    None
  }

  // FIXME: Need to handle cyclic types. Currently, stack is overflown. One example would be cyclic type aliases.
  // REVIEW: Consider making this function recursive (in the case that the user-defined type points to another user-defined type).
  /// Resolve a possible user-defined type, so it can be used properly.
  ///
  /// Should be used when the type is to be compared.
  pub fn flatten(&self, cache: &cache::Cache) -> Type {
    // REVISE: Cleanup.

    // REVIEW: What if it's a pointer to a user-defined type?
    if let Type::Stub(stub_type) = self {
      let target_node = cache.force_get(&stub_type.pattern.id);

      // REVIEW: What about type aliases, and other types that might be encountered in the future?

      // REVISE: Cleanup!
      if let NodeKind::TypeAlias(type_alias) = &target_node {
        return type_alias.ty.flatten(cache);
      } else if let NodeKind::Struct(target_type) = &target_node {
        // REVIEW: Why is `flatten_type` being called again with a struct type inside?
        return Type::Struct(target_type.clone()).flatten(cache);
      }
    } else if let Type::This(this_type) = &self {
      // REVISE: No need to clone?
      let target_struct_type = cache.force_get(&this_type.target_id.unwrap());

      if let NodeKind::Struct(struct_type) = &target_struct_type {
        return Type::Struct(struct_type.clone());
      }
    }

    // REVISE: Do not clone by default. Find a better alternative.
    self.clone()
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
  CallExpr(CallExpr),
  IntrinsicCall(IntrinsicCall),
  InlineExprStmt(InlineExprStmt),
  Reference(Reference),
  BinaryExpr(BinaryExpr),
  UnaryExpr(UnaryExpr),
  Parameter(Parameter),
  UnsafeExpr(UnsafeExpr),
  StaticArrayValue(StaticArrayValue),
  IndexingExpr(IndexingExpr),
  Enum(Enum),
  Struct(Struct),
  Signature(Signature),
  StructValue(StructValue),
  Pattern(Pattern),
  TypeAlias(TypeAlias),
  Closure(Closure),
  MemberAccess(MemberAccess),
  StructImpl(StructImpl),
  Trait(Trait),
  ParenthesesExpr(ParenthesesExpr),
  Using(Using),
  SizeofIntrinsic(SizeofIntrinsic),
  Range(Range),
  Type(Type),
}

impl NodeKind {
  // TODO: Can this be made a Rust Iterator? This way we get all of Iterator's features.
  pub fn traverse<'a>(&'a self, mut visitor: impl FnMut(&'a NodeKind) -> bool) {
    todo!();
    // let map_children = |children: &'a Vec<Node>| children.iter().map(|child_node| &child_node);

    // // TODO: signature.
    // // let map_signature = |signature: &'a signature| map_children(&signature.parameters);

    // let dispatcher = |node: &'a NodeKind| -> Vec<&NodeKind> {
    //   match node {
    //     NodeKind::InlineExprStmt(inline_expr_stmt) => vec![&inline_expr_stmt.expr],
    //     NodeKind::BinaryExpr(binary_expr) => {
    //       vec![&binary_expr.left, &binary_expr.right]
    //     }
    //     NodeKind::BlockExpr(block_expr) => map_children(block_expr.statements).collect(),
    //     NodeKind::UnaryExpr(unary_expr) => vec![&unary_expr.expr],
    //     NodeKind::UnsafeExpr(unsafe_expr) => vec![&unsafe_expr.0],
    //     NodeKind::ParenthesesExpr(parentheses_expr) => vec![&parentheses_expr.0],
    //     NodeKind::CallExpr(call_expr) => vec![&call_expr.callee_expr]
    //       .into_iter()
    //       .chain(map_children(call_expr.arguments))
    //       .collect(),
    //     // TODO: Include `else` expression, and alternative branches.
    //     NodeKind::IfExpr(if_expr) => vec![&if_expr.condition, &if_expr.then_value],
    //     // TODO: Missing signature.
    //     // NodeKind::Closure(closure) => map_children(&closure.body.statements).collect(),
    //     // TODO: Missing signature.
    //     NodeKind::Function(function) => map_children(function.body.statements).collect(),
    //     NodeKind::BindingStmt(binding_stmt) => vec![&binding_stmt.value],
    //     NodeKind::ReturnStmt(ReturnStmt { value: Some(value) }) => vec![&value],
    //     NodeKind::IndexingExpr(indexing_expr) => {
    //       vec![&indexing_expr.index_expr]
    //     }
    //     NodeKind::IntrinsicCall(intrinsic_call) => {
    //       map_children(intrinsic_call.arguments).collect()
    //     }
    //     NodeKind::MemberAccess(member_access) => vec![&member_access.base_expr],
    //     NodeKind::Range(range) => vec![&range.start, &range.end],
    //     NodeKind::StaticArrayValue(static_array_value) => {
    //       map_children(static_array_value.elements).collect()
    //     }
    //     // NodeKind::StructImpl(struct_impl) => {
    //     //   vec![struct_impl.static_methods, struct_impl.member_methods].into_iter().flatten().collect::<Vec<_>>()
    //     // }
    //     NodeKind::StructValue(struct_value) => map_children(struct_value.fields).collect(),
    //     // NodeKind::Trait(trait_) => {
    //     //   map_children(&trait_.methods).collect()
    //     // }
    //     // REVIEW: Not all nodes can be processed like this: What about signatures?
    //     _ => vec![],
    //   }
    // };

    // let mut queue = VecDeque::from([self]);

    // while let Some(node) = queue.pop_front() {
    //   if !visitor(node) {
    //     return;
    //   }

    //   let children = dispatcher(node);

    //   queue.reserve(children.len());

    //   for child in children {
    //     queue.push_back(child);
    //   }
    // }
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
    let mut result = false;

    self.traverse(|node| {
      if predicate(&node) {
        result = true;

        return false;
      }

      true
    });

    result
  }

  pub fn all(&self, mut predicate: impl FnMut(&NodeKind) -> bool) -> bool {
    !self.any(|node| !predicate(node))
  }

  pub fn is_constant_expr(&self) -> bool {
    let is_const_node = |node: &NodeKind| {
      if let NodeKind::BindingStmt(binding_stmt) = node {
        return binding_stmt.is_const_expr;
      }

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

  // REVIEW: Ensure this is tail-recursive.
  pub fn flatten<'a>(&'a self) -> &'a NodeKind {
    if let NodeKind::ParenthesesExpr(parentheses_expr) = self {
      return &parentheses_expr.0.flatten();
    }

    self
  }

  /// Infer and attempt to flatten this node's type.
  ///
  /// Should be used when the type is to be compared.
  pub fn infer_flatten_type(&self, cache: &cache::Cache) -> Type {
    self.infer_type(cache).flatten(cache)
  }
}

#[derive(Debug, Clone)]
pub struct Node {
  pub kind: NodeKind,
  // TODO: In the future, we may be able to use node location as its id.
  pub id: cache::Id,
  pub location: (usize, usize),
}

#[derive(Debug, Clone)]
pub struct Range {
  pub start: std::rc::Rc<Literal>,
  pub end: std::rc::Rc<Literal>,
}

#[derive(Debug, Clone)]
pub struct Using {
  pub package_name: String,
  pub module_name: String,
}

#[derive(Debug, Clone)]
pub struct Closure {
  pub captures: Vec<(String, Option<cache::Id>)>,
  pub signature: Signature,
  pub body: std::rc::Rc<BlockExpr>,
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
  pub target_id: Option<cache::Id>,
}

// FIXME: This will no longer have the `member_path` field. It will be replaced by the implementation of `MemberAccess`.
// TODO: If it's never boxed under `Node`, then there might not be a need for it to be included under `Node`?
#[derive(Debug, Clone, PartialEq)]
pub struct Pattern {
  pub id: cache::Id,
  pub qualifier: Option<name_resolution::Qualifier>,
  pub base_name: String,
  pub sub_name: Option<String>,
  pub symbol_kind: name_resolution::SymbolKind,
}

#[derive(PartialEq, Clone, Debug)]
pub struct StubType {
  pub pattern: Pattern,
}

#[derive(Debug, Clone)]
pub struct StructValue {
  pub struct_name: String,
  pub fields: Vec<std::rc::Rc<NodeKind>>,
  /// A unique id targeting the struct value's type. Resolved
  /// during name resolution.
  pub target_id: cache::Id,
  pub ty: Option<Type>,
}

#[derive(Debug, Clone)]
pub struct StructImpl {
  pub is_default: bool,
  pub target_struct_pattern: Pattern,
  pub trait_pattern: Option<Pattern>,
  pub member_methods: Vec<std::rc::Rc<Function>>,
  pub static_methods: Vec<std::rc::Rc<Function>>,
}

#[derive(Debug, Clone)]
pub struct Trait {
  pub name: String,
  pub methods: Vec<(String, Signature)>,
  pub id: cache::Id,
}

#[derive(Debug, Clone)]
pub struct Enum {
  pub name: String,
  pub variants: Vec<(String, cache::Id)>,
  pub id: cache::Id,
  pub value_type: Type,
}

#[derive(Debug, Clone)]
pub struct IndexingExpr {
  pub name: String,
  pub index_expr: Box<NodeKind>,
  pub target_id: cache::Id,
}

#[derive(Debug, Clone)]
pub struct StaticArrayValue {
  pub elements: Vec<NodeKind>,
  /// Holds the type of the array, in case it is an empty array.
  pub explicit_type: Option<Type>,
}

#[derive(Debug, Clone)]
pub struct UnsafeExpr(pub Box<NodeKind>);

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
pub struct Signature {
  pub parameters: Vec<std::rc::Rc<Parameter>>,
  pub return_type_hint: Option<Type>,
  pub is_variadic: bool,
  pub is_extern: bool,
  pub accepts_instance: bool,
  pub instance_type_id: Option<cache::Id>,
  pub this_parameter: Option<Parameter>,
}

#[derive(Debug, Clone)]
pub struct ExternFunction {
  pub name: String,
  pub signature: Signature,
  pub attributes: Vec<Attribute>,
  pub id: cache::Id,
}

#[derive(Debug, Clone)]
pub struct ExternStatic {
  pub name: String,
  pub ty: Type,
  pub id: cache::Id,
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
  pub signature: Signature,
  pub body: std::rc::Rc<BlockExpr>,
  pub attributes: Vec<Attribute>,
  pub id: cache::Id,
  pub generics: Option<Generics>,
}

#[derive(Debug, Clone)]
pub struct BlockExpr {
  pub statements: Vec<NodeKind>,
  pub yields: Option<Box<NodeKind>>,
  pub id: cache::Id,
}

#[derive(Debug, Clone)]
pub struct ReturnStmt {
  pub value: Option<Box<NodeKind>>,
}

#[derive(Debug, Clone)]
pub struct BindingStmt {
  pub name: String,
  pub value: Box<NodeKind>,
  pub is_const_expr: bool,
  pub id: cache::Id,
  pub type_hint: Option<Type>,
}

#[derive(Debug, Clone)]
pub struct IfExpr {
  pub condition: Box<NodeKind>,
  pub then_value: Box<NodeKind>,
  pub alternative_branches: Vec<(NodeKind, NodeKind)>,
  pub else_value: Option<Box<NodeKind>>,
}

#[derive(Debug, Clone)]
pub struct InlineExprStmt {
  pub expr: Box<NodeKind>,
}

#[derive(Debug, Clone)]
pub struct CallExpr {
  pub callee_expr: Box<NodeKind>,
  pub arguments: Vec<NodeKind>,
}

#[derive(Debug, Clone)]
pub enum IntrinsicKind {
  LengthOf,
}

#[derive(Debug, Clone)]
pub struct IntrinsicCall {
  pub kind: IntrinsicKind,
  pub arguments: Vec<NodeKind>,
}

#[derive(PartialEq, Clone, Debug)]
pub struct Struct {
  pub id: cache::Id,
  pub name: String,
  pub fields: Vec<(String, Type)>,
}

#[derive(Debug, Clone)]
pub struct TypeAlias {
  pub name: String,
  pub ty: Type,
  pub id: cache::Id,
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
  In,
}

#[derive(Debug, Clone)]
pub struct BinaryExpr {
  pub left: std::rc::Rc<NodeKind>,
  pub right: std::rc::Rc<NodeKind>,
  pub operator: OperatorKind,
}

#[derive(Debug, Clone)]
pub struct UnaryExpr {
  pub expr: std::rc::Rc<NodeKind>,
  pub operator: OperatorKind,
  /// Represents the type being casted to.
  ///
  /// Only available when the unary expression is a cast.
  pub cast_type: Option<std::rc::Rc<Type>>,
}

#[derive(Debug, Clone)]
pub struct MemberAccess {
  pub base_expr: std::rc::Rc<NodeKind>,
  pub member_name: String,
}

#[cfg(test)]
mod tests {
  use super::*;

  #[test]
  fn traverse_ast() {
    let node = NodeKind::BlockExpr(BlockExpr {
      id: 0,
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

  // TODO: Re-do.
  // #[test]
  // fn find_node_in_ast() {
  //   let target_node = NodeKind::BreakStmt(BreakStmt);

  //   let block = NodeKind::BlockExpr(BlockExpr {
  //     id: 0,
  //     statements: vec![Node {
  //       cached_type: None,
  //       kind: target_node.clone(),
  //     }],
  //     yields: None,
  //   });

  //   let search_result = block.find_node(|node| matches!(node, NodeKind::BreakStmt(_)));

  //   assert!(matches!(search_result, Some(NodeKind::BreakStmt(_))));
  // }
}
