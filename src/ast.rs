use crate::{name_resolution, symbol_table, type_inference};

#[macro_export]
macro_rules! force_match {
  ($subject:expr, $path:path) => {
    match $subject {
      $path(inner) => inner,
      _ => unreachable!(),
    }
  };
}

pub type AstMap = std::collections::BTreeMap<name_resolution::Qualifier, Vec<NodeKind>>;
pub type Diagnostic = codespan_reporting::diagnostic::Diagnostic<usize>;

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
  pub type_hint: Option<Type>,
  pub position: u32,
  pub id: symbol_table::NodeId,
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
pub enum TypeConstructorKind {
  StaticIndexable(u32),
  Pointer,
  Reference,
  Signature,
  Integer,
  Boolean,
  Nullptr,
  String,
}

#[derive(PartialEq, Clone, Debug)]
pub enum Type {
  /// A static indexable type.
  ///
  /// Its type and length are always known at compile-time.
  StaticIndexable(Box<Type>, u32),
  Basic(BasicType),
  Pointer(Box<Type>),
  Reference(Box<Type>),
  Struct(Struct),
  /// A type that needs to be resolved.
  Stub(StubType),
  Signature(SignatureType),
  This(ThisType),
  /// A meta type to be used during unification.
  ///
  /// Represents a type that has not yet been solved.
  Variable(type_inference::TypeVariableId),
  /// A meta type to be used during unification.
  ///
  /// Represents a constructor type that has not yet been solved.
  /// Can be used to represent arrays, where the generics are its
  /// elements, functions, where the generics are its parameters followed
  /// by its return type, and so on.
  Constructor(TypeConstructorKind, Vec<Type>),
  /// A meta type that represents a super-type of all integer types.
  AnyInteger,
  /// A meta type that represents the lack of a value.
  Unit,
  // TODO: Remove. We don't have panics anymore. [Actually, no. What about things like early returns, say if have a return inside an if expr? Or if in the future we decide to treat return statement as an expression?].
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
  // REVIEW: What about nested type constructors?
  /// Attempt to lift a type to a type constructor for use with the
  /// inference algorithm.
  ///
  /// If the type is not a type constructor, then it is returned as-is.
  pub fn try_upgrade_constructor(self) -> Type {
    let kind = match &self {
      Type::StaticIndexable(_, size) => TypeConstructorKind::StaticIndexable(*size),
      Type::Pointer(_) => TypeConstructorKind::Pointer,
      Type::Reference(_) => TypeConstructorKind::Reference,
      Type::Signature(_) => TypeConstructorKind::Signature,
      _ => return self,
    };

    // REVIEW: I think we've got the concept of generics wrong (Or the implementation's
    // ... handling is wrong). This looks like it should be it instead:
    // let generics = self
    //   .find_inner_generics()
    //   .into_iter()
    //   .map(|ty| ty.to_owned())
    //   .collect();

    Type::Constructor(
      kind,
      self
        .find_inner_generics()
        .into_iter()
        .map(|generic| generic.to_owned())
        .collect(),
    )
  }

  pub fn old_try_downgrade(self) -> Type {
    match self {
      Type::Constructor(kind, generics) => match &kind {
        TypeConstructorKind::StaticIndexable(size) => {
          // TODO: Cloning.
          // REVIEW: Is this conversion correct?
          // REVIEW: Direct access, might panic during runtime. Try separation of concerns?
          Type::StaticIndexable(Box::new(generics[0].clone()), *size)
        }
        _ => todo!(),
      },
      _ => self,
    }
  }

  pub fn find_inner_generics(&self) -> Vec<&Type> {
    // TODO: What if they contain nested generics? Is that possible?
    match &self {
      Type::StaticIndexable(inner, _) => vec![inner],
      Type::Pointer(inner) => vec![inner],
      Type::Reference(inner) => vec![inner],
      Type::Signature(signature_type) => signature_type
        .parameter_types
        .iter()
        .chain(std::iter::once(signature_type.return_type.as_ref()))
        .collect::<Vec<_>>(),
      _ => Vec::new(),
    }
  }

  pub fn is_a_constructor(&self) -> bool {
    return matches!(
      self,
      // FIXME: Any more? Yes! Missing `nullptr`, which is a constructor (generic).
      Type::StaticIndexable(..) | Type::Pointer(_) | Type::Reference(_)
    );
  }

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
    self.is_a_unit()
      || self.is_a_never()
      || matches!(
        self,
        Type::Any | Type::AnyInteger | Type::Variable(_) | Type::Constructor(..)
      )
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

  pub fn flat_is(&self, other: &Type, cache: &symbol_table::SymbolTable) -> bool {
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
    // If at least one is a meta integer type, return `true` if the other is an integer type,
    // or if they're both a meta integer type.
    else if matches!(self, Type::AnyInteger) || matches!(other, Type::AnyInteger) {
      return matches!(self, Type::Basic(BasicType::Int(_)))
        || matches!(other, Type::Basic(BasicType::Int(_)))
        || self == other;
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
  pub fn flatten(&self, cache: &symbol_table::SymbolTable) -> Type {
    // REVISE: Cleanup.

    // REVIEW: What if it's a pointer to a user-defined type?
    if let Type::Stub(stub_type) = self {
      let target_node = cache
        .find_decl_via_link(&stub_type.pattern.link_id)
        .unwrap();

      // REVIEW: What about type aliases, and other types that might be encountered in the future?

      // REVISE: Cleanup!
      if let NodeKind::TypeAlias(type_alias) = &target_node {
        return type_alias.ty.flatten(cache);
      } else if let NodeKind::Struct(target_type) = &target_node {
        // REVIEW: Why is `flatten_type` being called again with a struct type inside?
        return Type::Struct(target_type.as_ref().clone()).flatten(cache);
      }
    } else if let Type::This(this_type) = &self {
      // REVISE: No need to clone?
      let target_struct_type = cache
        .find_decl_via_link(&this_type.target_id.unwrap())
        .unwrap();

      if let NodeKind::Struct(struct_type) = &target_struct_type {
        return Type::Struct(struct_type.as_ref().clone());
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
  ExternFunction(std::rc::Rc<ExternFunction>),
  ExternStatic(std::rc::Rc<ExternStatic>),
  Function(std::rc::Rc<Function>),
  BlockExpr(std::rc::Rc<BlockExpr>),
  ReturnStmt(ReturnStmt),
  BindingStmt(std::rc::Rc<BindingStmt>),
  IfExpr(IfExpr),
  CallExpr(CallExpr),
  IntrinsicCall(IntrinsicCall),
  InlineExprStmt(InlineExprStmt),
  Reference(Reference),
  BinaryExpr(BinaryExpr),
  UnaryExpr(UnaryExpr),
  Parameter(std::rc::Rc<Parameter>),
  UnsafeExpr(UnsafeExpr),
  Array(Array),
  IndexingExpr(IndexingExpr),
  Enum(std::rc::Rc<Enum>),
  Struct(std::rc::Rc<Struct>),
  Signature(Signature),
  StructValue(StructValue),
  Pattern(Pattern),
  TypeAlias(std::rc::Rc<TypeAlias>),
  Closure(Closure),
  MemberAccess(MemberAccess),
  StructImpl(StructImpl),
  Trait(std::rc::Rc<Trait>),
  ParenthesesExpr(ParenthesesExpr),
  Using(Using),
  SizeofIntrinsic(SizeofIntrinsic),
  Range(Range),
  Type(Type),
}

impl NodeKind {
  pub fn find_type<'a>(&self, type_cache: &'a type_inference::TypeCache) -> Option<&'a Type> {
    self.find_id().and_then(|id| type_cache.get(&id))
  }

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
    //     NodeKind::StaticArrayValue(array) => {
    //       map_children(array.elements).collect()
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
  pub fn infer_flatten_type(&self, cache: &symbol_table::SymbolTable) -> Type {
    // self.infer_type(cache).flatten(cache)
    todo!()
  }

  pub fn find_id(&self) -> Option<symbol_table::NodeId> {
    match self {
      NodeKind::BindingStmt(binding_stmt) => Some(binding_stmt.id),
      NodeKind::Function(function) => Some(function.id),
      NodeKind::TypeAlias(type_alias) => Some(type_alias.id),
      NodeKind::Struct(struct_) => Some(struct_.id),
      NodeKind::Trait(trait_) => Some(trait_.id),
      NodeKind::Closure(closure) => Some(closure.id),
      NodeKind::Enum(enum_) => Some(enum_.id),
      // REVIEW: Should this be here as well? Not a declaration?
      NodeKind::Pattern(pattern) => Some(pattern.link_id),
      NodeKind::BlockExpr(block_expr) => Some(block_expr.id),
      NodeKind::ExternFunction(extern_function) => Some(extern_function.id),
      NodeKind::ExternStatic(extern_static) => Some(extern_static.id),
      NodeKind::Parameter(parameter) => Some(parameter.id),
      // REVIEW: Should this be here as well? Not a declaration?
      NodeKind::Reference(reference) => Some(reference.pattern.link_id),
      NodeKind::Literal(Literal::Nullptr(id, _)) => Some(id.to_owned()),
      NodeKind::Array(static_array) => Some(static_array.id),
      // TODO: What about parentheses expression (transient nodes)? Perform flattening?
      _ => None,
    }
  }

  pub fn find_signature(&self) -> Option<&Signature> {
    match self {
      NodeKind::Function(function) => Some(&function.signature),
      NodeKind::Closure(closure) => Some(&closure.signature),
      _ => None,
    }
  }
}

#[derive(Debug, Clone)]
pub struct Node {
  pub kind: NodeKind,
  // TODO: In the future, we may be able to use node location as its id.
  pub id: symbol_table::NodeId,
  pub location: (usize, usize),
}

#[derive(Debug, Clone)]
pub struct Range {
  pub start: Literal,
  pub end: Literal,
}

#[derive(Debug, Clone)]
pub struct Using {
  pub package_name: String,
  pub module_name: String,
}

#[derive(Debug, Clone)]
pub struct Closure {
  pub id: symbol_table::NodeId,
  pub captures: Vec<(String, Option<symbol_table::NodeId>)>,
  pub signature: Signature,
  pub body: std::rc::Rc<BlockExpr>,
}

#[derive(PartialEq, Clone, Debug)]
pub struct SignatureType {
  pub return_type: Box<Type>,
  pub parameter_types: Vec<Type>,
  pub is_variadic: bool,
}

#[derive(PartialEq, Clone, Debug)]
pub struct ThisType {
  pub target_id: Option<symbol_table::NodeId>,
}

// FIXME: This will no longer have the `member_path` field. It will be replaced by the implementation of `MemberAccess`.
// TODO: If it's never boxed under `Node`, then there might not be a need for it to be included under `Node`?
#[derive(Debug, Clone, PartialEq)]
pub struct Pattern {
  pub link_id: symbol_table::NodeId,
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
  pub target_id: symbol_table::NodeId,
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
  pub id: symbol_table::NodeId,
}

#[derive(Debug, Clone)]
pub struct Enum {
  pub name: String,
  pub variants: Vec<(String, symbol_table::NodeId)>,
  pub id: symbol_table::NodeId,
  pub value_type: Type,
}

#[derive(Debug, Clone)]
pub struct IndexingExpr {
  pub target_expr: Box<NodeKind>,
  pub index_expr: Box<NodeKind>,
}

/// A static array value.
///
/// Its size is known at compile time.
#[derive(Debug, Clone)]
pub struct Array {
  pub elements: Vec<NodeKind>,
  pub id: symbol_table::NodeId,
  pub element_type_id: symbol_table::NodeId,
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
  Nullptr(symbol_table::NodeId, Option<Type>),
}

#[derive(Debug, Clone)]
pub struct Signature {
  pub parameters: Vec<std::rc::Rc<Parameter>>,
  pub return_type_hint: Option<Type>,
  pub is_variadic: bool,
  pub is_extern: bool,
  pub accepts_instance: bool,
  pub instance_type_id: Option<symbol_table::NodeId>,
  pub return_type_id: symbol_table::NodeId,
  pub this_parameter: Option<Parameter>,
}

#[derive(Debug, Clone)]
pub struct ExternFunction {
  pub name: String,
  pub signature: Signature,
  pub attributes: Vec<Attribute>,
  pub id: symbol_table::NodeId,
}

#[derive(Debug, Clone)]
pub struct ExternStatic {
  pub name: String,
  pub ty: Type,
  pub id: symbol_table::NodeId,
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
  pub id: symbol_table::NodeId,
  pub generics: Option<Generics>,
}

#[derive(Debug, Clone)]
pub struct BlockExpr {
  pub statements: Vec<NodeKind>,
  pub yields: Option<Box<NodeKind>>,
  pub id: symbol_table::NodeId,
}

#[derive(Debug, Clone)]
pub struct ReturnStmt {
  pub value: Option<Box<NodeKind>>,
  // REVIEW: Considering linking it to a specific function, this may ease the problems
  // ... caused by buffers when processing inner functions (closures).
}

#[derive(Debug, Clone)]
pub struct BindingStmt {
  pub name: String,
  pub value: Box<NodeKind>,
  pub is_const_expr: bool,
  pub id: symbol_table::NodeId,
  pub type_hint: Option<Type>,
}

#[derive(Debug, Clone)]
pub struct IfExpr {
  pub id: symbol_table::NodeId,
  pub condition: Box<NodeKind>,
  pub then_branch: Box<NodeKind>,
  pub alternative_branches: Vec<(NodeKind, NodeKind)>,
  pub else_branch: Option<Box<NodeKind>>,
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
  pub id: symbol_table::NodeId,
  pub name: String,
  pub fields: Vec<(String, Type)>,
}

#[derive(Debug, Clone)]
pub struct TypeAlias {
  pub name: String,
  pub ty: Type,
  pub id: symbol_table::NodeId,
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
  // TODO: Missing inequality operator.
}

#[derive(Debug, Clone)]
pub struct BinaryExpr {
  pub left_operand: std::rc::Rc<NodeKind>,
  pub right_operand: std::rc::Rc<NodeKind>,
  pub operator: OperatorKind,
}

#[derive(Debug, Clone)]
pub struct UnaryExpr {
  pub operand: std::rc::Rc<NodeKind>,
  pub operator: OperatorKind,
  /// Represents the type being casted to.
  ///
  /// Only available when the unary expression is a cast.
  pub cast_type: Option<Type>,
}

#[derive(Debug, Clone)]
pub struct MemberAccess {
  pub base_expr: std::rc::Rc<NodeKind>,
  pub member_name: String,
}

#[cfg(test)]
mod tests {
  use super::*;
  use pretty_assertions::{assert_eq, assert_ne};

  // #[test]
  // fn traverse_ast() {
  //   let node = NodeKind::BlockExpr(BlockExpr {
  //     id: 0,
  //     statements: Vec::new(),
  //     yields: None,
  //   });

  //   let mut visitations = 0;

  //   node.traverse(|_| {
  //     visitations += 1;

  //     true
  //   });

  //   assert_eq!(1, visitations);
  // }

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
