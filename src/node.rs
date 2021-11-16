use crate::{int_kind, void_kind};

#[derive(Hash, Eq, PartialEq, Debug)]
pub enum KindTransport<'a> {
  IntKind(&'a int_kind::IntKind),
  VoidKind(&'a void_kind::VoidKind),
  BoolKind(&'a int_kind::BoolKind),
}

impl<'a> From<&'a KindHolder> for KindTransport<'a> {
  fn from(kind: &'a KindHolder) -> Self {
    match kind {
      KindHolder::IntKind(kind) => KindTransport::IntKind(kind),
      KindHolder::VoidKind(kind) => KindTransport::VoidKind(kind),
      KindHolder::BoolKind(kind) => KindTransport::BoolKind(kind),
    }
  }
}

// TODO: Rename to `ExprHolder` or the likes.
#[derive(Hash, Eq, PartialEq, Debug, Copy, Clone)]
pub enum ExprTransport<'a> {
  BoolLiteral(&'a BoolLiteral),
  IntLiteral(&'a IntLiteral),
  CallExpr(&'a CallExpr<'a>),
}

impl<'a> From<&'a ExprHolder<'a>> for ExprTransport<'a> {
  fn from(expr_holder: &'a ExprHolder<'a>) -> Self {
    match expr_holder {
      ExprHolder::BoolLiteral(bool_literal) => ExprTransport::BoolLiteral(bool_literal),
      ExprHolder::IntLiteral(int_literal) => ExprTransport::IntLiteral(int_literal),
      ExprHolder::CallExpr(call_expr) => ExprTransport::CallExpr(call_expr),
    }
  }
}

pub trait Node<'a> {
  // FIXME:
  // fn accept<'b>(&'b mut self, _: &mut dyn pass::Pass<'b>) -> pass::PassResult;

  // TODO: Consider switching to `visit_children()` because of limitations.
  fn get_children(&mut self) -> Vec<&mut dyn Node<'a>> {
    vec![]
  }
}

#[derive(Hash, Eq, PartialEq, Debug)]
pub struct Identifier {
  pub name: String,
}

#[derive(Hash, Eq, PartialEq, Debug)]
pub struct BoolLiteral {
  pub value: bool,
}

impl Node<'_> for BoolLiteral {}

#[derive(Hash, Eq, PartialEq, Debug)]
pub struct IntLiteral {
  pub value: u64,
  pub kind: int_kind::IntKind,
}

impl<'a> Node<'a> for IntLiteral {
  fn get_children(&mut self) -> Vec<&mut dyn Node<'a>> {
    vec![&mut self.kind]
  }
}

#[derive(Hash, Eq, PartialEq, Debug)]
pub struct KindGroup {
  pub kind: KindHolder,
  pub is_reference: bool,
  pub is_mutable: bool,
}

#[derive(Hash, Eq, PartialEq, Debug)]
pub struct External {
  pub prototype: Prototype,
}

impl<'a> Node<'a> for External {
  fn get_children(&mut self) -> Vec<&mut dyn Node<'a>> {
    vec![&mut self.prototype]
  }
}

#[derive(Hash, Eq, PartialEq, Debug)]
pub struct Function<'a> {
  pub is_public: bool,
  pub prototype: Prototype,
  pub body: Block<'a>,
}

pub type Parameter = (String, KindGroup);

#[derive(Hash, Eq, PartialEq, Debug)]
pub struct Prototype {
  pub name: String,
  pub parameters: Vec<Parameter>,
  pub is_variadic: bool,
  pub return_kind_group: KindGroup,
}

impl<'a> Node<'a> for Prototype {
  fn get_children(&mut self) -> Vec<&mut dyn Node<'a>> {
    let mut children = vec![&mut self.return_kind_group.kind];

    // children
    vec![]
  }
}

#[derive(Hash, Eq, PartialEq, Debug)]
pub enum ExprHolder<'a> {
  BoolLiteral(BoolLiteral),
  IntLiteral(IntLiteral),
  CallExpr(CallExpr<'a>),
}

#[derive(Hash, Eq, PartialEq, Debug)]
pub enum KindHolder {
  IntKind(int_kind::IntKind),
  VoidKind(void_kind::VoidKind),
  BoolKind(int_kind::BoolKind),
}

#[derive(Hash, Eq, PartialEq, Debug)]
pub enum TopLevelNodeHolder<'a> {
  Function(Function<'a>),
  External(External),
}

pub struct Module<'a> {
  pub name: String,
  pub symbol_table: std::collections::HashMap<String, TopLevelNodeHolder<'a>>,
}

impl<'a> Module<'a> {
  pub fn new(name: &str) -> Self {
    Module {
      name: name.into(),
      symbol_table: std::collections::HashMap::new(),
    }
  }
}

#[derive(Hash, Eq, PartialEq, Debug)]
pub enum AnyStmtNode<'a> {
  ReturnStmt(ReturnStmt<'a>),
  ExprWrapperStmt(ExprHolder<'a>),
  LetStmt(LetStmt<'a>),
}

#[derive(Hash, Eq, PartialEq, Debug)]
pub struct Block<'a> {
  pub statements: Vec<AnyStmtNode<'a>>,
}

#[derive(Hash, Eq, PartialEq, Debug)]
pub struct ReturnStmt<'a> {
  pub value: Option<ExprHolder<'a>>,
}

#[derive(Hash, Eq, PartialEq, Debug)]
pub struct LetStmt<'a> {
  pub name: String,
  pub kind_group: KindGroup,
  pub value: ExprHolder<'a>,
}

#[derive(Hash, Eq, PartialEq, Debug)]
pub enum AnyExprNode<'a> {
  CallExpr(&'a CallExpr<'a>),
  LiteralWrapperExpr(ExprHolder<'a>),
}

#[derive(Hash, Eq, PartialEq, Debug)]
pub struct CallExpr<'a> {
  pub callee: Stub<'a>,
  pub arguments: Vec<ExprTransport<'a>>,
}

#[derive(Hash, Eq, PartialEq, Debug)]
pub enum StubValueTransport<'a> {
  Function(&'a Function<'a>),
  External(&'a External),
}

#[derive(Hash, Eq, PartialEq, Debug)]
pub enum Stub<'a> {
  Callable {
    name: String,
    value: Option<StubValueTransport<'a>>,
  },
}

impl<'a> Stub<'a> {
  pub fn get_name(&self) -> String {
    match self {
      Self::Callable { name, .. } => name.clone(),
    }
  }
}
