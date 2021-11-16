use crate::{int_kind, pass, void_kind};

#[derive(Hash, Eq, PartialEq, Debug)]
pub enum KindTransport<'a> {
  IntKind(&'a int_kind::IntKind),
  VoidKind(&'a void_kind::VoidKind),
  BoolKind(&'a int_kind::BoolKind),
}

pub fn from_kind_holder<'a>(kind_holder: &'a KindHolder) -> KindTransport<'a> {
  match kind_holder {
    KindHolder::BoolKind(bool_kind) => KindTransport::BoolKind(&bool_kind),
    KindHolder::IntKind(int_kind) => KindTransport::IntKind(&int_kind),
    KindHolder::VoidKind(void_kind) => KindTransport::VoidKind(&void_kind),
  }
}

// TODO: Rename to `ExprHolder` or the likes.
#[derive(Hash, Eq, PartialEq, Debug, Copy, Clone)]
pub enum ExprTransport<'a> {
  BoolLiteral(&'a BoolLiteral),
  IntLiteral(&'a IntLiteral),
  CallExpr(&'a CallExpr<'a>),
}

pub fn from_expr_holder<'a>(expr_holder: &'a ExprHolder) -> ExprTransport<'a> {
  match expr_holder {
    ExprHolder::BoolLiteral(bool_literal) => ExprTransport::BoolLiteral(&bool_literal),
    ExprHolder::IntLiteral(int_literal) => ExprTransport::IntLiteral(&int_literal),
    ExprHolder::CallExpr(call_expr) => ExprTransport::CallExpr(&call_expr),
  }
}

pub trait Node {
  fn accept(&mut self, pass: &mut dyn pass::Pass) -> pass::PassResult;

  fn get_children(&self) -> Vec<&dyn Node> {
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

impl Node for BoolLiteral {
  fn accept(&mut self, pass: &mut dyn pass::Pass) -> pass::PassResult {
    // TODO:
    // pass.visit_bool_literal(self)

    Ok(())
  }
}

#[derive(Hash, Eq, PartialEq, Debug)]
pub struct IntLiteral {
  pub value: u64,
  pub kind: int_kind::IntKind,
}

impl Node for IntLiteral {
  fn accept(&mut self, pass: &mut dyn pass::Pass) -> pass::PassResult {
    // TODO:
    // pass.visit_int_literal(self)

    Ok(())
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

impl Node for External {
  fn accept(&mut self, pass: &mut dyn pass::Pass) -> pass::PassResult {
    // TODO:
    // pass.visit_external(self)

    Ok(())
  }
}

#[derive(Hash, Eq, PartialEq, Debug)]
pub struct Function<'a> {
  pub is_public: bool,
  pub prototype: Prototype,
  pub body: Block<'a>,
}

impl<'a> Node for Function<'a> {
  fn accept(&mut self, pass: &mut dyn pass::Pass) -> pass::PassResult {
    // TODO:
    // pass.visit_function(self)

    Ok(())
  }
}

pub type Parameter = (String, KindGroup);

#[derive(Hash, Eq, PartialEq, Debug)]
pub struct Prototype {
  pub name: String,
  pub parameters: Vec<Parameter>,
  pub is_variadic: bool,
  pub return_kind_group: KindGroup,
}

impl Node for Prototype {
  fn accept(&mut self, pass: &mut dyn pass::Pass) -> pass::PassResult {
    // TODO:
    // pass.visit_prototype(self)

    Ok(())
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
  pub fn new(name: String) -> Self {
    Module {
      name,
      symbol_table: std::collections::HashMap::new(),
    }
  }
}

impl<'a> Node for Module<'a> {
  fn accept(&mut self, pass: &mut dyn pass::Pass) -> pass::PassResult {
    // FIXME:
    // pass.visit_module(self)

    Ok(())
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

impl<'a> Node for Block<'a> {
  fn accept(&mut self, pass: &mut dyn pass::Pass) -> pass::PassResult {
    // TODO:
    // pass.visit_block(self)

    Ok(())
  }
}

#[derive(Hash, Eq, PartialEq, Debug)]
pub struct ReturnStmt<'a> {
  pub value: Option<ExprHolder<'a>>,
}

impl<'a> Node for ReturnStmt<'a> {
  fn accept(&mut self, pass: &mut dyn pass::Pass) -> pass::PassResult {
    // TODO:
    // pass.visit_return_stmt(self)

    Ok(())
  }
}

#[derive(Hash, Eq, PartialEq, Debug)]
pub struct LetStmt<'a> {
  pub name: String,
  pub kind_group: KindGroup,
  pub value: ExprHolder<'a>,
}

impl<'a> Node for LetStmt<'a> {
  fn accept(&mut self, pass: &mut dyn pass::Pass) -> pass::PassResult {
    // TODO:
    // pass.visit_let_stmt(self)

    Ok(())
  }
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

impl<'a> Node for CallExpr<'a> {
  fn accept(&mut self, pass: &mut dyn pass::Pass) -> pass::PassResult {
    // TODO:
    // pass.visit_call_expr(self)

    Ok(())
  }
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

impl<'a> Node for Stub<'a> {
  fn accept(&mut self, pass: &mut dyn pass::Pass) -> pass::PassResult {
    // TODO:
    // pass.visit_stub(self)
    Ok(())
  }
}
