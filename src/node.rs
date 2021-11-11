use crate::{int_kind, pass, void_kind};

#[macro_export]
macro_rules! stub_find_value {
  ($stub:expr, $symbol_table:expr) => {{
    let name = $stub.get_name();

    crate::assert!($symbol_table.contains_key(&name));

    $symbol_table.get(&name).unwrap()
  }};
}

#[derive(Hash, Eq, PartialEq, Debug, Copy, Clone)]
pub enum AnyKindNode {
  IntKind(int_kind::IntKind),
  VoidKind(void_kind::VoidKind),
  BoolKind(int_kind::BoolKind),
}

#[derive(Hash, Eq, PartialEq, Debug)]
pub enum AnyValueNode {
  BoolLiteral(BoolLiteral),
  IntLiteral(IntLiteral),
  CallExpr(CallExpr),
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

#[derive(Hash, Eq, PartialEq, Debug, Copy, Clone)]
pub struct BoolLiteral {
  pub value: bool,
}

impl Node for BoolLiteral {
  fn accept(&mut self, pass: &mut dyn pass::Pass) -> pass::PassResult {
    pass.visit_bool_literal(self)
  }
}

#[derive(Hash, Eq, PartialEq, Debug, Copy, Clone)]
pub struct IntLiteral {
  pub value: u64,
  pub kind: int_kind::IntKind,
}

impl Node for IntLiteral {
  fn accept(&mut self, pass: &mut dyn pass::Pass) -> pass::PassResult {
    pass.visit_int_literal(self)
  }
}

#[derive(Hash, Eq, PartialEq, Debug)]
pub struct KindGroup {
  pub kind: AnyKindNode,
  pub is_reference: bool,
  pub is_mutable: bool,
}

#[derive(Hash, Eq, PartialEq, Debug)]
pub struct External {
  pub prototype: Prototype,
}

impl Node for External {
  fn accept(&mut self, pass: &mut dyn pass::Pass) -> pass::PassResult {
    pass.visit_external(self)
  }
}

#[derive(Hash, Eq, PartialEq, Debug)]
pub struct Function {
  pub is_public: bool,
  pub prototype: Prototype,
  pub body: Block,
}

impl Node for Function {
  fn accept(&mut self, pass: &mut dyn pass::Pass) -> pass::PassResult {
    pass.visit_function(self)
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
    pass.visit_prototype(self)
  }
}

#[derive(Hash, Eq, PartialEq, Debug)]
pub enum AnyTopLevelNode {
  Function(Function),
  External(External),
}

pub struct Package {
  pub name: String,
  pub symbol_table: std::collections::HashMap<String, AnyTopLevelNode>,
}

impl Package {
  pub fn new(name: String) -> Self {
    Self {
      name,
      symbol_table: std::collections::HashMap::new(),
    }
  }
}

impl Node for Package {
  fn accept(&mut self, pass: &mut dyn pass::Pass) -> pass::PassResult {
    pass.visit_package(self)
  }
}

#[derive(Hash, Eq, PartialEq, Debug)]
pub enum AnyStmtNode {
  ReturnStmt(ReturnStmt),
  ExprWrapperStmt(AnyExprNode),
  LetStmt(LetStmt),
}

#[derive(Hash, Eq, PartialEq, Debug)]
pub struct Block {
  pub statements: Vec<AnyStmtNode>,
}

impl Node for Block {
  fn accept(&mut self, pass: &mut dyn pass::Pass) -> pass::PassResult {
    pass.visit_block(self)
  }
}

#[derive(Hash, Eq, PartialEq, Debug)]
pub struct ReturnStmt {
  pub value: Option<AnyValueNode>,
}

impl Node for ReturnStmt {
  fn accept(&mut self, pass: &mut dyn pass::Pass) -> pass::PassResult {
    pass.visit_return_stmt(self)
  }
}

#[derive(Hash, Eq, PartialEq, Debug)]
pub struct LetStmt {
  pub name: String,
  pub kind_group: KindGroup,
  pub value: AnyExprNode,
}

impl Node for LetStmt {
  fn accept(&mut self, pass: &mut dyn pass::Pass) -> pass::PassResult {
    pass.visit_let_stmt(self)
  }
}

#[derive(Hash, Eq, PartialEq, Debug)]
pub enum AnyExprNode {
  CallExpr(CallExpr),
  LiteralWrapperExpr(AnyValueNode),
}

#[derive(Hash, Eq, PartialEq, Debug)]
pub struct CallExpr {
  pub callee: Stub,
  pub arguments: Vec<AnyValueNode>,
}

impl Node for CallExpr {
  fn accept(&mut self, pass: &mut dyn pass::Pass) -> pass::PassResult {
    pass.visit_call_expr(self)
  }
}

#[derive(Hash, Eq, PartialEq, Debug)]
pub enum StubKind {
  Callable,
}

#[derive(Hash, Eq, PartialEq, Debug)]
pub enum Stub {
  Callable {
    name: String,
    value: Option<AnyTopLevelNode>,
  },
}

impl Stub {
  pub fn get_name(&self) -> String {
    match self {
      Self::Callable { name, .. } => name.clone(),
    }
  }
}

impl Node for Stub {
  fn accept(&mut self, pass: &mut dyn pass::Pass) -> pass::PassResult {
    pass.visit_stub(self)
  }
}
