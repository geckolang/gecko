use crate::int_kind;

pub type Parameter = (String, KindGroup);

#[derive(Hash, Eq, PartialEq, Debug)]
pub enum KindTransport<'a> {
  IntKind(&'a int_kind::IntKind),
  BoolKind(&'a int_kind::BoolKind),
}

impl<'a> From<&'a KindHolder> for KindTransport<'a> {
  fn from(kind: &'a KindHolder) -> Self {
    match kind {
      KindHolder::IntKind(kind) => KindTransport::IntKind(kind),
      KindHolder::BoolKind(kind) => KindTransport::BoolKind(kind),
    }
  }
}

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

#[derive(Hash, Eq, PartialEq, Debug)]
pub struct Identifier {
  pub name: String,
}

#[derive(Hash, Eq, PartialEq, Debug)]
pub struct BoolLiteral {
  pub value: bool,
}

#[derive(Hash, Eq, PartialEq, Debug)]
pub struct IntLiteral {
  pub value: u64,
  pub kind: int_kind::IntKind,
}

#[derive(Hash, Eq, PartialEq, Debug)]
pub struct StringLiteral {
  pub value: String,
  // TODO: In the future, add support for prefixes (as well as parsing of them).
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

#[derive(Hash, Eq, PartialEq, Debug)]
pub struct Function<'a> {
  pub is_public: bool,
  pub prototype: Prototype,
  pub body: Block<'a>,
}

#[derive(Hash, Eq, PartialEq, Debug)]
pub struct Prototype {
  pub name: String,
  pub parameters: Vec<Parameter>,
  pub is_variadic: bool,
  pub return_kind_group: Option<KindGroup>,
}

#[derive(Hash, Eq, PartialEq, Debug)]
pub enum ExprHolder<'a> {
  BoolLiteral(BoolLiteral),
  IntLiteral(IntLiteral),
  CallExpr(CallExpr<'a>),
}

#[derive(Hash, Eq, PartialEq, Debug, Clone)]
pub enum KindHolder {
  IntKind(int_kind::IntKind),
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
      name: name.to_string(),
      symbol_table: std::collections::HashMap::new(),
    }
  }
}

#[derive(Hash, Eq, PartialEq, Debug)]
pub enum AnyStmtNode<'a> {
  ReturnStmt(ReturnStmt<'a>),
  ExprWrapperStmt(ExprHolder<'a>),
  LetStmt(LetStmt<'a>),
  IfStmt(IfStmt<'a>),
  WhileStmt(WhileStmt<'a>),
  BlockStmt(BlockStmt<'a>),
  BreakStmt(BreakStmt),
}

#[derive(Hash, Eq, PartialEq, Debug)]
pub struct Block<'a> {
  // TODO: Consider using an enum then assigning a name based on its value.
  pub llvm_name: String,
  pub statements: Vec<AnyStmtNode<'a>>,
}

// TODO: Consider having nodes with no implementations, strictly.
impl<'a> Block<'a> {
  /// Attempt to find a return statement in the block.
  ///
  /// Only the first return statement is returned (if any). There may be multiple return
  /// statements in a block.
  pub fn find_terminator(&self) -> Option<&ReturnStmt<'a>> {
    for statement in &self.statements {
      match statement {
        AnyStmtNode::ReturnStmt(return_stmt) => return Some(return_stmt),
        _ => continue,
      };
    }

    None
  }
}

#[derive(Hash, Eq, PartialEq, Debug)]
pub struct BlockStmt<'a> {
  pub block: Block<'a>,
}

#[derive(Hash, Eq, PartialEq, Debug)]
pub struct BreakStmt {
  //
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
pub struct IfStmt<'a> {
  pub condition: ExprHolder<'a>,
  pub then_block: Block<'a>,
  pub else_block: Option<Block<'a>>,
}

#[derive(Hash, Eq, PartialEq, Debug)]
pub struct WhileStmt<'a> {
  pub condition: ExprHolder<'a>,
  pub body: Block<'a>,
}

#[derive(Hash, Eq, PartialEq, Debug)]
pub enum AnyExprNode<'a> {
  CallExpr(&'a CallExpr<'a>),
  LiteralWrapperExpr(ExprHolder<'a>),
}

#[derive(Hash, Eq, PartialEq, Debug)]
pub enum CalleeTransport<'a> {
  Function(&'a Function<'a>),
  External(&'a External),
}

impl<'a> CalleeTransport<'a> {
  pub fn get_prototype(&self) -> &Prototype {
    match self {
      CalleeTransport::Function(function) => &function.prototype,
      CalleeTransport::External(external) => &external.prototype,
    }
  }
}

#[derive(Hash, Eq, PartialEq, Debug)]
pub struct CalleeStub<'a> {
  pub name: String,
  pub value: Option<CalleeTransport<'a>>,
}

#[derive(Hash, Eq, PartialEq, Debug)]
pub struct CallExpr<'a> {
  pub callee_stub: CalleeStub<'a>,
  pub arguments: Vec<ExprTransport<'a>>,
}
