use crate::int_kind;

#[macro_export]
macro_rules! dispatch {
  ($node:expr, $target_fn:expr $(, $($args:expr),* )? ) => {
    match $node {
      $crate::ast::Node::Function(inner) => $target_fn(inner $(, $($args),* )?),
      _ => todo!(),
    }
  };
}

pub type Parameter = (String, KindGroup);

pub enum Node<'a> {
  BoolLiteral(BoolLiteral),
  IntLiteral(IntLiteral),
  StringLiteral(StringLiteral),
  External(External),
  Function(Function<'a>),
  Prototype(Prototype),
  Module(Module<'a>),
  Block(Block<'a>),
  BlockStmt(BlockStmt<'a>),
  ReturnStmt(ReturnStmt<'a>),
  LetStmt(LetStmt<'a>),
  IfStmt(IfStmt<'a>),
  WhileStmt(WhileStmt<'a>),
  CallExpr(CallExpr<'a>),
  Literal(Literal),
}

pub enum IntegerKind {
  I8,
  I16,
  I32,
  I64,
  Isize,
  U8,
  U16,
  U32,
  U64,
  Usize,
}

pub enum Literal {
  Bool(bool),
  Integer(u64, IntegerKind),
  Char(char),
  String(String),
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
pub struct CallExpr<'a> {
  // FIXME: Finish implementing.
  pub callee: Option<context::>,
  pub arguments: Vec<ExprTransport<'a>>,
}
