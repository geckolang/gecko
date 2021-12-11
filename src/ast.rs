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
  Literal(Literal),
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

pub struct External {
  pub prototype: Prototype,
}

pub struct Function<'a> {
  pub prototype: Prototype,
  pub body: Block<'a>,
}

pub struct Prototype {
  pub name: String,
  pub parameters: Vec<Parameter>,
  pub is_variadic: bool,
  // TODO: Return type.
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

pub struct Block<'a> {
  // TODO: Consider using an enum then assigning a name based on its value.
  pub llvm_name: String,
  // TODO: Statements.
}

pub struct BlockStmt<'a> {
  pub block: Block<'a>,
}

pub struct BreakStmt {
  //
}

pub struct ReturnStmt<'a> {
  pub value: Option<ExprHolder<'a>>,
}

pub struct LetStmt<'a> {
  pub name: String,
  pub kind_group: KindGroup,
  pub value: ExprHolder<'a>,
}

pub struct IfStmt<'a> {
  pub condition: ExprHolder<'a>,
  pub then_block: Block<'a>,
  pub else_block: Option<Block<'a>>,
}

pub struct WhileStmt<'a> {
  pub condition: ExprHolder<'a>,
  pub body: Block<'a>,
}

pub struct CallExpr<'a> {
  // FIXME: Finish implementing.
  pub callee: Option<context::>,
  pub arguments: Vec<ExprTransport<'a>>,
}
