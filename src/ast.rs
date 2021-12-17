#[macro_export]
macro_rules! dispatch {
  ($node:expr, $target_fn:expr $(, $($args:expr),* )? ) => {
    match $node {
      $crate::ast::Node::Function(inner) => $target_fn(inner $(, $($args),* )?),
      _ => todo!(),
    }
  };
}

pub type Parameter = (String, Type);

pub enum IntSize {
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

pub enum PrimitiveType {
  Int(IntSize),
  Bool,
  Char,
}

pub enum Type {
  PrimitiveType(PrimitiveType),
  Prototype(Vec<(String, Type)>, Option<Box<Type>>, bool),
}

pub enum Node {
  Literal(Literal),
  Extern(Extern),
  Function(Function),
  Module(Module),
  Block(Block),
  BlockStmt(BlockStmt),
  ReturnStmt(ReturnStmt),
  LetStmt(LetStmt),
  IfStmt(IfStmt),
  WhileStmt(WhileStmt),
  CallExpr(CallExpr),
  BreakStmt(BreakStmt),
  ExprWrapperStmt(Box<Node>),
}

pub enum Literal {
  Bool(bool),
  Int(u64, IntSize),
  Char(char),
  String(String),
}

pub struct Extern {
  pub name: String,
  pub prototype: Type,
}

pub struct Function {
  pub name: String,
  pub prototype: Type,
  pub body: Block,
}

pub struct Module {
  pub name: String,
  // TODO: Symbol table?
  // pub symbol_table: std::collections::HashMap<String, TopLevelNodeHolder>,
}

pub struct Block {
  // TODO: Consider using an enum then assigning a name based on its value.
  pub llvm_name: String,
  pub statements: Vec<Box<Node>>,
}

pub struct BlockStmt {
  pub block: Block,
}

pub struct BreakStmt {
  //
}

pub struct ReturnStmt {
  pub value: Option<Box<Node>>,
}

pub struct LetStmt {
  pub name: String,
  pub ty: Type,
  pub value: Box<Node>,
}

pub struct IfStmt {
  pub condition: Box<Node>,
  pub then_block: Block,
  pub else_block: Option<Block>,
}

pub struct WhileStmt {
  pub condition: Box<Node>,
  pub body: Block,
}

pub struct CallExpr {
  pub callee: Box<Node>,
  pub arguments: Vec<Box<Node>>,
}
