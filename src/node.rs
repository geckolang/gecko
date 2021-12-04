use std::ops::DerefMut;

use crate::{int_kind, pass};

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

pub trait Node {
  fn accept_pass<'a>(&'a self, _: &mut dyn pass::AnalysisPass<'a>) -> pass::PassResult;

  fn accept_transform_pass<'a>(
    &'a mut self,
    _: &mut dyn pass::TransformPass<'a>,
  ) -> pass::PassResult {
    Ok(())
  }

  // TODO: Consider switching to just invoking `visit_children()` because of limitations.
  fn get_children(&self) -> Vec<&mut dyn Node> {
    vec![]
  }

  fn is_top_level(&self) -> bool {
    false
  }
}

pub trait MutableNode {
  fn accept<'a>(&'a mut self, _: &mut dyn pass::AnalysisPass<'a>) -> pass::PassResult;
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
  fn accept_pass<'a>(&'a self, pass: &mut dyn pass::AnalysisPass<'a>) -> pass::PassResult {
    pass.visit_bool_literal(self)
  }
}

#[derive(Hash, Eq, PartialEq, Debug)]
pub struct IntLiteral {
  pub value: u64,
  pub kind: int_kind::IntKind,
}

impl Node for IntLiteral {
  fn accept_pass<'a>(&'a self, pass: &mut dyn pass::AnalysisPass<'a>) -> pass::PassResult {
    pass.visit_int_literal(self)
  }

  fn get_children(&self) -> Vec<&mut dyn Node> {
    vec![&mut self.kind]
  }
}

#[derive(Hash, Eq, PartialEq, Debug)]
pub struct StringLiteral {
  pub value: String,
  // TODO: In the future, add support for prefixes (as well as parsing of them).
}

impl Node for StringLiteral {
  fn accept_pass<'a>(&'a self, pass: &mut dyn pass::AnalysisPass<'a>) -> pass::PassResult {
    pass.visit_string_literal(self)
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
  fn accept_pass<'a>(&'a self, pass: &mut dyn pass::AnalysisPass<'a>) -> pass::PassResult {
    pass.visit_external(self)
  }

  fn get_children(&self) -> Vec<&mut dyn Node> {
    vec![&mut self.prototype]
  }

  fn is_top_level(&self) -> bool {
    true
  }
}

#[derive(Hash, Eq, PartialEq, Debug)]
pub struct Function<'a> {
  pub is_public: bool,
  pub prototype: Prototype,
  pub body: Block<'a>,
}

impl<'a> Node for Function<'a> {
  fn accept_pass<'b>(&'b self, pass: &mut dyn pass::AnalysisPass<'b>) -> pass::PassResult {
    pass.visit_function(self)
  }

  fn get_children(&self) -> Vec<&mut dyn Node> {
    vec![&mut self.prototype, &mut self.body]
  }

  fn is_top_level(&self) -> bool {
    true
  }
}

pub type Parameter = (String, KindGroup);

#[derive(Hash, Eq, PartialEq, Debug)]
pub struct Prototype {
  pub name: String,
  pub parameters: Vec<Parameter>,
  pub is_variadic: bool,
  pub return_kind_group: Option<KindGroup>,
}

impl<'a> Node for Prototype {
  fn accept_pass<'b>(&'b self, pass: &mut dyn pass::AnalysisPass<'b>) -> pass::PassResult {
    pass.visit_prototype(self)
  }

  fn get_children(&self) -> Vec<&mut dyn Node> {
    // TODO: Figure this out.
    // vec![&self.return_kind_group.kind]
    vec![]
  }
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

// TODO: Document methods (for developers).
impl<'a> Node for Module<'a> {
  fn accept_pass<'b>(&'b self, pass: &mut dyn pass::AnalysisPass<'b>) -> pass::PassResult {
    pass.visit_module(self)
  }

  fn is_top_level(&self) -> bool {
    true
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

impl Node for Block<'_> {
  fn accept_pass<'a>(&'a self, pass: &mut dyn pass::AnalysisPass<'a>) -> pass::PassResult {
    pass.visit_block(self)
  }

  fn get_children(&self) -> Vec<&mut dyn Node> {
    let mut children = Vec::new();

    for statement in &mut self.statements {
      match statement {
        AnyStmtNode::ReturnStmt(stmt) => children.push(stmt as &mut dyn Node),
        AnyStmtNode::ExprWrapperStmt(expr) => children.push(match expr {
          ExprHolder::BoolLiteral(expr) => expr as &mut dyn Node,
          ExprHolder::IntLiteral(expr) => expr as &mut dyn Node,
          ExprHolder::CallExpr(expr) => expr as &mut dyn Node,
        }),
        AnyStmtNode::LetStmt(stmt) => children.push(stmt as &mut dyn Node),
        AnyStmtNode::IfStmt(stmt) => children.push(stmt as &mut dyn Node),
        AnyStmtNode::WhileStmt(stmt) => children.push(stmt as &mut dyn Node),
        AnyStmtNode::BlockStmt(stmt) => children.push(stmt as &mut dyn Node),
        AnyStmtNode::BreakStmt(stmt) => children.push(stmt as &mut dyn Node),
      };
    }

    children
  }
}

#[derive(Hash, Eq, PartialEq, Debug)]
pub struct BlockStmt<'a> {
  pub block: Block<'a>,
}

impl Node for BlockStmt<'_> {
  fn accept_pass<'a>(&'a self, pass: &mut dyn pass::AnalysisPass<'a>) -> pass::PassResult {
    pass.visit_block_stmt(self)
  }

  fn get_children(&self) -> Vec<&mut dyn Node> {
    vec![&mut self.block]
  }
}

#[derive(Hash, Eq, PartialEq, Debug)]
pub struct BreakStmt {
  //
}

impl Node for BreakStmt {
  fn accept_pass<'a>(&'a self, pass: &mut dyn pass::AnalysisPass<'a>) -> pass::PassResult {
    pass.visit_break_stmt(self)
  }
}

#[derive(Hash, Eq, PartialEq, Debug)]
pub struct ReturnStmt<'a> {
  pub value: Option<ExprHolder<'a>>,
}

impl<'a> Node for ReturnStmt<'a> {
  fn accept_pass<'b>(&'b self, pass: &mut dyn pass::AnalysisPass<'b>) -> pass::PassResult {
    pass.visit_return_stmt(self)
  }

  fn get_children(&self) -> Vec<&mut dyn Node> {
    match &mut self.value {
      Some(value) => vec![match value {
        ExprHolder::BoolLiteral(expr) => expr as &mut dyn Node,
        ExprHolder::IntLiteral(expr) => expr as &mut dyn Node,
        ExprHolder::CallExpr(expr) => expr as &mut dyn Node,
      }],
      None => vec![],
    }
  }
}

#[derive(Hash, Eq, PartialEq, Debug)]
pub struct LetStmt<'a> {
  pub name: String,
  pub kind_group: KindGroup,
  pub value: ExprHolder<'a>,
}

impl Node for LetStmt<'_> {
  fn accept_pass<'a>(&'a self, pass: &mut dyn pass::AnalysisPass<'a>) -> pass::PassResult {
    pass.visit_let_stmt(self)
  }

  fn get_children(&self) -> Vec<&mut dyn Node> {
    vec![
      match &mut self.kind_group.kind {
        KindHolder::IntKind(kind) => kind as &mut dyn Node,
        KindHolder::BoolKind(kind) => kind as &mut dyn Node,
      },
      match &mut self.value {
        ExprHolder::BoolLiteral(expr) => expr as &mut dyn Node,
        ExprHolder::IntLiteral(expr) => expr as &mut dyn Node,
        ExprHolder::CallExpr(expr) => expr as &mut dyn Node,
      },
    ]
  }
}

#[derive(Hash, Eq, PartialEq, Debug)]
pub struct IfStmt<'a> {
  pub condition: ExprHolder<'a>,
  pub then_block: Block<'a>,
  pub else_block: Option<Block<'a>>,
}

impl Node for IfStmt<'_> {
  fn accept_pass<'a>(&'a self, pass: &mut dyn pass::AnalysisPass<'a>) -> pass::PassResult {
    pass.visit_if_stmt(self)
  }

  fn get_children(&self) -> Vec<&mut dyn Node> {
    // TODO:
    vec![]
    // vec![&self.condition, &self.then_block]
  }
}

#[derive(Hash, Eq, PartialEq, Debug)]
pub struct WhileStmt<'a> {
  pub condition: ExprHolder<'a>,
  pub body: Block<'a>,
}

impl Node for WhileStmt<'_> {
  fn accept_pass<'a>(&'a self, pass: &mut dyn pass::AnalysisPass<'a>) -> pass::PassResult {
    pass.visit_while_stmt(self)
  }

  fn get_children(&self) -> Vec<&mut dyn Node> {
    // TODO: Missing condition.
    vec![&mut self.body]
  }
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

#[derive(Hash, Eq, PartialEq, Debug)]
pub struct CalleeStub<'a> {
  pub name: String,
  pub value: Option<CalleeTransport<'a>>,
}

impl<'a> Node for CalleeStub<'_> {
  fn accept_pass<'b>(&'b self, pass: &mut dyn pass::AnalysisPass<'b>) -> pass::PassResult {
    pass.visit_callee_stub(self)
  }

  fn accept_transform_pass<'c>(
    &'c mut self,
    pass: &mut dyn pass::TransformPass<'c>,
  ) -> pass::PassResult {
    pass.visit_callee_stub(self)
  }

  fn get_children(&self) -> Vec<&mut dyn Node> {
    // match self.value.as_mut() {
    //   // TODO: Dereferencing value.
    //   Some(value) => vec![match value.deref_mut() {
    //     CalleeTransport::Function(func) => func as &mut dyn Node,
    //     CalleeTransport::External(external) => external as &mut dyn Node,
    //   }],
    //   None => vec![],
    // }
    // FIXME:
    vec![]
  }
}

#[derive(Hash, Eq, PartialEq, Debug)]
pub struct CallExpr<'a> {
  pub callee_stub: CalleeStub<'a>,
  pub arguments: Vec<ExprTransport<'a>>,
}

impl Node for CallExpr<'_> {
  fn accept_pass<'a>(&'a self, pass: &mut dyn pass::AnalysisPass<'a>) -> pass::PassResult {
    pass.visit_call_expr(self)
  }
}
