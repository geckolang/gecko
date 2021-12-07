use crate::{diagnostic, int_kind, type_check};

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

pub trait Node {
  fn type_check(&self) -> type_check::TypeCheckResult {
    None
  }

  // TODO: Consider returning `&dyn Node` instead of a mutable one. There is no need for it to be mutable.
  // TODO: Consider switching to just invoking `visit_children()` because of limitations.
  fn get_children(&mut self) -> Vec<&mut dyn Node> {
    vec![]
  }

  /// Walk the node's children in a depth-first manner.
  fn walk_children(&mut self, callback: &mut dyn FnMut(&mut dyn Node)) {
    let mut children_queue = self.get_children();

    while let Some(child) = children_queue.pop() {
      // TODO: Does callback order matter?
      callback(child);
      children_queue.append(&mut child.get_children());
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

impl Node for BoolLiteral {
  //
}

#[derive(Hash, Eq, PartialEq, Debug)]
pub struct IntLiteral {
  pub value: u64,
  pub kind: int_kind::IntKind,
}

impl Node for IntLiteral {
  fn get_children(&mut self) -> Vec<&mut dyn Node> {
    vec![&mut self.kind]
  }
}

#[derive(Hash, Eq, PartialEq, Debug)]
pub struct StringLiteral {
  pub value: String,
  // TODO: In the future, add support for prefixes (as well as parsing of them).
}

impl Node for StringLiteral {
  //
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
  fn get_children(&mut self) -> Vec<&mut dyn Node> {
    vec![&mut self.prototype]
  }
}

#[derive(Hash, Eq, PartialEq, Debug)]
pub struct Function<'a> {
  pub is_public: bool,
  pub prototype: Prototype,
  pub body: Block<'a>,
}

impl<'a> Node for Function<'a> {
  fn get_children(&mut self) -> Vec<&mut dyn Node> {
    vec![&mut self.prototype, &mut self.body]
  }

  fn type_check(&self) -> Option<Vec<diagnostic::Diagnostic>> {
    type_check::type_check_function(self)
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
  fn get_children(&mut self) -> Vec<&mut dyn Node> {
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

impl<'a> Node for Module<'a> {
  fn get_children(&mut self) -> Vec<&mut dyn Node> {
    let mut children = vec![];

    for (_, value) in &mut self.symbol_table {
      children.push(match value {
        TopLevelNodeHolder::Function(function) => function as &mut dyn Node,
        TopLevelNodeHolder::External(external) => external as &mut dyn Node,
      });
    }

    children
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
  fn get_children(&mut self) -> Vec<&mut dyn Node> {
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
  fn get_children(&mut self) -> Vec<&mut dyn Node> {
    vec![&mut self.block]
  }
}

#[derive(Hash, Eq, PartialEq, Debug)]
pub struct BreakStmt {
  //
}

impl Node for BreakStmt {
  //
}

#[derive(Hash, Eq, PartialEq, Debug)]
pub struct ReturnStmt<'a> {
  pub value: Option<ExprHolder<'a>>,
}

impl<'a> Node for ReturnStmt<'a> {
  fn get_children(&mut self) -> Vec<&mut dyn Node> {
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
  fn get_children(&mut self) -> Vec<&mut dyn Node> {
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
  fn get_children(&mut self) -> Vec<&mut dyn Node> {
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
  fn get_children(&mut self) -> Vec<&mut dyn Node> {
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

impl<'a> Node for CalleeStub<'_> {
  fn get_children(&mut self) -> Vec<&mut dyn Node> {
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
  //
}
