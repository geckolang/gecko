use crate::{ast, context, diagnostic, dispatch};

pub type TypeCheckResult = Option<Vec<diagnostic::Diagnostic>>;

// TODO: Consider making this an optional function under the `TypeCheck` trait?
fn infer_type_of(
  node: &Box<ast::Node>,
  context: &mut context::Context,
) -> Option<ast::PrimitiveType> {
  match node.as_ref() {
    // NOTE: Binary expressions are checked recursively, to have all
    // its sub-expressions be the same type. In other words, we just
    // need to check any single sub-expression.
    // FIXME: This is wrong in our context. We're using this to check for booleans.
    ast::Node::BinaryExpr(binary_expr) => infer_type_of(&binary_expr.left, context),
    ast::Node::Literal(ast::Literal::Bool(_)) => Some(ast::PrimitiveType::Bool),
    ast::Node::Literal(ast::Literal::Char(_)) => Some(ast::PrimitiveType::Char),
    ast::Node::Literal(ast::Literal::Int(_, size)) => Some(ast::PrimitiveType::Int(size.clone())),
    ast::Node::Literal(ast::Literal::String(_)) => Some(ast::PrimitiveType::String),
    ast::Node::VariableRef(variable_ref) => {
      let target_variable = &*context
        .declarations
        .get(&variable_ref.definition_key.unwrap())
        .unwrap()
        .as_ref()
        .borrow();

      // TODO: Simplify code.
      let variable_type = match target_variable {
        ast::Node::LetStmt(let_stmt) => &let_stmt.ty,
        ast::Node::Parameter(parameter) => &parameter.1,
        _ => unreachable!(),
      };

      match variable_type {
        ast::Type::PrimitiveType(primitive_type) => Some(primitive_type.clone()),
        _ => unreachable!(),
      }
    }
    ast::Node::FunctionCall(function_call) => {
      // TODO: Is this cloning? Simplify this messy code section.
      let function_or_extern = &*context
        .declarations
        .get(&function_call.callee_definition_key.unwrap())
        .unwrap()
        .as_ref()
        .borrow();

      let prototype = match function_or_extern {
        ast::Node::Function(function) => &function.prototype,
        ast::Node::Extern(extern_) => &extern_.prototype,
        _ => unreachable!(),
      };

      // TODO: Simplify this process.
      match prototype {
        ast::Type::Prototype(_, return_type, _) => {
          if return_type.is_none() {
            None
          } else {
            match return_type.as_ref().unwrap().as_ref() {
              ast::Type::PrimitiveType(primitive_type) => Some(primitive_type.clone()),
              _ => unreachable!(),
            }
          }
        }
        _ => unreachable!(),
      }
    }
    _ => unreachable!(),
  }
}

pub struct TypeCheckContext {
  pub diagnostics: diagnostic::DiagnosticBuilder,
  in_loop: bool,
  in_unsafe_block: bool,
  does_function_return: bool,
}

impl TypeCheckContext {
  pub fn new() -> Self {
    Self {
      diagnostics: diagnostic::DiagnosticBuilder::new(),
      in_loop: false,
      in_unsafe_block: false,
      does_function_return: false,
    }
  }
}

pub trait TypeCheck {
  fn type_check<'ctx>(
    &self,
    _type_context: &mut TypeCheckContext,
    _context: &mut context::Context,
  ) {
    //
  }
}

impl TypeCheck for ast::Node {
  fn type_check<'ctx>(&self, type_context: &mut TypeCheckContext, context: &mut context::Context) {
    dispatch!(self, TypeCheck::type_check, type_context, context);
  }
}

impl TypeCheck for ast::UnsafeBlock {
  fn type_check<'ctx>(&self, type_context: &mut TypeCheckContext, context: &mut context::Context) {
    // TODO: To avoid problems with nested cases, save a buffer here, then restore?
    type_context.in_unsafe_block = true;
    self.0.type_check(type_context, context);
    type_context.in_unsafe_block = false;
  }
}

impl TypeCheck for ast::Extern {
  //
}

impl TypeCheck for ast::Parameter {
  //
}

impl TypeCheck for ast::Block {
  fn type_check<'ctx>(&self, type_context: &mut TypeCheckContext, context: &mut context::Context) {
    for statement in &self.statements {
      statement.type_check(type_context, context);
    }
  }
}

impl TypeCheck for ast::VariableRef {
  //
}

impl TypeCheck for ast::Literal {
  //
}

impl TypeCheck for ast::IfStmt {
  fn type_check<'ctx>(&self, type_context: &mut TypeCheckContext, context: &mut context::Context) {
    if infer_type_of(&self.condition, context) != Some(ast::PrimitiveType::Bool) {
      type_context
        .diagnostics
        .error("if statement condition must evaluate to a boolean".to_string());
    }
  }
}

impl TypeCheck for ast::BinaryExpr {
  fn type_check<'ctx>(&self, type_context: &mut TypeCheckContext, context: &mut context::Context) {
    self.left.type_check(type_context, context);
    self.right.type_check(type_context, context);

    let left_type = infer_type_of(&self.left, context);
    let right_type = infer_type_of(&self.right, context);

    // TODO: Also add checks for when using operators with wrong values (ex. less-than or greater-than comparison of booleans).

    // FIXME: Does this equality check work as expected? Research & ensure.
    if left_type != right_type {
      type_context
        .diagnostics
        .error("binary expression operands must be the same type".to_string());

      return;
    }

    // TODO: Check for mixed operators that don't make sense (ex. addition, then a comparison operator).

    // NOTE: By this point, it is assumed that both operands are of the same type.
    match self.operator {
      ast::OperatorKind::Add
      | ast::OperatorKind::Subtract
      | ast::OperatorKind::Multiply
      | ast::OperatorKind::Divide
      | ast::OperatorKind::LessThan
      | ast::OperatorKind::GreaterThan => {
        // TODO: What about floats?
        if !matches!(left_type, Some(ast::PrimitiveType::Int(_))) {
          type_context
            .diagnostics
            .error("binary expression operands must be both integers".to_string());
        }
      }
      // TODO: Equality operator, and others?
      _ => todo!(),
    }
  }
}

impl TypeCheck for ast::BreakStmt {
  fn type_check<'ctx>(&self, type_context: &mut TypeCheckContext, _context: &mut context::Context) {
    if !type_context.in_loop {
      type_context
        .diagnostics
        .error("break statement may only occur inside loops".to_string());
    }
  }
}

impl TypeCheck for ast::Definition {
  fn type_check<'ctx>(&self, type_context: &mut TypeCheckContext, context: &mut context::Context) {
    self.node.borrow().type_check(type_context, context);
  }
}

impl TypeCheck for ast::ExprWrapperStmt {
  fn type_check<'ctx>(&self, type_context: &mut TypeCheckContext, context: &mut context::Context) {
    self.expr.type_check(type_context, context);
  }
}

impl TypeCheck for ast::LetStmt {
  fn type_check<'ctx>(&self, type_context: &mut TypeCheckContext, context: &mut context::Context) {
    self.value.type_check(type_context, context);

    let self_type = Some(match &self.ty {
      ast::Type::PrimitiveType(primitive_type) => primitive_type.clone(),
      _ => unreachable!(),
    });

    let value_type = infer_type_of(&self.value, context);

    // FIXME: Ensure this comparison works as expected.
    if self_type != value_type {
      type_context
        .diagnostics
        .error("let statement value and type mismatch".to_string());
    }
  }
}

impl TypeCheck for ast::ReturnStmt {
  fn type_check<'ctx>(&self, type_context: &mut TypeCheckContext, _context: &mut context::Context) {
    if type_context.does_function_return && self.value.is_none() {
      type_context
        .diagnostics
        .error("return statement must have a value".to_string());
    } else if !type_context.does_function_return && self.value.is_some() {
      type_context
        .diagnostics
        .error("return statement must not have a value".to_string());
    }
  }
}

impl TypeCheck for ast::Function {
  fn type_check<'ctx>(&self, type_context: &mut TypeCheckContext, context: &mut context::Context) {
    type_context.does_function_return = match &self.prototype {
      ast::Type::Prototype(_, return_type, _) => return_type.is_some(),
      _ => unreachable!(),
    };

    // TODO: Type-check parameters?
    self.body.type_check(type_context, context);

    // TODO: If it must return value, ensure a value was returned.
  }
}

impl TypeCheck for ast::FunctionCall {
  fn type_check<'ctx>(&self, type_context: &mut TypeCheckContext, context: &mut context::Context) {
    // TODO: Need access to the current function.
    // TODO: Ensure externs and unsafe function are only called from unsafe functions.

    let callee = context
      .declarations
      .get(self.callee_definition_key.as_ref().unwrap())
      .unwrap();

    // TODO: Continue implementation.
    match *callee.as_ref().borrow() {
      ast::Node::Extern(_) => {
        if !type_context.in_unsafe_block {
          type_context
            .diagnostics
            .error("extern function calls may only occur inside an unsafe block".to_string());
        }
      }
      _ => {}
    };
  }
}

impl TypeCheck for ast::WhileStmt {
  fn type_check<'ctx>(&self, type_context: &mut TypeCheckContext, context: &mut context::Context) {
    // TODO: To avoid problems with nested cases, save a buffer here, then restore.
    type_context.in_loop = true;
    self.condition.type_check(type_context, context);
    self.body.type_check(type_context, context);
    type_context.in_loop = false;
  }
}
