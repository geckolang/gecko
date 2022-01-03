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
    // TODO: Implement.
    ast::Node::VariableRef(_variable_ref) => todo!(),
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
  fn type_check<'ctx>(&self, ty_context: &mut TypeCheckContext, context: &mut context::Context);
}

impl TypeCheck for ast::Node {
  fn type_check<'ctx>(&self, ty_context: &mut TypeCheckContext, context: &mut context::Context) {
    dispatch!(self, TypeCheck::type_check, ty_context, context);
  }
}

impl TypeCheck for ast::UnsafeBlock {
  fn type_check<'ctx>(&self, ty_context: &mut TypeCheckContext, context: &mut context::Context) {
    // TODO: To avoid problems with nested cases, save a buffer here, then restore.
    ty_context.in_unsafe_block = true;
    self.0.type_check(ty_context, context);
    ty_context.in_unsafe_block = false;
  }
}

impl TypeCheck for ast::Extern {
  fn type_check<'ctx>(&self, _ty_context: &mut TypeCheckContext, _context: &mut context::Context) {
    // TODO: Implement.
  }
}

impl TypeCheck for ast::Parameter {
  fn type_check<'ctx>(&self, _ty_context: &mut TypeCheckContext, _context: &mut context::Context) {
    // TODO: Implement.
    todo!();
  }
}

impl TypeCheck for ast::Block {
  fn type_check<'ctx>(&self, ty_context: &mut TypeCheckContext, context: &mut context::Context) {
    for statement in &self.statements {
      statement.type_check(ty_context, context);
    }
  }
}

impl TypeCheck for ast::VariableRef {
  fn type_check<'ctx>(&self, _ty_context: &mut TypeCheckContext, _context: &mut context::Context) {
    // TODO: Implement.
  }
}

impl TypeCheck for ast::Literal {
  fn type_check<'ctx>(&self, _ty_context: &mut TypeCheckContext, _context: &mut context::Context) {
    //
  }
}

impl TypeCheck for ast::IfStmt {
  fn type_check<'ctx>(&self, ty_context: &mut TypeCheckContext, context: &mut context::Context) {
    if infer_type_of(&self.condition, context) != Some(ast::PrimitiveType::Bool) {
      ty_context
        .diagnostics
        .error("if statement condition must evaluate to a boolean".to_string());
    }
  }
}

impl TypeCheck for ast::BinaryExpr {
  fn type_check<'ctx>(&self, _ty_context: &mut TypeCheckContext, _context: &mut context::Context) {
    // TODO: Implement.
  }
}

impl TypeCheck for ast::BreakStmt {
  fn type_check<'ctx>(&self, ty_context: &mut TypeCheckContext, _context: &mut context::Context) {
    if !ty_context.in_loop {
      ty_context
        .diagnostics
        .error("break statement may only occur inside loops".to_string());
    }
  }
}

impl TypeCheck for ast::Definition {
  fn type_check<'ctx>(&self, ty_context: &mut TypeCheckContext, context: &mut context::Context) {
    self.node.borrow().type_check(ty_context, context);
  }
}

impl TypeCheck for ast::ExprWrapperStmt {
  fn type_check<'ctx>(&self, ty_context: &mut TypeCheckContext, context: &mut context::Context) {
    self.expr.type_check(ty_context, context);
  }
}

impl TypeCheck for ast::LetStmt {
  fn type_check<'ctx>(&self, _ty_context: &mut TypeCheckContext, _context: &mut context::Context) {
    // TODO: Implement.
  }
}

impl TypeCheck for ast::ReturnStmt {
  fn type_check<'ctx>(&self, ty_context: &mut TypeCheckContext, _context: &mut context::Context) {
    if ty_context.does_function_return && self.value.is_none() {
      ty_context
        .diagnostics
        .error("return statement must have a value".to_string());
    } else if !ty_context.does_function_return && self.value.is_some() {
      ty_context
        .diagnostics
        .error("return statement must not have a value".to_string());
    }
  }
}

impl TypeCheck for ast::Function {
  fn type_check<'ctx>(&self, ty_context: &mut TypeCheckContext, context: &mut context::Context) {
    ty_context.does_function_return = match &self.prototype {
      ast::Type::Prototype(_, return_type, _) => return_type.is_some(),
      _ => unreachable!(),
    };

    // TODO: Type-check parameters?
    self.body.type_check(ty_context, context);

    // TODO: If it must return value, ensure a value was returned.
  }
}

impl TypeCheck for ast::FunctionCall {
  fn type_check<'ctx>(&self, ty_context: &mut TypeCheckContext, context: &mut context::Context) {
    // TODO: Need access to the current function.
    // TODO: Ensure externs and unsafe function are only called from unsafe functions.

    let callee = context
      .declarations
      .get(self.callee_definition_key.as_ref().unwrap())
      .unwrap();

    // TODO: Continue implementation.
    match *callee.as_ref().borrow() {
      ast::Node::Extern(_) => {
        if !ty_context.in_unsafe_block {
          ty_context
            .diagnostics
            .error("extern function calls may only occur inside an unsafe block".to_string());
        }
      }
      _ => {}
    };
  }
}

impl TypeCheck for ast::WhileStmt {
  fn type_check<'ctx>(&self, ty_context: &mut TypeCheckContext, context: &mut context::Context) {
    // TODO: To avoid problems with nested cases, save a buffer here, then restore.
    ty_context.in_loop = true;
    self.condition.type_check(ty_context, context);
    self.body.type_check(ty_context, context);
    ty_context.in_loop = false;
  }
}
