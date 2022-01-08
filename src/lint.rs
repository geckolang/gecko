use crate::{ast, context, diagnostic};

pub trait Lint {
  fn lint(&self, _context: &mut context::Context, _lint_context: &mut LintContext) {
    //
  }
}

pub struct LintContext {
  pub diagnostics: diagnostic::DiagnosticBuilder,
  did_block_return: bool,
}

impl LintContext {
  pub fn new() -> Self {
    Self {
      diagnostics: diagnostic::DiagnosticBuilder::new(),
      did_block_return: false,
    }
  }

  fn lint_name(&mut self, subject: &str, name: &str, case: convert_case::Case) {
    use convert_case::Casing;

    let case_name = match case {
      convert_case::Case::Snake => "snake",
      convert_case::Case::Pascal => "pascal",
      _ => unreachable!(),
    };

    if !name.is_case(case) {
      self.diagnostics.warning(format!(
        "{} name `{}` should be written in {} case",
        subject, name, case_name
      ))
    }
  }
}

impl Lint for ast::Node {
  fn lint(&self, context: &mut context::Context, lint_context: &mut LintContext) {
    crate::dispatch!(self, Lint::lint, context, lint_context);
  }
}

impl Lint for ast::ArrayAssignStmt {
  fn lint(&self, context: &mut context::Context, lint_context: &mut LintContext) {
    self.value.lint(context, lint_context);
  }
}

impl Lint for ast::ArrayIndexing {
  fn lint(&self, context: &mut context::Context, lint_context: &mut LintContext) {
    self.index.lint(context, lint_context);
  }
}

impl Lint for ast::ArrayValue {
  fn lint(&self, context: &mut context::Context, lint_context: &mut LintContext) {
    for element in &self.elements {
      element.lint(context, lint_context);
    }
  }
}

impl Lint for ast::BinaryExpr {
  fn lint(&self, context: &mut context::Context, lint_context: &mut LintContext) {
    self.left.lint(context, lint_context);
    self.right.lint(context, lint_context);
  }
}

impl Lint for ast::Block {
  fn lint(&self, context: &mut context::Context, lint_context: &mut LintContext) {
    lint_context.did_block_return = false;

    if self.statements.is_empty() {
      lint_context.diagnostics.warning("empty block".to_string());
    }

    for statement in &self.statements {
      if lint_context.did_block_return {
        lint_context
          .diagnostics
          .warning("unreachable code after return statement".to_string());

        // TODO: Consider whether we should stop linking the block at this point.
      }

      statement.lint(context, lint_context);
    }
  }
}

impl Lint for ast::BreakStmt {
  //
}

impl Lint for ast::ContinueStmt {
  //
}

impl Lint for ast::Definition {
  fn lint(&self, context: &mut context::Context, lint_context: &mut LintContext) {
    self.node.borrow().lint(context, lint_context);
  }
}

impl Lint for ast::Enum {
  fn lint(&self, _context: &mut context::Context, lint_context: &mut LintContext) {
    lint_context.lint_name("enum", &self.name, convert_case::Case::Pascal);

    for variant in &self.variants {
      lint_context.lint_name("enum variant", &variant, convert_case::Case::Pascal);
    }
  }
}

impl Lint for ast::ExprStmt {
  fn lint(&self, _context: &mut context::Context, lint_context: &mut LintContext) {
    match self.expr.as_ref() {
      ast::Node::FunctionCall(_) => {}
      _ => lint_context
        .diagnostics
        .warning("expression may be redundant".to_string()),
    };
  }
}

impl Lint for ast::Extern {
  // NOTE: There are no naming rules for externs.
}

impl Lint for ast::Function {
  fn lint(&self, context: &mut context::Context, lint_context: &mut LintContext) {
    self.body.lint(context, lint_context);
  }
}

impl Lint for ast::FunctionCall {
  // TODO: Consider warning against large amounts of arguments.
}

impl Lint for ast::IfStmt {
  fn lint(&self, context: &mut context::Context, lint_context: &mut LintContext) {
    self.condition.lint(context, lint_context);
    self.then_block.lint(context, lint_context);

    if let Some(else_block) = &self.else_block {
      else_block.lint(context, lint_context);
    }
  }
}

impl Lint for ast::LetStmt {
  fn lint(&self, context: &mut context::Context, lint_context: &mut LintContext) {
    self.value.lint(context, lint_context);
    lint_context.lint_name("variable", &self.name, convert_case::Case::Snake);
  }
}

impl Lint for ast::Literal {
  //
}

impl Lint for ast::Parameter {
  fn lint(&self, _context: &mut context::Context, lint_context: &mut LintContext) {
    lint_context.lint_name("parameter", &self.0, convert_case::Case::Snake);
  }
}

impl Lint for ast::ReturnStmt {
  fn lint(&self, context: &mut context::Context, lint_context: &mut LintContext) {
    lint_context.did_block_return = true;

    if let Some(value) = &self.value {
      value.lint(context, lint_context);
    }
  }
}

impl Lint for ast::UnsafeBlockStmt {
  fn lint(&self, context: &mut context::Context, lint_context: &mut LintContext) {
    self.0.lint(context, lint_context);
  }
}

impl Lint for ast::VariableAssignStmt {
  fn lint(&self, context: &mut context::Context, lint_context: &mut LintContext) {
    self.value.lint(context, lint_context);
  }
}

impl Lint for ast::VariableRef {
  //
}

impl Lint for ast::WhileStmt {
  fn lint(&self, context: &mut context::Context, lint_context: &mut LintContext) {
    self.condition.lint(context, lint_context);
    self.body.lint(context, lint_context);
  }
}
