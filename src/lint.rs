use crate::{ast, context, diagnostic};

pub trait Lint {
  fn lint(&self, _context: &mut context::Context, _lint_context: &mut LintContext) {
    //
  }
}

pub struct LintContext {
  pub diagnostics: diagnostic::DiagnosticBuilder,
  block_depth: usize,
  // TODO: No need to count references, with a single one it should be enough.
  function_references: std::collections::HashMap<context::DefinitionKey, usize>,
}

impl LintContext {
  pub fn new() -> Self {
    Self {
      diagnostics: diagnostic::DiagnosticBuilder::new(),
      block_depth: 0,
      function_references: std::collections::HashMap::new(),
    }
  }

  pub fn finalize(&mut self, context: &context::Context) {
    for (function_key, ref_count) in &self.function_references {
      let function_node = context.declarations[&function_key].as_ref().borrow();

      let name = match &*function_node {
        ast::Node::Function(function) => &function.name,
        ast::Node::Extern(extern_) => &extern_.name,
        _ => unreachable!(),
      };

      // TODO: Dereferencing count.
      if *ref_count == 0 {
        self
          .diagnostics
          .warning(format!("function `{}` is never called", name));
      }
    }
  }

  fn increment_function_ref(&mut self, key: context::DefinitionKey) {
    let count = self.function_references.entry(key).or_insert(0);

    *count += 1;
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

impl Lint for ast::StructDef {
  // TODO: Implement.
}

impl Lint for ast::UnaryExpr {
  fn lint(&self, context: &mut context::Context, lint_context: &mut LintContext) {
    self.expr.lint(context, lint_context);
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
    let mut did_return = false;

    if self.statements.is_empty() {
      lint_context.diagnostics.warning("empty block".to_string());
    }

    // TODO: Might be repetitive for subsequent nested blocks.
    if lint_context.block_depth > 4 {
      lint_context
        .diagnostics
        .warning("block depth is deeper than 4".to_string());
    }

    lint_context.block_depth += 1;

    for statement in &self.statements {
      if did_return {
        lint_context
          .diagnostics
          .warning("unreachable code after return statement".to_string());

        // TODO: Consider whether we should stop linting the block at this point.
      }

      if matches!(statement.as_ref(), ast::Node::ReturnStmt(_)) {
        did_return = true;
      }

      statement.lint(context, lint_context);
    }

    lint_context.block_depth -= 1;
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
    let node = &*self.node.borrow();

    if matches!(node, ast::Node::Function(_)) {
      lint_context.function_references.insert(self.key, 0);
    }

    node.lint(context, lint_context);
  }
}

impl Lint for ast::Enum {
  fn lint(&self, _context: &mut context::Context, lint_context: &mut LintContext) {
    lint_context.lint_name("enum", &self.name, convert_case::Case::Pascal);

    if self.variants.is_empty() {
      lint_context.diagnostics.warning("empty enum".to_string());
    }

    for variant in &self.variants {
      lint_context.lint_name(
        format!("enum `{}` variant", &self.name).as_str(),
        &variant,
        convert_case::Case::Pascal,
      );
    }
  }
}

impl Lint for ast::ExprStmt {
  fn lint(&self, context: &mut context::Context, lint_context: &mut LintContext) {
    match self.expr.as_ref() {
      ast::Node::FunctionCall(_) => {}
      _ => lint_context
        .diagnostics
        .warning("expression may be redundant".to_string()),
    };

    self.expr.lint(context, lint_context);
  }
}

impl Lint for ast::Extern {
  // NOTE: There are no naming rules for externs.
}

impl Lint for ast::Function {
  fn lint(&self, context: &mut context::Context, lint_context: &mut LintContext) {
    lint_context.lint_name("function", &self.name, convert_case::Case::Snake);

    match &self.prototype {
      ast::Type::Prototype(parameters, _, _) => {
        if parameters.len() > 4 {
          lint_context
            .diagnostics
            .warning("function has more than 4 parameters".to_string());
        }
      }
      _ => unreachable!(),
    };

    self.body.lint(context, lint_context);
  }
}

impl Lint for ast::FunctionCall {
  fn lint(&self, _context: &mut context::Context, lint_context: &mut LintContext) {
    lint_context.increment_function_ref(self.callee_definition_key.unwrap());
  }
}

impl Lint for ast::IfStmt {
  fn lint(&self, context: &mut context::Context, lint_context: &mut LintContext) {
    // TODO: In the future, binary conditions should also be evaluated (if using literals on both operands).
    if matches!(self.condition.as_ref(), ast::Node::Literal(_)) {
      lint_context
        .diagnostics
        .warning("if condition is constant".to_string());
    }

    if let Some(else_block) = &self.else_block {
      else_block.lint(context, lint_context);
    }

    self.condition.lint(context, lint_context);
    self.then_block.lint(context, lint_context);
  }
}

impl Lint for ast::LetStmt {
  fn lint(&self, context: &mut context::Context, lint_context: &mut LintContext) {
    lint_context.lint_name("variable", &self.name, convert_case::Case::Snake);
    self.value.lint(context, lint_context);
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
