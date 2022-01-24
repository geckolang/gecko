use crate::{ast, cache, diagnostic, llvm_lowering};

pub trait Lint {
  fn lint(&self, _cache: &mut cache::Cache, _context: &mut LintContext) {
    //
  }
}

pub struct LintContext {
  pub diagnostics: diagnostic::DiagnosticBuilder,
  block_depth: usize,
  function_references: std::collections::HashMap<cache::DefinitionKey, bool>,
  variable_references: std::collections::HashMap<cache::DefinitionKey, bool>,
}

impl LintContext {
  pub fn new() -> Self {
    Self {
      diagnostics: diagnostic::DiagnosticBuilder::new(),
      block_depth: 0,
      function_references: std::collections::HashMap::new(),
      variable_references: std::collections::HashMap::new(),
    }
  }

  pub fn finalize(&mut self, cache: &cache::Cache) {
    for (function_key, was_called) in &self.function_references {
      // TODO: Dereferencing value.
      if *was_called {
        continue;
      }

      let function_node = cache.declarations[&function_key].as_ref().borrow();

      let name = match &*function_node {
        ast::Node::Function(function) => &function.name,
        ast::Node::Extern(extern_) => &extern_.name,
        _ => unreachable!(),
      };

      if name != llvm_lowering::MAIN_FUNCTION_NAME {
        self
          .diagnostics
          .warning(format!("function `{}` is never called", name));
      }
    }

    for (variable_def_key, was_used) in &self.variable_references {
      // TODO: Dereferencing value.
      if *was_used {
        continue;
      }

      let variable_def_node = cache.declarations[&variable_def_key].as_ref().borrow();

      let name = match &*variable_def_node {
        ast::Node::LetStmt(let_stmt) => &let_stmt.name,
        _ => unreachable!(),
      };

      self
        .diagnostics
        .warning(format!("variable `{}` is never used", name));
    }
  }

  fn lint_name_casing(&mut self, subject: &str, name: &str, case: convert_case::Case) {
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
  fn lint(&self, context: &mut cache::Cache, lint_context: &mut LintContext) {
    crate::dispatch!(self, Lint::lint, context, lint_context);
  }
}

impl Lint for ast::StructValue {
  //
}

impl Lint for ast::Prototype {
  //
}

impl Lint for ast::StructType {
  // TODO: Implement.
}

impl Lint for ast::UnaryExpr {
  fn lint(&self, cache: &mut cache::Cache, context: &mut LintContext) {
    self.expr.lint(cache, context);
  }
}

impl Lint for ast::ArrayIndexing {
  fn lint(&self, cache: &mut cache::Cache, context: &mut LintContext) {
    context
      .variable_references
      .insert(self.target_key.unwrap(), true);

    self.index.lint(cache, context);
  }
}

impl Lint for ast::ArrayValue {
  fn lint(&self, cache: &mut cache::Cache, context: &mut LintContext) {
    for element in &self.elements {
      element.lint(cache, context);
    }
  }
}

impl Lint for ast::BinaryExpr {
  fn lint(&self, cache: &mut cache::Cache, context: &mut LintContext) {
    self.left.lint(cache, context);
    self.right.lint(cache, context);
  }
}

impl Lint for ast::Block {
  fn lint(&self, cache: &mut cache::Cache, context: &mut LintContext) {
    let mut did_return = false;

    if self.statements.is_empty() {
      context.diagnostics.warning("empty block".to_string());
    }

    // TODO: Might be repetitive for subsequent nested blocks.
    if context.block_depth > 4 {
      context
        .diagnostics
        .warning("block depth is deeper than 4".to_string());
    }

    context.block_depth += 1;

    for statement in &self.statements {
      if did_return {
        context
          .diagnostics
          .warning("unreachable code after return statement".to_string());

        // TODO: Consider whether we should stop linting the block at this point.
      }

      if matches!(statement.as_ref(), ast::Node::ReturnStmt(_)) {
        did_return = true;
      }

      statement.lint(cache, context);
    }

    context.block_depth -= 1;
  }
}

impl Lint for ast::BreakStmt {
  //
}

impl Lint for ast::ContinueStmt {
  //
}

impl Lint for ast::Definition {
  fn lint(&self, cache: &mut cache::Cache, context: &mut LintContext) {
    let node = &*self.node.borrow();

    match node {
      ast::Node::Function(_) => {
        context
          .function_references
          .insert(self.definition_key, false);
      }
      ast::Node::LetStmt(_) => {
        context
          .variable_references
          .insert(self.definition_key, false);
      }
      // TODO: Lint other definitions.
      _ => {}
    };

    node.lint(cache, context);
  }
}

impl Lint for ast::Enum {
  fn lint(&self, _cache: &mut cache::Cache, context: &mut LintContext) {
    context.lint_name_casing("enum", &self.name, convert_case::Case::Pascal);

    if self.variants.is_empty() {
      context.diagnostics.warning("empty enum".to_string());
    }

    for variant in &self.variants {
      context.lint_name_casing(
        format!("enum `{}` variant", &self.name).as_str(),
        &variant,
        convert_case::Case::Pascal,
      );
    }
  }
}

impl Lint for ast::ExprStmt {
  fn lint(&self, cache: &mut cache::Cache, context: &mut LintContext) {
    match self.expr.as_ref() {
      ast::Node::FunctionCall(_) => {}
      _ => context
        .diagnostics
        .warning("expression may be redundant".to_string()),
    };

    self.expr.lint(cache, context);
  }
}

impl Lint for ast::Extern {
  // NOTE: There are no naming rules for externs.
}

impl Lint for ast::Function {
  fn lint(&self, cache: &mut cache::Cache, context: &mut LintContext) {
    context.lint_name_casing("function", &self.name, convert_case::Case::Snake);

    if self.prototype.parameters.len() > 4 {
      context
        .diagnostics
        .warning("function has more than 4 parameters".to_string());
    }

    self.body.lint(cache, context);
  }
}

impl Lint for ast::FunctionCall {
  fn lint(&self, _cache: &mut cache::Cache, context: &mut LintContext) {
    context
      .function_references
      .insert(self.target_key.unwrap(), true);
  }
}

impl Lint for ast::IfStmt {
  fn lint(&self, cache: &mut cache::Cache, context: &mut LintContext) {
    // TODO: In the future, binary conditions should also be evaluated (if using literals on both operands).
    if matches!(self.condition.as_ref(), ast::Node::Literal(_)) {
      context
        .diagnostics
        .warning("if condition is constant".to_string());
    }

    if let Some(else_block) = &self.else_block {
      else_block.lint(cache, context);
    }

    self.condition.lint(cache, context);
    self.then_block.lint(cache, context);
  }
}

impl Lint for ast::LetStmt {
  fn lint(&self, cache: &mut cache::Cache, context: &mut LintContext) {
    context.lint_name_casing("variable", &self.name, convert_case::Case::Snake);
    self.value.lint(cache, context);
  }
}

impl Lint for ast::Literal {
  //
}

impl Lint for ast::Parameter {
  fn lint(&self, _cache: &mut cache::Cache, context: &mut LintContext) {
    context.lint_name_casing("parameter", &self.0, convert_case::Case::Snake);
  }
}

impl Lint for ast::ReturnStmt {
  fn lint(&self, cache: &mut cache::Cache, context: &mut LintContext) {
    if let Some(value) = &self.value {
      value.lint(cache, context);
    }
  }
}

impl Lint for ast::UnsafeBlockStmt {
  fn lint(&self, cache: &mut cache::Cache, context: &mut LintContext) {
    self.0.lint(cache, context);
  }
}

impl Lint for ast::AssignStmt {
  fn lint(&self, cache: &mut cache::Cache, context: &mut LintContext) {
    self.value.lint(cache, context);
  }
}

impl Lint for ast::VariableRef {
  fn lint(&self, _cache: &mut cache::Cache, context: &mut LintContext) {
    context
      .variable_references
      .insert(self.target_key.unwrap(), true);
  }
}

impl Lint for ast::WhileStmt {
  fn lint(&self, cache: &mut cache::Cache, context: &mut LintContext) {
    self.condition.lint(cache, context);
    self.body.lint(cache, context);
  }
}
