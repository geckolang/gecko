use crate::{ast, cache, diagnostic, llvm_lowering};

pub trait Lint {
  fn lint(&self, _cache: &mut cache::Cache, _context: &mut LintContext) {
    //
  }
}

pub struct LintContext {
  pub diagnostic_builder: diagnostic::DiagnosticBuilder,
  block_depth: usize,
  function_references: std::collections::HashMap<cache::UniqueId, bool>,
  variable_references: std::collections::HashMap<cache::UniqueId, bool>,
}

impl LintContext {
  pub fn new() -> Self {
    Self {
      diagnostic_builder: diagnostic::DiagnosticBuilder::new(),
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

      let function_or_extern_node = cache.get(&function_key);

      let name = match &*function_or_extern_node {
        ast::NodeKind::Function(function) => &function.name,
        ast::NodeKind::ExternFunction(extern_) => &extern_.name,
        _ => unreachable!(),
      };

      if name != llvm_lowering::MAIN_FUNCTION_NAME {
        self
          .diagnostic_builder
          .warning(format!("function `{}` is never called", name));
      }
    }

    for (variable_def_key, was_used) in &self.variable_references {
      if *was_used {
        continue;
      }

      let variable_def_node = cache.get(&variable_def_key);

      let name = match &*variable_def_node {
        ast::NodeKind::LetStmt(let_stmt) => &let_stmt.name,
        _ => unreachable!(),
      };

      self
        .diagnostic_builder
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
      self.diagnostic_builder.warning(format!(
        "{} name `{}` should be written in {} case",
        subject, name, case_name
      ));
    }
  }
}

impl Lint for ast::NodeKind {
  fn lint(&self, cache: &mut cache::Cache, lint_context: &mut LintContext) {
    crate::dispatch!(self, Lint::lint, cache, lint_context);
  }
}

impl Lint for ast::Closure {
  //
}

impl Lint for ast::TypeAlias {
  //
}

impl Lint for ast::Pattern {
  fn lint(&self, _cache: &mut cache::Cache, _lint_context: &mut LintContext) {
    // TODO: Lint name(s).
  }
}

impl Lint for ast::IntrinsicCall {
  //
}

impl Lint for ast::ExternStatic {
  fn lint(&self, _cache: &mut cache::Cache, _context: &mut LintContext) {
    //
  }
}

impl Lint for ast::StructValue {
  //
}

impl Lint for ast::Prototype {
  //
}

impl Lint for ast::StructType {
  fn lint(&self, _cache: &mut cache::Cache, context: &mut LintContext) {
    context.lint_name_casing("struct", &self.name, convert_case::Case::Pascal);

    // TODO: Any more linting?
  }
}

impl Lint for ast::UnaryExpr {
  fn lint(&self, cache: &mut cache::Cache, context: &mut LintContext) {
    self.expr.kind.lint(cache, context);
  }
}

impl Lint for ast::ArrayIndexing {
  fn lint(&self, cache: &mut cache::Cache, context: &mut LintContext) {
    context
      .variable_references
      .insert(self.target_key.unwrap(), true);

    self.index_expr.kind.lint(cache, context);
  }
}

impl Lint for ast::ArrayValue {
  fn lint(&self, cache: &mut cache::Cache, context: &mut LintContext) {
    for element in &self.elements {
      element.kind.lint(cache, context);
    }
  }
}

impl Lint for ast::BinaryExpr {
  fn lint(&self, cache: &mut cache::Cache, context: &mut LintContext) {
    self.left.kind.lint(cache, context);
    self.right.kind.lint(cache, context);
  }
}

impl Lint for ast::Block {
  fn lint(&self, cache: &mut cache::Cache, context: &mut LintContext) {
    let mut did_return = false;

    // TODO: Might be repetitive for subsequent nested blocks.
    if context.block_depth > 4 {
      context
        .diagnostic_builder
        .warning("block depth is deeper than 4".to_string());
    }

    context.block_depth += 1;

    for statement in &self.statements {
      if did_return {
        context
          .diagnostic_builder
          .warning("unreachable code after return statement".to_string());

        // TODO: Consider whether we should stop linting the block at this point.
      }

      if matches!(statement.kind, ast::NodeKind::ReturnStmt(_)) {
        did_return = true;
      }

      statement.kind.lint(cache, context);
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
    let node = &*self.node_ref_cell.borrow();

    // TODO: Simplify. Abstract the map, then process.
    match node {
      ast::NodeKind::Function(_) => {
        if !context
          .function_references
          .contains_key(&self.definition_key)
        {
          context
            .function_references
            .insert(self.definition_key, false);
        }
      }
      ast::NodeKind::LetStmt(_) => {
        // NOTE: Variable declarations always occur before their usage.
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
      context.diagnostic_builder.warning("empty enum".to_string());
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

impl Lint for ast::InlineExprStmt {
  fn lint(&self, cache: &mut cache::Cache, context: &mut LintContext) {
    self.expr.kind.lint(cache, context);
  }
}

impl Lint for ast::ExternFunction {
  // NOTE: There are no naming rules for externs.
}

impl Lint for ast::Function {
  fn lint(&self, cache: &mut cache::Cache, context: &mut LintContext) {
    context.lint_name_casing("function", &self.name, convert_case::Case::Snake);

    if self.prototype.parameters.len() > 4 {
      context
        .diagnostic_builder
        .warning("function has more than 4 parameters".to_string());
    }

    self.body.lint(cache, context);
  }
}

impl Lint for ast::CallExpr {
  fn lint(&self, _cache: &mut cache::Cache, context: &mut LintContext) {
    // FIXME:
    // context
    //   .function_references
    //   .insert(self.callee_expr.target_key.unwrap(), true);
  }
}

impl Lint for ast::IfStmt {
  fn lint(&self, cache: &mut cache::Cache, context: &mut LintContext) {
    // TODO: In the future, binary conditions should also be evaluated (if using literals on both operands).
    // TODO: Add a helper method to "unbox" expressions? (e.g. case for `(true)`).
    if matches!(self.condition.kind, ast::NodeKind::Literal(_)) {
      context
        .diagnostic_builder
        .warning("if condition is a constant expression".to_string());
    }

    if let Some(else_block) = &self.else_block {
      else_block.lint(cache, context);
    }

    self.condition.kind.lint(cache, context);
    self.then_block.lint(cache, context);
  }
}

impl Lint for ast::LetStmt {
  fn lint(&self, cache: &mut cache::Cache, context: &mut LintContext) {
    context.lint_name_casing("variable", &self.name, convert_case::Case::Snake);
    self.value.kind.lint(cache, context);
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
      value.kind.lint(cache, context);
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
    self.value.kind.lint(cache, context);
  }
}

impl Lint for ast::Reference {
  fn lint(&self, _cache: &mut cache::Cache, context: &mut LintContext) {
    context
      .variable_references
      .insert(self.0.target_key.unwrap(), true);
  }
}

impl Lint for ast::LoopStmt {
  fn lint(&self, cache: &mut cache::Cache, context: &mut LintContext) {
    if let Some(condition) = &self.condition {
      condition.kind.lint(cache, context);
    }

    self.body.lint(cache, context);
  }
}
