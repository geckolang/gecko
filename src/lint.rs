use crate::{ast, cache, diagnostic};

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

  pub fn finalize(&mut self, _cache: &cache::Cache) {
    // TODO: Re-implement.
  }

  /// Attempt to lint a name and subject with the provided casing.
  ///
  /// If there isn't a name for the specified casing, `unknown` will
  /// be used instead. The supported casings are limited to only snake
  /// and pascal casing.
  fn lint_name_casing(&mut self, subject: &str, name: &str, case: convert_case::Case) {
    use convert_case::Casing;

    let case_name = match case {
      convert_case::Case::Snake => "snake",
      convert_case::Case::Pascal => "pascal",
      _ => "unknown",
    };

    if !name.is_case(case) {
      self.diagnostic_builder.warning(format!(
        "{} name `{}` should be written in {} case",
        subject, name, case_name
      ));
    }
  }
}

pub trait Lint {
  fn lint(&self, _cache: &cache::Cache, _context: &mut LintContext) {
    //
  }
}

impl Lint for ast::Node {
  fn lint(&self, cache: &cache::Cache, lint_context: &mut LintContext) {
    // TODO: Here we have access to the node's metadata. Consider using some system to provide it to whatever needs it.
    crate::dispatch!(&self.kind, Lint::lint, cache, lint_context);
  }
}

impl Lint for ast::Trait {
  //
}

impl Lint for ast::StructImpl {
  //
}

impl Lint for ast::MemberAccess {
  //
}

impl Lint for ast::Closure {
  //
}

impl Lint for ast::TypeAlias {
  //
}

impl Lint for ast::Pattern {
  fn lint(&self, _cache: &cache::Cache, _lint_context: &mut LintContext) {
    // TODO: Lint name(s).
  }
}

impl Lint for ast::IntrinsicCall {
  //
}

impl Lint for ast::ExternStatic {
  fn lint(&self, _cache: &cache::Cache, _context: &mut LintContext) {
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
  fn lint(&self, _cache: &cache::Cache, context: &mut LintContext) {
    context.lint_name_casing("struct", &self.name, convert_case::Case::Pascal);

    // TODO: Any more linting?
  }
}

impl Lint for ast::UnaryExpr {
  fn lint(&self, cache: &cache::Cache, context: &mut LintContext) {
    self.expr.lint(cache, context);
  }
}

impl Lint for ast::ArrayIndexing {
  fn lint(&self, cache: &cache::Cache, context: &mut LintContext) {
    context
      .variable_references
      .insert(self.target_id.unwrap(), true);

    self.index_expr.lint(cache, context);
  }
}

impl Lint for ast::ArrayValue {
  fn lint(&self, cache: &cache::Cache, context: &mut LintContext) {
    for element in &self.elements {
      element.lint(cache, context);
    }
  }
}

impl Lint for ast::BinaryExpr {
  fn lint(&self, cache: &cache::Cache, context: &mut LintContext) {
    self.left.lint(cache, context);
    self.right.lint(cache, context);
  }
}

impl Lint for ast::BlockExpr {
  fn lint(&self, cache: &cache::Cache, context: &mut LintContext) {
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

// impl Lint for ast::Definition {
//   fn lint(&self, cache: &cache::Cache, context: &mut LintContext) {
//     // TODO: Simplify. Abstract the map, then process.
//     match self.node.kind {
//       ast::NodeKind::Function(_) => {
//         if !context.function_references.contains_key(&self.unique_id) {
//           context.function_references.insert(self.unique_id, false);
//         }
//       }
//       ast::NodeKind::LetStmt(_) => {
//         // NOTE: Variable declarations always occur before their usage.
//         context.variable_references.insert(self.unique_id, false);
//       }
//       // TODO: Lint other definitions.
//       _ => {}
//     };

//     self.node.lint(cache, context);
//   }
// }

impl Lint for ast::Enum {
  fn lint(&self, _cache: &cache::Cache, context: &mut LintContext) {
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
  fn lint(&self, cache: &cache::Cache, context: &mut LintContext) {
    self.expr.lint(cache, context);
  }
}

impl Lint for ast::ExternFunction {
  // NOTE: There are no naming rules for externs.
}

impl Lint for ast::Function {
  fn lint(&self, cache: &cache::Cache, context: &mut LintContext) {
    context.lint_name_casing("function", &self.name, convert_case::Case::Snake);

    if self.prototype.parameters.len() > 4 {
      context
        .diagnostic_builder
        .warning("function has more than 4 parameters".to_string());
    }

    self.body_value.lint(cache, context);
  }
}

impl Lint for ast::CallExpr {
  fn lint(&self, _cache: &cache::Cache, _context: &mut LintContext) {
    // FIXME:
    // context
    //   .function_references
    //   .insert(self.callee_expr.target_key.unwrap(), true);
  }
}

impl Lint for ast::IfExpr {
  fn lint(&self, cache: &cache::Cache, context: &mut LintContext) {
    // TODO: In the future, binary conditions should also be evaluated (if using literals on both operands).
    // TODO: Add a helper method to "unbox" expressions? (e.g. case for `(true)`).
    if matches!(self.condition.kind, ast::NodeKind::Literal(_)) {
      context
        .diagnostic_builder
        .warning("if condition is a constant expression".to_string());
    }

    if let Some(else_block) = &self.else_value {
      else_block.lint(cache, context);
    }

    self.condition.lint(cache, context);
    self.then_value.lint(cache, context);
  }
}

impl Lint for ast::LetStmt {
  fn lint(&self, cache: &cache::Cache, context: &mut LintContext) {
    context.lint_name_casing("variable", &self.name, convert_case::Case::Snake);
    self.value.lint(cache, context);
  }
}

impl Lint for ast::Literal {
  //
}

impl Lint for ast::Parameter {
  fn lint(&self, _cache: &cache::Cache, context: &mut LintContext) {
    context.lint_name_casing("parameter", &self.name, convert_case::Case::Snake);
  }
}

impl Lint for ast::ReturnStmt {
  fn lint(&self, cache: &cache::Cache, context: &mut LintContext) {
    if let Some(value) = &self.value {
      value.lint(cache, context);
    }
  }
}

impl Lint for ast::UnsafeBlockStmt {
  fn lint(&self, cache: &cache::Cache, context: &mut LintContext) {
    self.0.lint(cache, context);
  }
}

impl Lint for ast::Reference {
  fn lint(&self, _cache: &cache::Cache, context: &mut LintContext) {
    context
      .variable_references
      .insert(self.0.target_id.unwrap(), true);
  }
}

impl Lint for ast::LoopStmt {
  fn lint(&self, cache: &cache::Cache, context: &mut LintContext) {
    if let Some(condition) = &self.condition {
      condition.lint(cache, context);
    }

    self.body.lint(cache, context);
  }
}
