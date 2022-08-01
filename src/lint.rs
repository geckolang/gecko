use crate::{ast, cache, visitor::AnalysisVisitor};

// REVIEW: Why not abstract the error reporting to the lint methods themselves? This is more functional.
// ... Perhaps we can also get rid of variable reference counting, in favor of functional programming.
pub struct LintContext {
  pub diagnostics: Vec<ast::Diagnostic>,
  variable_references: std::collections::HashMap<cache::Id, bool>,
}

// REVISE: Use the `traverse` method to walk the AST and invoke these
// ... methods in conjunction with the other read-only semantic checks.
// ... This will improve performance because we'd be using less passes.
impl LintContext {
  pub fn new() -> Self {
    Self {
      diagnostics: Vec::new(),
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

    if name.starts_with("_") || name.is_case(case) {
      return;
    }

    let case_name = match case {
      convert_case::Case::Snake => "snake",
      convert_case::Case::Pascal => "pascal",
      _ => "unknown",
    };

    self.diagnostics.push(
      codespan_reporting::diagnostic::Diagnostic::warning().with_message(format!(
        "{} name `{}` should be written in {} case",
        subject, name, case_name
      )),
    )
  }
}

impl AnalysisVisitor for LintContext {
  fn visit_pattern(&mut self, _pattern: &ast::Pattern) {
    // TODO: Lint name(s).
  }

  fn visit_struct(&mut self, struct_type: &ast::Struct) {
    self.lint_name_casing("struct", &struct_type.name, convert_case::Case::Pascal);

    // REVIEW: Any more linting needed?
  }

  fn enter_block_expr(&mut self, block: &ast::BlockExpr) {
    let mut did_return = false;

    for statement in &block.statements {
      if did_return {
        self.diagnostics.push(
          codespan_reporting::diagnostic::Diagnostic::warning()
            .with_message("unreachable code after return statement"),
        );

        // REVIEW: Consider whether we should stop linting the block at this point.
      }

      if matches!(statement, ast::NodeKind::ReturnStmt(_)) {
        did_return = true;
      }
    }
  }

  fn visit_enum(&mut self, enum_: &ast::Enum) {
    self.lint_name_casing("enum", &enum_.name, convert_case::Case::Pascal);

    if enum_.variants.is_empty() {
      self
        .diagnostics
        .push(codespan_reporting::diagnostic::Diagnostic::warning().with_message("empty enum"));
    }

    for variant in &enum_.variants {
      self.lint_name_casing(
        format!("enum `{}` variant", &enum_.name).as_str(),
        &variant.0,
        convert_case::Case::Pascal,
      );
    }
  }

  fn visit_parameter(&mut self, parameter: &ast::Parameter) {
    self.lint_name_casing("parameter", &parameter.name, convert_case::Case::Snake);
  }

  fn visit_reference(&mut self, reference: &ast::Reference) {
    self
      .variable_references
      .insert(reference.pattern.link_id, true);
  }

  fn visit_binding_stmt(&mut self, binding_stmt: &ast::BindingStmt) {
    self.lint_name_casing("variable", &binding_stmt.name, convert_case::Case::Snake);
  }

  fn visit_if_expr(&mut self, if_expr: &ast::IfExpr) {
    // TODO: In the future, binary conditions should also be evaluated (if using literals on both operands).
    // TODO: Add a helper method to "unbox" expressions? (e.g. case for `(true)`).
    if matches!(if_expr.condition.as_ref(), ast::NodeKind::Literal(_)) {
      self.diagnostics.push(
        codespan_reporting::diagnostic::Diagnostic::warning()
          .with_message("if expression's condition is a constant expression"),
      )
    }
  }

  fn visit_call_expr(&mut self, _call_expr: &ast::CallExpr) {
    // TODO:
    // context
    //   .function_references
    //   .insert(self.callee_expr.target_key.unwrap(), true);
  }

  fn enter_function(&mut self, function: &ast::Function) {
    self.lint_name_casing("function", &function.name, convert_case::Case::Snake);

    if function.signature.parameters.len() > 4 {
      self.diagnostics.push(
        codespan_reporting::diagnostic::Diagnostic::warning()
          .with_message("function has more than 4 parameters"),
      );
    }
  }

  fn visit_indexing_expr(&mut self, _indexing_expr: &ast::IndexingExpr) {
    // self
    //   .variable_references
    //   .insert(indexing_expr.target_id, true);
  }
}
