use crate::{ast, cache};

// REVIEW: Why not abstract the error reporting to the lint methods themselves? This is more functional.
// ... Perhaps we can also get rid of variable reference counting, in favor of functional programming.
pub struct LintContext {
  pub diagnostics: Vec<codespan_reporting::diagnostic::Diagnostic<usize>>,
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

pub trait Lint {
  fn lint(&self, _context: &mut LintContext) {
    //
  }
}

// REVISE: Redundant, implement for `NodeKind` instead?
impl Lint for ast::NodeKind {
  fn lint(&self, lint_context: &mut LintContext) {
    // TODO: Here we have access to the node's metadata.
    // ... Consider using some system to provide it to whatever needs it.
    crate::dispatch!(&self, Lint::lint, lint_context);
  }
}

impl Lint for ast::Range {
  //
}

impl Lint for ast::SizeofIntrinsic {
  //
}

impl Lint for ast::Using {
  //
}

impl Lint for ast::ParenthesesExpr {
  //
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
  fn lint(&self, _lint_context: &mut LintContext) {
    // TODO: Lint name(s).
  }
}

impl Lint for ast::IntrinsicCall {
  //
}

impl Lint for ast::ExternStatic {
  //
}

impl Lint for ast::StructValue {
  //
}

impl Lint for ast::Prototype {
  //
}

impl Lint for ast::StructType {
  fn lint(&self, context: &mut LintContext) {
    context.lint_name_casing("struct", &self.name, convert_case::Case::Pascal);

    // REVIEW: Any more linting needed?
  }
}

impl Lint for ast::UnaryExpr {
  //
}

impl Lint for ast::IndexingExpr {
  fn lint(&self, context: &mut LintContext) {
    context
      .variable_references
      .insert(self.target_id.unwrap(), true);
  }
}

impl Lint for ast::StaticArrayValue {
  //
}

impl Lint for ast::BinaryExpr {
  //
}

impl Lint for ast::BlockExpr {
  fn lint(&self, context: &mut LintContext) {
    let mut did_return = false;

    for statement in &self.statements {
      if did_return {
        context.diagnostics.push(
          codespan_reporting::diagnostic::Diagnostic::warning()
            .with_message("unreachable code after return statement"),
        );

        // REVIEW: Consider whether we should stop linting the block at this point.
      }

      if matches!(statement.kind, ast::NodeKind::ReturnStmt(_)) {
        did_return = true;
      }
    }
  }
}

impl Lint for ast::Enum {
  fn lint(&self, context: &mut LintContext) {
    context.lint_name_casing("enum", &self.name, convert_case::Case::Pascal);

    if self.variants.is_empty() {
      context
        .diagnostics
        .push(codespan_reporting::diagnostic::Diagnostic::warning().with_message("empty enum"));
    }

    for variant in &self.variants {
      context.lint_name_casing(
        format!("enum `{}` variant", &self.name).as_str(),
        &variant.0,
        convert_case::Case::Pascal,
      );
    }
  }
}

impl Lint for ast::InlineExprStmt {
  //
}

impl Lint for ast::ExternFunction {
  // NOTE: There are no naming rules for externs.
}

impl Lint for ast::Function {
  fn lint(&self, context: &mut LintContext) {
    context.lint_name_casing("function", &self.name, convert_case::Case::Snake);

    if self.prototype.parameters.len() > 4 {
      context.diagnostics.push(
        codespan_reporting::diagnostic::Diagnostic::warning()
          .with_message("function has more than 4 parameters"),
      );
    }
  }
}

impl Lint for ast::CallExpr {
  fn lint(&self, _context: &mut LintContext) {
    // TODO:
    // context
    //   .function_references
    //   .insert(self.callee_expr.target_key.unwrap(), true);
  }
}

impl Lint for ast::IfExpr {
  fn lint(&self, context: &mut LintContext) {
    // TODO: In the future, binary conditions should also be evaluated (if using literals on both operands).
    // TODO: Add a helper method to "unbox" expressions? (e.g. case for `(true)`).
    if matches!(self.condition.kind, ast::NodeKind::Literal(_)) {
      context.diagnostics.push(
        codespan_reporting::diagnostic::Diagnostic::warning()
          .with_message("if expression's condition is a constant expression"),
      )
    }
  }
}

impl Lint for ast::BindingStmt {
  fn lint(&self, context: &mut LintContext) {
    context.lint_name_casing("variable", &self.name, convert_case::Case::Snake);
  }
}

impl Lint for ast::Literal {
  //
}

impl Lint for ast::Parameter {
  fn lint(&self, context: &mut LintContext) {
    context.lint_name_casing("parameter", &self.name, convert_case::Case::Snake);
  }
}

impl Lint for ast::ReturnStmt {
  //
}

impl Lint for ast::UnsafeExpr {
  //
}

impl Lint for ast::Reference {
  fn lint(&self, context: &mut LintContext) {
    context
      .variable_references
      .insert(self.pattern.target_id.unwrap(), true);
  }
}
