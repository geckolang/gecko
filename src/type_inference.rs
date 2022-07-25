use crate::{ast, cache, visitor::AnalysisVisitor};

pub type TypeCache = std::collections::HashMap<cache::Id, ast::Type>;
type TypeConstraint = (ast::Type, ast::Type);

pub struct TypeInferenceContext<'a> {
  pub diagnostics: Vec<codespan_reporting::diagnostic::Diagnostic<usize>>,
  /// A map from a type variable's id to a type.
  ///
  /// This serves as a buffer for type inference to occur. It is
  /// populated during parsing phase, when type variables are created, and
  /// it also is scope-less/context-free.
  substitutions: std::collections::HashMap<usize, ast::Type>,
  cache: &'a cache::Cache,
  // REVIEW: We can only store types of nodes that have ids (e.g. bindings, parameters, etc.).
  /// A mapping from a node to its most recent type.
  ///
  /// This is needed because analysis passes cannot mutate nodes.
  type_cache: &'a mut TypeCache,
  constraints: Vec<TypeConstraint>,
}

impl<'a> TypeInferenceContext<'a> {
  pub fn new(cache: &'a cache::Cache, type_cache: &'a mut TypeCache) -> Self {
    Self {
      diagnostics: Vec::new(),
      substitutions: std::collections::HashMap::new(),
      cache,
      type_cache,
      constraints: Vec::new(),
    }
  }

  pub fn solve_constrains(&mut self) {
    // TODO: Implement.
  }

  // REVIEW: Now with the implementation of `node.find_id()`, things like `(expr)` might have issues.
  // ... Perhaps employ the use of node flattening to solve any issues?
  fn infer_type_of(&mut self, node: &ast::NodeKind) -> ast::Type {
    let id = match node.find_id() {
      Some(id) => id,
      None => return ast::Type::Unit,
    };

    if let Some(cached_type) = self.type_cache.get(&id) {
      return cached_type.clone();
    }

    let ty = match &node {
      ast::NodeKind::BinaryExpr(binary_expr) => match binary_expr.operator {
        ast::OperatorKind::LessThan
        | ast::OperatorKind::GreaterThan
        | ast::OperatorKind::Equality
        | ast::OperatorKind::And
        | ast::OperatorKind::Or
        | ast::OperatorKind::Nand
        | ast::OperatorKind::Nor
        | ast::OperatorKind::Xor
        | ast::OperatorKind::In => ast::Type::Basic(ast::BasicType::Bool),
        _ => self.infer_type_of(&binary_expr.left),
      },
      ast::NodeKind::BindingStmt(binding_stmt) => binding_stmt
        .type_hint
        // REVISE: Cloning regardless.
        .clone()
        .unwrap_or(self.create_type_variable()),
      ast::NodeKind::Parameter(parameter) => parameter
        .type_hint
        // REVISE: Cloning regardless.
        .clone()
        .unwrap_or(self.create_type_variable()),
      _ => ast::Type::Unit,
    };

    self.type_cache.insert(id, ty.clone());

    ty
  }

  fn create_type_variable(&mut self) -> ast::Type {
    let id = self.substitutions.len();
    let result = ast::Type::Variable(id.clone());

    self.substitutions.insert(id, result.clone());

    result
  }

  /// Add a constraint stating that both of the provided types are equal.
  fn report_constraint(&mut self, for_type_var_or_type: ast::Type, is_equal_to: ast::Type) {
    self.constraints.push((for_type_var_or_type, is_equal_to));
  }
}

impl<'a> AnalysisVisitor for TypeInferenceContext<'a> {
  fn visit_binding_stmt(&mut self, binding_stmt: &ast::BindingStmt) {
    if binding_stmt.type_hint.is_some() {
      return;
    }

    // If the type hint is not present, create a fresh type
    // variable and associate it with the binding.
    let fresh_type_variable = self.create_type_variable();

    self
      .substitutions
      .insert(binding_stmt.id, fresh_type_variable);
  }

  fn visit_unary_expr(&mut self, unary_expr: &ast::UnaryExpr) {
    // TODO: Add support for missing hints; logical not, pointer dereference, and address-of?
    if !matches!(
      unary_expr.operator,
      ast::OperatorKind::Add | ast::OperatorKind::SubtractOrNegate
    ) {
      return;
    }

    // TODO:
    // - {error handling} check types to be what we expect

    // Report that the type of the unary expression's operand must
    // be an integer.
    let expr_type = self.infer_type_of(&unary_expr.expr);

    self.report_constraint(expr_type, ast::Type::MetaInteger);
  }

  fn visit_return_stmt(&mut self, _return_stmt: &ast::ReturnStmt) {
    // TODO: Where is signature return type type variable created?
  }
}
