use crate::{ast, cache, visitor::AnalysisVisitor};

pub type TypeCache = std::collections::HashMap<cache::Id, ast::Type>;
type TypeConstraint = (ast::Type, ast::Type);

struct TypeInferenceVisitor<'a> {
  /// A map from a type variable's id to a type.
  ///
  /// This serves as a buffer for type inference to occur. It is
  /// populated during parsing phase, when type variables are created, and
  /// it also is scope-less/context-free.
  substitutions: std::collections::HashMap<usize, ast::Type>,
  cache: &'a cache::Cache,
  /// A mapping from a node to its most recent type.
  ///
  /// This is needed because analysis passes cannot mutate nodes.
  type_cache: &'a mut TypeCache,
  constraints: Vec<TypeConstraint>,
}

impl<'a> TypeInferenceVisitor<'a> {
  pub fn infer_type_of(&mut self, node: std::rc::Rc<ast::Node>) -> ast::Type {
    if let Some(cached_type) = self.type_cache.get(&node.id) {
      return cached_type.clone();
    }

    let ty = match &node.kind {
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
        _ => self.infer_type_of(std::rc::Rc::clone(&binary_expr.left)),
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

    self.type_cache.insert(node.id, ty.clone());

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

impl<'a> AnalysisVisitor for TypeInferenceVisitor<'a> {
  fn visit_binding_stmt(&mut self, binding_stmt: &ast::BindingStmt, node: std::rc::Rc<ast::Node>) {
    if binding_stmt.type_hint.is_some() {
      return;
    }

    // If the type hint is not present, create a fresh type
    // variable and associate it with the binding.
    let fresh_type_variable = self.create_type_variable();

    self.substitutions.insert(node.id, fresh_type_variable);
  }

  fn visit_unary_expr(&mut self, unary_expr: &ast::UnaryExpr, _node: std::rc::Rc<ast::Node>) {
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
    let expr_type = self.infer_type_of(std::rc::Rc::clone(&unary_expr.expr));

    self.report_constraint(expr_type, ast::Type::MetaInteger);
  }

  fn visit_return_stmt(&mut self, _return_stmt: &ast::ReturnStmt, _node: std::rc::Rc<ast::Node>) {
    // TODO: Where is prototype return type type variable created?
  }
}
