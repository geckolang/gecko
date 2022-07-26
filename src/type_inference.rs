use crate::{
  ast, cache,
  visitor::{self, AnalysisVisitor},
};

pub type TypeCache = std::collections::HashMap<cache::Id, ast::Type>;
type TypeConstraint = (ast::Type, ast::Type);

pub fn run(ast_map: &ast::AstMap, cache: &cache::Cache) -> (Vec<ast::Diagnostic>, TypeCache) {
  let mut type_inference_ctx = TypeInferenceContext::new(cache);

  for inner_ast in ast_map.values() {
    for top_level_node in inner_ast {
      visitor::traverse(&top_level_node, &mut type_inference_ctx);
    }
  }

  type_inference_ctx.solve_constraints();

  (
    type_inference_ctx.diagnostics,
    type_inference_ctx.type_cache,
  )
}

pub struct TypeInferenceContext<'a> {
  pub diagnostics: Vec<ast::Diagnostic>,
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
  type_cache: TypeCache,
  constraints: Vec<TypeConstraint>,
}

impl<'a> TypeInferenceContext<'a> {
  pub fn new(cache: &'a cache::Cache) -> Self {
    Self {
      diagnostics: Vec::new(),
      substitutions: std::collections::HashMap::new(),
      cache,
      type_cache: TypeCache::new(),
      constraints: Vec::new(),
    }
  }

  // REVISE: Avoid excessive cloning.
  fn unify(&mut self, type_a: &ast::Type, type_b: &ast::Type) {
    // TODO: Cleanup code. Perhaps expand it to not be a big match statement?
    match (type_a, type_b) {
      // TODO: Missing type constructor support.
      // If both sides are the same type variable, do nothing.
      (ast::Type::Variable(id_a), ast::Type::Variable(id_b)) if id_a == id_b => {}
      // If one of the types is a type variable that’s bound in the substitution,
      // use unify with that type instead.
      (ast::Type::Variable(id), _)
        if {
          let access = self.substitutions.get(id);

          // REVIEW: Here we manually added the `.is_some()` check. Verify this is as expected.
          access.is_some() && access != Some(&ast::Type::Variable(*id))
        } =>
      {
        self.unify(&self.substitutions.get(id).unwrap().clone(), type_b)
      }
      (_, ast::Type::Variable(id))
        if {
          let access = self.substitutions.get(id);

          // REVIEW: Here we manually added the `.is_some()` check. Verify this is as expected.
          access.is_some() && access != Some(&ast::Type::Variable(*id))
        } =>
      {
        self.unify(type_a, &self.substitutions.get(id).unwrap().clone())
      }
      // Otherwise, if one of the types is an unbound type variable, bind it to the
      // other type. Remember to do an occurs check to avoid constructing infinite types.
      (ast::Type::Variable(id_a), _) => {
        // REVISE: Proper error handling.
        assert!(!self.occurs_in(id_a.to_owned(), &type_b));

        self.substitutions.insert(*id_a, type_b.clone());
      }
      (_, ast::Type::Variable(id_b)) => {
        // REVISE: Proper error handling.
        assert!(!self.occurs_in(id_b.to_owned(), &type_a));

        self.substitutions.insert(*id_b, type_a.clone());
      }
      _ => {}
    }
  }

  /// Recursively check if a type variable index occurs in
  /// a type.
  ///
  /// For this to be `true`, the type in question must be a type variable.
  /// Any other type will yield `false`.
  fn occurs_in(&self, index_id: usize, ty: &ast::Type) -> bool {
    match ty {
      ast::Type::Variable(id)
        if self.substitutions.get(id).unwrap() != &ast::Type::Variable(id.to_owned()) =>
      {
        self.occurs_in(index_id, &self.substitutions.get(id).unwrap())
      }
      // REVIEW: Will this compare the underlying values or the addresses?
      ast::Type::Variable(id) => id == &index_id,
      // TODO: Generics / type constructors.
      _ => false,
    }
  }

  // TODO: This is the same thing as `node.unification`, but it assumed nodes can be mutated as in object-oriented languages.
  /// Solves constraints by performing unification.
  ///
  /// This occurs after all the constraints have been added,
  /// and is the last step for Hindley-Milner type inference.
  /// After this process is completed, nodes can proceed to perform
  /// their post-unification phase, which mostly consists of replacing
  /// their type variables with concrete types.
  fn solve_constraints(&mut self) {
    // REVIEW: Any way to avoid cloning?
    for constrain in self.constraints.clone() {
      self.unify(&constrain.0, &constrain.1);
    }

    /// !!!!!!!!!!!!!! HERE DEBUG self.substitutions !!!!!!!!!!!!!!!!
    self.constraints.clear();
  }

  /// Substitute a type variable with its non-variable type (if defined).
  ///
  /// This function will recursively substitute type variables,
  /// until a non-variable type is found.
  pub fn substitute(&self, ty: ast::Type) -> ast::Type {
    if let ast::Type::Variable(id) = &ty {
      let substitution = self.substitutions.get(id).unwrap().clone();

      // case TVariable(i) if substitution(i) != TVariable(i) =>
      //substitute(substitution(i))

      // REVIEW: Is this condition correct?
      if substitution != ty {
        return self.substitute(substitution);
      }
    }

    // TODO: Missing support for constructor types.

    ty
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
      // REVIEW: What about other nodes? Say, indexing, array value, etc. Their types shouldn't be `Unit`.
      _ => ast::Type::Unit,
    };

    // REVIEW: Why is this here? We need to report things' types somewhere to fill up the
    // ... type cache, but why here? Also, probably only declaration's types are to be inserted
    // ... on the type cache, because only declarations have ids.
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
      .insert(binding_stmt.id, fresh_type_variable.clone());

    // FIXME: Added this constraint because it makes sense, but it was not part of the reference code.
    let value_type = self.infer_type_of(&binding_stmt.value);

    self.report_constraint(fresh_type_variable, value_type);
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
