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
  // TODO: This doc. is outdated. It is not created during parsing. Instead,
  // ... it is created during type inference. If the type hint of something is `None`,
  // ... a fresh variable type is created for it here.
  /// A map from a type variable's (TODO: Or actually, a node's id) id to a type.
  ///
  /// This serves as a buffer for type inference to occur. It is
  /// populated during parsing, when type variables are created, and
  /// it also is scope-less/context-free.
  substitutions: std::collections::HashMap<usize, ast::Type>,
  cache: &'a cache::Cache,
  // REVIEW: We can only store types of nodes that have ids (e.g. bindings, parameters, etc.).
  /// A mapping from a node to its most recent type.
  ///
  /// This is needed because analysis passes cannot mutate nodes.
  type_cache: TypeCache,
  /// Constraints are expectations, or hints, of equality between a pair of types.
  ///
  /// They are first gathered, then the unification algorithm is performed to solve types, at
  /// the last step of type inference.
  constraints: Vec<TypeConstraint>,
  current_function_id: Option<cache::Id>,
}

impl<'a> TypeInferenceContext<'a> {
  pub fn new(cache: &'a cache::Cache) -> Self {
    Self {
      diagnostics: Vec::new(),
      substitutions: std::collections::HashMap::new(),
      cache,
      type_cache: TypeCache::new(),
      constraints: Vec::new(),
      current_function_id: None,
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
    // REVIEW: What if we have conflicting constraints? Say, we have different calls with different types to the same function?
    // ... Or if the parameters are constrained to be something, yet the arguments are constrained to be different?
    // REVIEW: Any way to avoid cloning?
    for constrain in self.constraints.clone() {
      self.unify(&constrain.0, &constrain.1);
    }

    for (id, ty) in &self.substitutions {
      // Update the type for this node in the type cache.
      self.type_cache.insert(id.clone(), ty.clone());
    }

    self.constraints.clear();
  }

  /// Substitute a type variable with its non-variable type (if defined).
  ///
  /// If the substitution is not defined, the same type is returned. This
  /// function will recursively substitute type variables, until a non-variable
  /// type is found.
  pub fn substitute(&self, ty: ast::Type) -> ast::Type {
    if let ast::Type::Variable(id) = &ty {
      // REVIEW: Should we be force-unwrapping here?
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
  // REVIEW: What is the `expectedType` parameter used for in the type inference reference?
  fn infer_type_of(&mut self, node: &ast::NodeKind) -> ast::Type {
    // REVIEW: This only works for direct declarations. All other nodes will type `Unit`.
    if let Some(id) = node.find_id() {
      if let Some(cached_type) = self.type_cache.get(&id) {
        return cached_type.clone();
      }
    }

    // REVIEW: Shouldn't we be associating (substitutions is associations) any type variables created with the id of their corresponding nodes?
    // ... Example: Nullptr literal, its id => type variable.
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
        .unwrap_or_else(|| self.create_type_variable(binding_stmt.id)),
      ast::NodeKind::Parameter(parameter) => parameter
        .type_hint
        // REVISE: Cloning regardless.
        .clone()
        .unwrap_or_else(|| self.create_type_variable(parameter.id)),
      ast::NodeKind::Literal(literal) => match literal {
        ast::Literal::Bool(_) => ast::Type::Basic(ast::BasicType::Bool),
        ast::Literal::Char(_) => ast::Type::Basic(ast::BasicType::Char),
        ast::Literal::Int(_, size) => ast::Type::Basic(ast::BasicType::Int(size.clone())),
        ast::Literal::String(_) => ast::Type::Basic(ast::BasicType::String),
        ast::Literal::Nullptr(id, _) => {
          // TODO: Temporary.
          // return ast::Type::Pointer(Box::new(self.create_type_variable()))
          self.create_type_variable(id.to_owned())
        }
      },
      ast::NodeKind::Reference(reference) => {
        let target = self
          .cache
          .find_decl_via_link(&reference.pattern.id)
          .unwrap();

        self.infer_type_of(&target)
      }
      // REVIEW: What about other nodes? Say, indexing, array value, etc. Their types shouldn't be `Unit`.
      _ => ast::Type::Unit,
    };

    // REVIEW: Why is this here? We need to report things' types somewhere to fill up the
    // ... type cache, but why here? Also, probably only declaration's types are to be inserted
    // ... on the type cache, because only declarations have ids. Actually, it's here because this
    // ... function performs memoization of the inferred types, but apparently only for declarations.
    // ... Review what can be done, and whether it is efficient to only infer and memoize declaration's types.
    if let Some(id) = node.find_id() {
      // Update association of the node's id with the newly inferred type.
      self.substitutions.insert(id, ty.clone());
    }

    ty
  }

  // TODO: Why not take an id here, and directly associate?
  fn create_type_variable(&mut self, for_node_id: cache::Id) -> ast::Type {
    // REVIEW: Why add have a new id for this type variable? Couldn't we use the node id?
    // let id = self.substitutions.len();

    let type_variable = ast::Type::Variable(for_node_id.clone());

    // Update association of the node's id with the new fresh type variable.
    self
      .substitutions
      .insert(for_node_id, type_variable.clone());

    // REVIEW: Why add have a new id for this type variable? Couldn't we use the node id?
    // self.substitutions.insert(id, result.clone());

    type_variable
  }

  /// Add a constraint stating that both of the provided types are equal.
  fn report_constraint(&mut self, that_type_or_var: ast::Type, is_equal_to: ast::Type) {
    self.constraints.push((that_type_or_var, is_equal_to));
  }
}

impl<'a> AnalysisVisitor for TypeInferenceContext<'a> {
  fn visit_literal(&mut self, literal: &ast::Literal) {
    let id = match literal {
      ast::Literal::Nullptr(id, _) => id,
      // Only the nullptr literal has the juicy stuff.
      _ => return,
    };

    self.create_type_variable(id.to_owned());

    // self.substitutions.insert(id.to_owned(), ty);
  }

  fn visit_binding_stmt(&mut self, binding_stmt: &ast::BindingStmt) {
    // REVIEW: Don't we have to specify that the type of the binding must equal its value's type,
    // ... regardless of the type hint's presence?
    if binding_stmt.type_hint.is_some() {
      return;
    }

    // If the type hint is not present, create a fresh type
    // variable.
    let fresh_type_variable = self.create_type_variable(binding_stmt.id);

    // Associate the fresh type variable with the binding.
    // self
    //   .substitutions
    //   .insert(binding_stmt.id, fresh_type_variable.clone());

    // FIXME: Added this constraint because it makes sense, but it was not part of the reference code.
    let value_type = self.infer_type_of(&binding_stmt.value);

    self.report_constraint(fresh_type_variable, value_type);
  }

  fn visit_unary_expr(&mut self, unary_expr: &ast::UnaryExpr) {
    // REVIEW: We need to insert a substitution? Otherwise what links the constraint back to this?
    let expr_type = self.infer_type_of(&unary_expr.expr);

    match unary_expr.operator {
      ast::OperatorKind::Add | ast::OperatorKind::SubtractOrNegate => {
        // Report that the type of the unary expression's operand must
        // be an integer.
        self.report_constraint(expr_type, ast::Type::MetaInteger);
      }
      // TODO: Add support for missing hints; logical not, and address-of?
      ast::OperatorKind::MultiplyOrDereference => {
        // let _pointer_inner_type = self.create_type_variable();

        // TODO: How to specify/deal with type constructor? Currently just defaulting to creating a type variable.
        // self.report_constraint(expr_type, ast::Type::Pointer(Box::new(pointer_inner_type)))

        // TODO: Temporary.
        self.report_constraint(expr_type, ast::Type::Any);
      }
      ast::OperatorKind::Not => {
        self.report_constraint(expr_type, ast::Type::Basic(ast::BasicType::Bool));
      }
      // NOTE: All cases for unary operators are covered above.
      _ => {}
    }
  }

  fn visit_binary_expr(&mut self, binary_expr: &ast::BinaryExpr) {
    let left_expr_type = self.infer_type_of(&binary_expr.left);
    let right_expr_type = self.infer_type_of(&binary_expr.right);

    // REVIEW: Should we report a constraint that both operands must be of the same type?
    // ... Or is it implied by both being expected to equal the same type? Will it affect the
    // ... algorithm if we don't report them to be equal to each other specifically?

    // REVIEW: What about the right expression?
    let ty = match binary_expr.operator {
      ast::OperatorKind::Add
      | ast::OperatorKind::SubtractOrNegate
      | ast::OperatorKind::MultiplyOrDereference
      | ast::OperatorKind::Divide
      | ast::OperatorKind::LessThan
      | ast::OperatorKind::LessThanOrEqual
      | ast::OperatorKind::Equality
      | ast::OperatorKind::GreaterThan
      | ast::OperatorKind::GreaterThanOrEqual => ast::Type::MetaInteger,
      ast::OperatorKind::And
      | ast::OperatorKind::Or
      | ast::OperatorKind::Nand
      | ast::OperatorKind::Nor
      | ast::OperatorKind::Xor => ast::Type::Basic(ast::BasicType::Bool),
      // TODO: Implement.
      ast::OperatorKind::Cast | ast::OperatorKind::In => todo!(),
      // NOTE: All cases for binary operators are covered above.
      _ => return,
    };

    self.report_constraint(left_expr_type, ty.clone());
    self.report_constraint(right_expr_type, ty);
  }

  fn visit_return_stmt(&mut self, return_stmt: &ast::ReturnStmt) {
    // REVIEW: What about block yielding as the return value of a function?

    let return_value_type = if let Some(return_value) = &return_stmt.value {
      self.infer_type_of(&return_value)
    } else {
      ast::Type::Unit
    };

    let current_function_sig = self
      .cache
      // NOTE: No need to follow declaration link because it is directly set.
      .declarations
      .get(&self.current_function_id.unwrap())
      .unwrap()
      .find_signature()
      .unwrap();

    // FIXME: Here, we are manually doing the job of `infer_type_of`, which does automatic caching as well.
    // ... Prone to bugs. Consider abstracting the common functionality, or merge into `infer_type_of`.
    let current_function_return_type = current_function_sig
      .return_type_hint
      // REVISE: Cloning regardless.
      .clone()
      .unwrap_or_else(|| self.create_type_variable(current_function_sig.return_type_id));

    self.report_constraint(return_value_type, current_function_return_type);
  }

  fn visit_signature(&mut self, _signature: &ast::Signature) {
    // TODO: For the return type, we might need to add the `id` field to `ast::Signature`, and then
    // ... make it an `rc::Rc<>`, and possibly use a buffer id to be able to retrieve it during visitation.
  }

  fn visit_call_expr(&mut self, _call_expr: &ast::CallExpr) {
    // TODO: The arguments of the call constrain the types of the target's parameters.
  }

  fn enter_function(&mut self, function: &ast::Function) {
    self.current_function_id = Some(function.id);
  }

  fn exit_function(&mut self, _function: &ast::Function) -> () {
    self.current_function_id = None;
  }
}

#[cfg(test)]
mod tests {
  use crate::mock::tests::Mock;

  use super::*;

  #[test]
  fn occurs_in() {
    let cache = cache::Cache::new();
    let mut type_inference_ctx = TypeInferenceContext::new(&cache);
    let first_index_id = 0;
    let second_index_id = first_index_id + 1;

    type_inference_ctx
      .substitutions
      .insert(first_index_id, ast::Type::Variable(first_index_id));

    type_inference_ctx
      .substitutions
      .insert(second_index_id, ast::Type::Unit);

    let subject_type_variable = ast::Type::Variable(first_index_id);

    assert!(type_inference_ctx.occurs_in(first_index_id, &subject_type_variable));
    assert!(!type_inference_ctx.occurs_in(second_index_id, &subject_type_variable));
    assert!(!type_inference_ctx.occurs_in(first_index_id, &ast::Type::Unit));
  }

  #[test]
  fn create_type_variable() {
    let cache = cache::Cache::new();
    let mut type_context = TypeInferenceContext::new(&cache);

    assert_eq!(type_context.create_type_variable(0), ast::Type::Variable(0));
    assert_eq!(1, type_context.substitutions.len());
  }

  #[test]
  fn solve_constraints() {
    let cache = cache::Cache::new();
    let mut type_context = TypeInferenceContext::new(&cache);

    // TODO: Add actual constraints to complete this test.

    type_context.solve_constraints();
    assert!(type_context.constraints.is_empty());
  }

  #[test]
  fn substitute() {
    let cache = cache::Cache::new();
    let mut type_context = TypeInferenceContext::new(&cache);

    assert_eq!(ast::Type::Unit, type_context.substitute(ast::Type::Unit));

    let type_variable_id = 0;
    let non_type_variable = ast::Type::Basic(ast::BasicType::Bool);

    type_context
      .substitutions
      .insert(type_variable_id, non_type_variable.clone());

    assert_eq!(
      non_type_variable,
      type_context.substitute(ast::Type::Variable(type_variable_id))
    );
  }

  #[test]
  fn infer_binding_from_literal() {
    let cache = cache::Cache::new();
    let mut type_inference_ctx = TypeInferenceContext::new(&cache);

    let binding_stmt = Mock::free_binding(
      0,
      "a",
      ast::NodeKind::Literal(ast::Literal::Bool(true)),
      None,
    );

    type_inference_ctx.dispatch(&binding_stmt);
    type_inference_ctx.solve_constraints();

    assert_eq!(
      type_inference_ctx.substitute(ast::Type::Variable(0)),
      ast::Type::Basic(ast::BasicType::Bool)
    );
  }

  #[test]
  fn infer_binding_from_reference() {
    let mut cache = cache::Cache::new();
    let binding_stmt_a_id = 0;
    let binding_stmt_b_id = binding_stmt_a_id + 1;

    let binding_stmt_a = Mock::free_binding(
      binding_stmt_a_id,
      "a",
      ast::NodeKind::Literal(ast::Literal::Bool(true)),
      None,
    );

    cache.links.insert(binding_stmt_a_id, binding_stmt_a_id);

    cache
      .declarations
      .insert(binding_stmt_a_id, binding_stmt_a.clone());

    let binding_stmt_b = Mock::free_binding(
      binding_stmt_b_id,
      "b",
      ast::NodeKind::Reference(Mock::reference(binding_stmt_a_id)),
      None,
    );

    let mut type_inference_ctx = TypeInferenceContext::new(&cache);

    type_inference_ctx.dispatch(&binding_stmt_a);
    visitor::traverse(&binding_stmt_b, &mut type_inference_ctx);
    type_inference_ctx.solve_constraints();

    assert_eq!(
      type_inference_ctx.substitute(ast::Type::Variable(binding_stmt_b_id)),
      ast::Type::Basic(ast::BasicType::Bool)
    );

    // REVIEW: Why is this failing?
    assert_eq!(
      type_inference_ctx
        .type_cache
        .get(&binding_stmt_b_id)
        .unwrap(),
      &ast::Type::Basic(ast::BasicType::Bool)
    );
  }

  #[test]
  fn infer_binding_from_nullptr() {
    let mut cache = cache::Cache::new();

    let binding_id = 0;

    let binding_stmt = Mock::free_binding(
      binding_id.clone(),
      "a",
      ast::NodeKind::Literal(ast::Literal::Nullptr(1, None)),
      None,
    );

    cache.links.insert(binding_id, binding_id);
    cache.declarations.insert(binding_id, binding_stmt.clone());

    let mut type_inference_ctx = TypeInferenceContext::new(&cache);

    let deref_expr = ast::NodeKind::UnaryExpr(ast::UnaryExpr {
      cast_type: None,
      expr: std::rc::Rc::new(ast::NodeKind::Reference(Mock::reference(binding_id))),
      operator: ast::OperatorKind::MultiplyOrDereference,
    });

    visitor::traverse(&binding_stmt, &mut type_inference_ctx);
    visitor::traverse(&deref_expr, &mut type_inference_ctx);
    type_inference_ctx.solve_constraints();

    assert_eq!(
      type_inference_ctx.substitute(ast::Type::Variable(0)),
      // FIXME: Awaiting type constructor inference.
      ast::Type::Any
    );
  }

  #[test]
  fn infer_parameter_from_unary_expr() {
    let mut cache = cache::Cache::new();

    let id = 0;

    let parameter = ast::NodeKind::Parameter(std::rc::Rc::new(ast::Parameter {
      id,
      name: "a".to_string(),
      position: 0,
      type_hint: None,
    }));

    cache.links.insert(id, id);
    cache.declarations.insert(id, parameter.clone());

    let mut type_inference_ctx = TypeInferenceContext::new(&cache);

    let negation_expr = ast::NodeKind::UnaryExpr(ast::UnaryExpr {
      cast_type: None,
      expr: std::rc::Rc::new(ast::NodeKind::Reference(Mock::reference(id))),
      operator: ast::OperatorKind::SubtractOrNegate,
    });

    type_inference_ctx.dispatch(&parameter);
    visitor::traverse(&negation_expr, &mut type_inference_ctx);
    type_inference_ctx.solve_constraints();

    assert_eq!(
      type_inference_ctx.substitute(ast::Type::Variable(id)),
      // FIXME: Awaiting type constructor inference.
      ast::Type::MetaInteger
    );

    assert_eq!(
      type_inference_ctx.type_cache.get(&id).unwrap(),
      &ast::Type::MetaInteger
    );
  }

  #[test]
  fn infer_return_type_from_literal() {
    let mut cache = cache::Cache::new();

    let return_stmt = ast::NodeKind::ReturnStmt(ast::ReturnStmt {
      value: Some(Box::new(ast::NodeKind::Literal(ast::Literal::Bool(true)))),
    });

    let function = Mock::free_function(vec![return_stmt]);

    cache
      .declarations
      .insert(function.find_id().unwrap(), function.clone());

    let mut type_inference_ctx = TypeInferenceContext::new(&cache);
    let signature = function.find_signature().unwrap();

    visitor::traverse(&function, &mut type_inference_ctx);
    type_inference_ctx.solve_constraints();

    assert_eq!(
      type_inference_ctx.type_cache.get(&signature.return_type_id),
      Some(&ast::Type::Basic(ast::BasicType::Bool))
    );

    // assert_eq!(
    //   type_inference_ctx.substitute(ast::Type::Variable(0)),
    //   ast::Type::Basic(ast::BasicType::Bool)
    // );
  }

  // TODO: Use the empty array type test.
  // TODO: Also, create a second test for inferring of parameter types.
}
