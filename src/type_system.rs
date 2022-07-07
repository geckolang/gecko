use crate::{ast, cache, dispatch, lowering};

#[derive(Clone)]
enum TypeConstrainKind {
  Equality,
}

type TypeConstraint = (ast::Type, ast::Type, TypeConstrainKind);

pub struct TypeContext {
  diagnostics: Vec<codespan_reporting::diagnostic::Diagnostic<usize>>,
  in_loop: bool,
  in_unsafe_block: bool,
  in_impl: bool,
  current_function_id: Option<cache::Id>,
  // REVISE: Make use-of or discard.
  _types_cache: std::collections::HashMap<cache::Id, ast::Type>,
  usings: Vec<ast::Using>,
  constraints: Vec<TypeConstraint>,
  /// A map from a type variable's id to a type.
  ///
  /// This serves as a buffer for type inference to occur. It is
  /// populated during parsing phase, when type variables are created, and
  /// it also is scope-less/context-free.
  substitutions: std::collections::HashMap<usize, ast::Type>,
}

impl TypeContext {
  pub fn run(
    ast: &Vec<ast::Node>,
    cache: &cache::Cache,
  ) -> (
    Vec<codespan_reporting::diagnostic::Diagnostic<usize>>,
    Vec<ast::Using>,
  ) {
    let mut type_context = TypeContext::new();

    // TODO: What about constraint reports, and post-unification?

    // for top_level_node in inner_ast.iter_mut() {
    //   top_level_node
    //     .kind
    //     .post_unification(&mut type_context, &cache);
    // }

    for node in ast {
      node.kind.check(&mut type_context, cache);
      // node.report_constraints(&mut semantic_check_context, cache);
    }

    (type_context.diagnostics, type_context.usings)
  }

  pub fn new() -> Self {
    Self {
      diagnostics: Vec::new(),
      in_loop: false,
      in_unsafe_block: false,
      in_impl: false,
      current_function_id: None,
      _types_cache: std::collections::HashMap::new(),
      usings: Vec::new(),
      constraints: Vec::new(),
      substitutions: std::collections::HashMap::new(),
    }
  }

  pub fn infer_prototype_type(prototype: &ast::Prototype, return_type: ast::Type) -> ast::Type {
    ast::Type::Function(ast::FunctionType {
      return_type: Box::new(return_type),
      parameter_types: prototype
        .parameters
        .iter()
        .map(|parameter| parameter.ty.clone())
        .collect(),
      is_variadic: prototype.is_variadic,
      is_extern: prototype.is_extern,
    })
  }

  // pub fn test() -> i32 {
  //   let a: &str = {
  //     return 0;
  //   };
  // }

  pub fn infer_return_value_type(body: &ast::BlockExpr, cache: &cache::Cache) -> ast::Type {
    let body_type = body.infer_type(cache).flatten(cache);

    if body_type.is_a_lowerable() {
      return body_type;
    }

    let mut ty = ast::Type::Unit;

    // BUG: Finish re-implementing. This is essential.
    // REVISE: Cloning body. This may be a large AST.
    ast::NodeKind::BlockExpr(body.clone()).traverse(|child| {
      if let ast::NodeKind::ReturnStmt(return_stmt) = child {
        // REVIEW: What if the return statement's value is a block that contains a return statement?
        if let Some(return_value) = &return_stmt.value {
          ty = return_value.kind.infer_type(cache);
        }

        // If the return statement is empty, then the function's return type is unit.
        return false;
      }

      true
    });

    ty
  }

  // TODO: Make use-of, or get rid-of.
  fn _fetch_type(
    &mut self,
    node_kind: &ast::NodeKind,
    unique_key: &cache::Id,
    cache: &cache::Cache,
  ) -> ast::Type {
    if let Some(cached_type) = self._types_cache.get(unique_key) {
      return cached_type.clone();
    }

    let inferred_type = node_kind.infer_type(cache);

    self
      ._types_cache
      // TODO: Cloning type.
      .insert(unique_key.clone(), inferred_type.clone());

    return inferred_type;
  }

  // TODO: Use an enum to specify error type instead of a string.
  // REVIEW: Consider using `Result` instead of `Option`.
  // pub fn compare_prototypes(
  //   prototype_a: &ast::Prototype,
  //   prototype_b: &ast::Prototype,
  //   cache: &cache::Cache,
  // ) -> Option<String> {
  //   if prototype_a.parameters.len() != prototype_b.parameters.len() {
  //     return Some("parameter count".to_string());
  //   }

  //   let parameter_types = prototype_a
  //     .parameters
  //     .iter()
  //     .zip(prototype_b.parameters.iter())
  //     .map(|(param_def_a, param_def_b)| (param_def_a.ty.clone(), param_def_b.ty.clone()));

  //   for (param_type_a, param_type_b) in parameter_types {
  //     if !Self::compare(&param_type_a, &param_type_b, cache) {
  //       // TODO: Be more specific.
  //       return Some("parameter type".to_string());
  //     }
  //   }

  //   if !Self::compare(
  //     &prototype_a.return_type_annotation,
  //     &prototype_b.return_type_annotation,
  //     cache,
  //   ) {
  //     return Some("return type".to_string());
  //   }

  //   None
  // }

  // TODO: Create a `finalize` method step to ensure that the main function was defined.

  fn create_type_variable(&mut self) -> ast::Type {
    let id = self.substitutions.len();
    let result = ast::Type::Variable(id.clone());

    self.substitutions.insert(id, result.clone());

    result
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
}

pub trait Check {
  // REVIEW: Consider caching inference results here, if they are indeed costly.
  fn infer_type(&self, _cache: &cache::Cache) -> ast::Type {
    ast::Type::Unit
  }

  fn check(&self, _context: &mut TypeContext, _cache: &cache::Cache) {
    //
  }

  fn report_constraints(&mut self, _context: &mut TypeContext, _cache: &cache::Cache) {
    //
  }

  fn post_unification(&mut self, _context: &mut TypeContext, _cache: &cache::Cache) {
    //
  }
}

impl Check for ast::NodeKind {
  fn infer_type(&self, cache: &cache::Cache) -> ast::Type {
    dispatch!(&self, Check::infer_type, cache)
  }

  fn check(&self, context: &mut TypeContext, cache: &cache::Cache) {
    dispatch!(&self, Check::check, context, cache);
  }
}

impl Check for ast::Range {
  fn check(&self, context: &mut TypeContext, _cache: &cache::Cache) {
    // NOTE: No need to check whether the range's bounds are constant
    // ... expressions, this is ensured by the parser.

    let start_literal = crate::force_match!(&self.start.kind, ast::NodeKind::Literal);
    let end_literal = crate::force_match!(&self.end.kind, ast::NodeKind::Literal);

    let start_int = match start_literal {
      ast::Literal::Int(value, _) => value,
      _ => unreachable!(),
    };

    let end_int = match end_literal {
      ast::Literal::Int(value, _) => value,
      _ => unreachable!(),
    };

    if start_int > end_int {
      context.diagnostics.push(
        codespan_reporting::diagnostic::Diagnostic::error().with_message(String::from(
          "range start must be less than or equal to end",
        )),
      );
    }
  }
}

impl Check for ast::SizeofIntrinsic {
  fn infer_type(&self, _cache: &cache::Cache) -> ast::Type {
    ast::Type::Basic(ast::BasicType::Int(ast::IntSize::I64))
  }

  fn check(&self, context: &mut TypeContext, cache: &cache::Cache) {
    if self.ty.flatten(cache).is(&ast::Type::Unit) {
      context.diagnostics.push(
        codespan_reporting::diagnostic::Diagnostic::error()
          .with_message("cannot determine size of unit type"),
      );
    }
  }
}

impl Check for ast::Using {
  fn check(&self, context: &mut TypeContext, _cache: &cache::Cache) {
    // FIXME: Can't just push the import once encountered; only when it's actually used.
    context.usings.push(self.clone());
  }
}

impl Check for ast::ParenthesesExpr {
  fn infer_type(&self, cache: &cache::Cache) -> ast::Type {
    self.expr.kind.infer_type(cache)
  }

  fn check(&self, context: &mut TypeContext, cache: &cache::Cache) {
    self.expr.kind.check(context, cache);
  }
}

impl Check for ast::Trait {
  //
}

impl Check for ast::StructImpl {
  fn check(&self, context: &mut TypeContext, cache: &cache::Cache) {
    context.in_impl = true;

    for method in &self.member_methods {
      if !method.prototype.accepts_instance {
        context.diagnostics.push(
          codespan_reporting::diagnostic::Diagnostic::error().with_message(format!(
            "implementation method `{}` is missing the instance parameter `this`",
            method.name
          )),
        )
      }

      method.check(context, cache);
    }

    let target_node = cache.force_get(&self.target_struct_pattern.target_id.unwrap());

    // REVISE: Cleanup.
    if let ast::NodeKind::StructType(_target_struct_type) = &target_node {
      if let Some(trait_pattern) = &self.trait_pattern {
        let trait_node = cache.force_get(&trait_pattern.target_id.unwrap());

        if let ast::NodeKind::Trait(trait_type) = &trait_node {
          for trait_method in &trait_type.methods {
            let impl_method_result = self
              .member_methods
              .iter()
              .find(|impl_method| impl_method.name == trait_method.0);

            if let Some(_impl_method) = impl_method_result {
              // TODO: Finish implementing.
              let prototype_unification_result =
                // TypeCheckContext::unify_prototypes(&trait_method.1, impl_method, cache);
                Some("pending error".to_string());

              if let Some(error) = prototype_unification_result {
                // REVISE: Use expected/got system.
                context.diagnostics.push(
                  codespan_reporting::diagnostic::Diagnostic::error().with_message(format!(
                    "prototype of implementation method `{}` for trait `{}` mismatch in {}",
                    "pending impl method name", trait_type.name, error
                  )),
                )
              }
            } else {
              context.diagnostics.push(
                codespan_reporting::diagnostic::Diagnostic::error().with_message(format!(
                  "required method `{}` not implemented",
                  trait_method.0
                )),
              );
            }
          }
        } else {
          context.diagnostics.push(
            codespan_reporting::diagnostic::Diagnostic::error().with_message(format!(
              "cannot implement non-trait `{}`",
              &trait_pattern.base_name
            )),
          );
        }
      }
    } else {
      context.diagnostics.push(
        codespan_reporting::diagnostic::Diagnostic::error().with_message(format!(
          "cannot implement for a non-struct type `{}`",
          self.target_struct_pattern.base_name
        )),
      );
    }

    context.in_impl = false;
  }
}

impl Check for ast::MemberAccess {
  fn infer_type(&self, cache: &cache::Cache) -> ast::Type {
    let base_expr_type = self.base_expr.kind.infer_flatten_type(cache);

    let struct_type = match base_expr_type {
      ast::Type::Struct(struct_type) => struct_type,
      // REVIEW: Investigate this strategy. Shouldn't we be using `unreachable!()` instead?
      // ... But this point may be reachable from the user-side. Need to somehow properly
      // ... handle this case.
      _ => return ast::Type::Error,
    };

    if let Some(struct_field) = struct_type
      .fields
      .iter()
      .find(|field| field.0 == self.member_name)
    {
      return struct_field.1.clone();
    }

    // REVIEW: Why not abstract this to the `Reference` node? We're doing the same thing (or very similar at least), correct?
    // TODO: Lookup implementation, and attempt to match a method.
    if let Some(struct_impls) = cache.struct_impls.get(&struct_type.cache_id) {
      for (method_cache_id, method_name) in struct_impls {
        if method_name == &self.member_name {
          return cache.force_get(&method_cache_id).infer_type(cache);
        }
      }
    }

    return ast::Type::Error;
  }

  fn check(&self, context: &mut TypeContext, cache: &cache::Cache) {
    let base_expr_type = self.base_expr.kind.infer_flatten_type(cache);

    let struct_type = match base_expr_type {
      ast::Type::Struct(struct_type) => struct_type,
      // TODO: Implement.
      ast::Type::This(_) => return,
      // REVIEW: Investigate this strategy. Shouldn't we be using `unreachable!()` instead?
      _ => {
        context.diagnostics.push(
          codespan_reporting::diagnostic::Diagnostic::error()
            .with_message("expression is not a struct"),
        );

        return;
      }
    };

    if !struct_type.fields.iter().any(|x| x.0 == self.member_name) {
      // context.diagnostic_builder.error(format!(
      //   "struct type `{}` does not contain a field named `{}`",
      //   struct_type.name, self.member_name
      // ));
    }
  }
}

impl Check for ast::Closure {
  fn infer_type(&self, cache: &cache::Cache) -> ast::Type {
    TypeContext::infer_prototype_type(
      &self.prototype,
      TypeContext::infer_return_value_type(&self.body, cache),
    )
  }

  fn check(&self, context: &mut TypeContext, cache: &cache::Cache) {
    // REVIEW: Might need to mirror `Function`'s type check.
    let previous_function_id = context.current_function_id.clone();

    context.current_function_id = Some(self.id);

    if self.prototype.accepts_instance {
      context.diagnostics.push(
        codespan_reporting::diagnostic::Diagnostic::error()
          .with_message("closures cannot accept instances"),
      );
    }

    self.prototype.check(context, cache);
    self.body.check(context, cache);
    context.current_function_id = previous_function_id;
  }
}

impl Check for ast::TypeAlias {
  // REVIEW: Don't we need to implement `infer_type` here? Seems like not. Confirm.
}

impl Check for ast::Pattern {
  //
}

impl Check for ast::IntrinsicCall {
  // TODO: Implement.
}

impl Check for ast::ExternStatic {
  fn infer_type(&self, _cache: &cache::Cache) -> ast::Type {
    self.ty.clone()
  }
}

impl Check for ast::StructValue {
  fn infer_type(&self, cache: &cache::Cache) -> ast::Type {
    let struct_type = match cache.force_get(&self.target_id.unwrap()) {
      ast::NodeKind::StructType(struct_type) => struct_type,
      _ => unreachable!(),
    };

    // REVIEW: Is this the correct type? We might need this one in order to unify with the original struct type.
    ast::Type::Struct(struct_type.clone())
  }

  fn check(&self, context: &mut TypeContext, cache: &cache::Cache) {
    let struct_type_node = cache.force_get(&self.target_id.unwrap());

    let struct_type = match struct_type_node {
      ast::NodeKind::StructType(struct_type) => struct_type,
      _ => unreachable!(),
    };

    if self.fields.len() != struct_type.fields.len() {
      context.diagnostics.push(
        codespan_reporting::diagnostic::Diagnostic::error()
          .with_message("invalid amount of fields in struct value"),
      );

      return;
    }

    for (index, (value_field, struct_field)) in self
      .fields
      .iter()
      .zip(struct_type.fields.iter())
      .enumerate()
    {
      value_field.kind.check(context, cache);

      let value_field_type = value_field.kind.infer_type(cache);

      // FIXME: Uncomment and translate to current codebase.
      // if !unify_option(value_field_type, Some(struct_field.1), cache) {
      //   context.diagnostics.error(format!(
      //     "field and value at position `{}` type for struct `{}` mismatch",
      //     index, struct_type.name
      //   ));
      // }
    }
  }
}

impl Check for ast::Prototype {
  fn check(&self, _context: &mut TypeContext, _cache: &cache::Cache) {
    // TODO: Implement?
  }
}

impl Check for ast::StructType {
  // REVIEW: Implement? This is already a type on itself.
}

impl Check for ast::UnaryExpr {
  fn infer_type(&self, cache: &cache::Cache) -> ast::Type {
    let expr_type = self.expr.kind.infer_type(cache);

    // Short-circuit if the expression's type is unit.
    if expr_type.is_a_unit() {
      return ast::Type::Unit;
    }

    return match self.operator {
      ast::OperatorKind::AddressOf => ast::Type::Pointer(Box::new(expr_type)),
      ast::OperatorKind::Cast => self.cast_type.as_ref().unwrap().clone(),
      ast::OperatorKind::Not => ast::Type::Basic(ast::BasicType::Bool),
      ast::OperatorKind::SubtractOrNegate => expr_type,
      // BUG: The type must be whatever was inside the pointer; otherwise assume type error.
      ast::OperatorKind::MultiplyOrDereference => expr_type,
      _ => unreachable!(),
    };
  }

  fn check(&self, context: &mut TypeContext, cache: &cache::Cache) {
    let expr_type = &self.expr.kind.infer_flatten_type(cache);

    match self.operator {
      ast::OperatorKind::MultiplyOrDereference => {
        if !context.in_unsafe_block {
          context.diagnostics.push(
            codespan_reporting::diagnostic::Diagnostic::error()
              .with_message("can only dereference inside an unsafe block"),
          );
        }

        if !matches!(expr_type, ast::Type::Pointer(_)) {
          context.diagnostics.push(
            codespan_reporting::diagnostic::Diagnostic::error()
              .with_message("can only dereference pointers"),
          );
        }
      }
      ast::OperatorKind::Not => {
        if !expr_type.is(&ast::Type::Basic(ast::BasicType::Bool)) {
          context.diagnostics.push(
            codespan_reporting::diagnostic::Diagnostic::error()
              .with_message("can only negate boolean expressions"),
          );
        }
      }
      ast::OperatorKind::SubtractOrNegate => {
        // TODO: Include floats.
        if !matches!(expr_type, ast::Type::Basic(ast::BasicType::Int(_))) {
          // REVISE: Error message too similar to the boolean negation case.
          context.diagnostics.push(
            codespan_reporting::diagnostic::Diagnostic::error()
              .with_message("can only negate integer or float expressions"),
          );
        }
      }
      ast::OperatorKind::AddressOf => {
        // TODO: Implement.
        return;
      }
      ast::OperatorKind::Cast => {
        // REVIEW: What if it's an alias? This could be solved by flattening above.
        if !matches!(expr_type, ast::Type::Basic(_))
          || !matches!(self.cast_type.as_ref().unwrap(), ast::Type::Basic(_))
        {
          context.diagnostics.push(
            codespan_reporting::diagnostic::Diagnostic::error()
              .with_message("can only cast between primitive types"),
          );
        } else if expr_type.is(self.cast_type.as_ref().unwrap()) {
          context.diagnostics.push(
            codespan_reporting::diagnostic::Diagnostic::warning()
              .with_message("redundant cast to the same type"),
          );
        }
      }
      _ => unreachable!(),
    };
  }
}

impl Check for ast::Enum {
  // REVIEW: Isn't there a need for its variants to be considered integer types?
}

impl Check for ast::AssignStmt {
  fn check(&self, context: &mut TypeContext, cache: &cache::Cache) {
    // TODO: Need to unify the value and the target's type.

    let assignee_type = self.assignee_expr.kind.infer_flatten_type(cache);

    if matches!(assignee_type, ast::Type::Reference(_)) {
      context.diagnostics.push(
        codespan_reporting::diagnostic::Diagnostic::error()
          .with_message("can't assign to a reference; references cannot be reseated"),
      );

      // REVIEW: We should continue gathering other diagnostics (ex. immutable)?
      return;
    }

    // NOTE: References cannot be reseated/assigned-to, only pointers.
    let is_pointer = matches!(assignee_type, ast::Type::Pointer(_));

    // REVISE: This checks are superficial. They do not consider
    // ... that expressions may be nested (ie. parentheses expr.).
    let is_array_indexing = matches!(self.assignee_expr.kind, ast::NodeKind::IndexingExpr(_));
    let is_variable_ref = matches!(self.assignee_expr.kind, ast::NodeKind::Reference(_));

    // FIXME: What if the member accessed is a method? Is that even possible?
    // ... Maybe to disambiguate that specific case we'd need to add a check below.
    let is_member_access = matches!(self.assignee_expr.kind, ast::NodeKind::MemberAccess(_));

    // TODO: Missing member access (struct fields) support.
    // NOTE: The assignee expression may only be an expression of type `Pointer`
    // or `Reference`, a variable reference, or an array indexing.
    if !is_pointer && !is_variable_ref && !is_array_indexing && !is_member_access {
      context.diagnostics.push(
          codespan_reporting::diagnostic::Diagnostic::error()
            .with_message("assignee must be an expression of pointer or reference type, a member access expression, a variable reference, or an array indexing expression"),
        );
    } else if is_variable_ref {
      // If the assignee is a variable reference, ensure that the variable is mutable.
      match &self.assignee_expr.kind {
        ast::NodeKind::Reference(variable_ref) => {
          let binding_node = cache.force_get(&variable_ref.pattern.target_id.unwrap());

          match binding_node {
            ast::NodeKind::BindingStmt(binding)
              if binding.modifier != ast::BindingModifier::Mutable =>
            {
              context.diagnostics.push(
                codespan_reporting::diagnostic::Diagnostic::error()
                  .with_message("assignee is immutable"),
              );
            }
            // TODO: Parameters should be immutable by default.
            _ => {}
          };
        }
        _ => unreachable!(),
      };
    }

    // REVIEW: should this checks be placed before or after?
    self.assignee_expr.kind.check(context, cache);
    self.value.kind.check(context, cache);
  }
}

impl Check for ast::ContinueStmt {
  fn check(&self, context: &mut TypeContext, _cache: &cache::Cache) {
    if !context.in_loop {
      context.diagnostics.push(
        codespan_reporting::diagnostic::Diagnostic::error()
          .with_message("continue statement may only occur inside loops"),
      );
    }
  }
}

impl Check for ast::IndexingExpr {
  fn infer_type(&self, cache: &cache::Cache) -> ast::Type {
    let target_array = cache.force_get(&self.target_id.unwrap());
    let array_type = target_array.infer_flatten_type(cache);

    // TODO: In the future, add support for when indexing strings.
    let element_type = match array_type {
      ast::Type::Array(element_type, _) => element_type.as_ref().clone(),
      _ => unreachable!(),
    };

    element_type
  }

  fn check(&self, context: &mut TypeContext, cache: &cache::Cache) {
    self.index_expr.kind.check(context, cache);

    let index_expr_type = self.index_expr.kind.infer_flatten_type(cache);

    let is_index_proper_type =
      index_expr_type.is(&ast::Type::Basic(ast::BasicType::Int(ast::IntSize::U32)));

    if !is_index_proper_type {
      context.diagnostics.push(
        codespan_reporting::diagnostic::Diagnostic::error()
          .with_message("array index expression must be of type `U32`"),
      );

      // REVIEW: Should we actually not continue?
      // Can't continue if the index expression is not of the proper type.
      return;
    }

    let target_array = cache.force_get(&self.target_id.unwrap());
    let target_expr_type = target_array.infer_flatten_type(cache);

    // REVIEW: Any way of avoiding nesting?
    if let ast::Type::Array(_, length) = target_expr_type {
      // If the index expression is not a constant expression, then
      // this scope must fall under a bounds check for that index, and
      // the length of the array.
      if !self.index_expr.kind.is_constant_expr() {
        // TODO: Support for dynamic index, but require a bounds check.

        context.diagnostics.push(
          codespan_reporting::diagnostic::Diagnostic::error()
            .with_message("array index expression must be a constant expression"),
        );
      } else {
        // FIXME: Why we allow unary expressions on const expressions if we extract their value intact?
        let index_expr_literal = crate::force_match!(
          self
            .index_expr
            .kind
            .find_node(|node| matches!(node, ast::NodeKind::Literal(..)))
            .unwrap(),
          ast::NodeKind::Literal
        );

        let index_expr_value = match index_expr_literal {
          // NOTE: Safe cast because we know that the literal is of type `U32` at this point.
          ast::Literal::Int(value, _) => *value as u32,
          _ => unreachable!(),
        };

        // NOTE: Because of its type, the index expression will never be lower than 0.
        if index_expr_value >= length {
          context.diagnostics.push(
            codespan_reporting::diagnostic::Diagnostic::error()
              .with_message("array index expression must be within the bounds of the array"),
          );
        }
      }
    } else {
      context.diagnostics.push(
        codespan_reporting::diagnostic::Diagnostic::error()
          .with_message("can only index into arrays"),
      );
    }
  }
}

impl Check for ast::StaticArrayValue {
  fn infer_type(&self, cache: &cache::Cache) -> ast::Type {
    // TODO: Temporary, until type-inference is implemented.
    // We assume that the length is `0` if the explicit type is provided, otherwise
    // the array type is determined by the first element.
    let array_element_type = if let Some(explicit_type) = &self.explicit_type {
      explicit_type.clone()
    } else {
      self.elements.first().unwrap().kind.infer_type(cache)
    };

    // REVIEW: Is the length conversion safe?
    ast::Type::Array(Box::new(array_element_type), self.elements.len() as u32)
  }

  fn check(&self, context: &mut TypeContext, cache: &cache::Cache) {
    let mut mixed_elements_flag = false;

    let expected_element_type = if let Some(explicit_type) = &self.explicit_type {
      explicit_type.clone()
    } else {
      self.elements.first().unwrap().kind.infer_type(cache)
    };

    // TODO: Skip the first element during iteration, as it is redundant.
    for element in &self.elements {
      // Report this error only once.
      if !mixed_elements_flag && element.kind.infer_type(cache) != expected_element_type {
        context.diagnostics.push(
          codespan_reporting::diagnostic::Diagnostic::error()
            .with_message("array elements must all be of the same type"),
        );

        mixed_elements_flag = true;
      }

      element.kind.check(context, cache);
    }
  }
}

impl Check for ast::UnsafeExpr {
  fn infer_type(&self, cache: &cache::Cache) -> ast::Type {
    self.0.kind.infer_type(cache)
  }

  fn check(&self, context: &mut TypeContext, cache: &cache::Cache) {
    // REVIEW: To avoid problems with nested cases, save a buffer here, then restore?
    context.in_unsafe_block = true;
    self.0.kind.check(context, cache);
    context.in_unsafe_block = false;
  }
}

impl Check for ast::ExternFunction {
  fn infer_type(&self, _cache: &cache::Cache) -> ast::Type {
    TypeContext::infer_prototype_type(
      &self.prototype,
      self.prototype.return_type_annotation.clone(),
    )
  }

  fn check(&self, context: &mut TypeContext, _cache: &cache::Cache) {
    if self.prototype.accepts_instance {
      context.diagnostics.push(
        codespan_reporting::diagnostic::Diagnostic::error()
          .with_message("extern functions cannot accept instances"),
      );
    }
  }
}

impl Check for ast::Parameter {
  fn infer_type(&self, _cache: &cache::Cache) -> ast::Type {
    self.ty.clone()
  }
}

impl Check for ast::BlockExpr {
  fn infer_type(&self, cache: &cache::Cache) -> ast::Type {
    if let Some(yields_value) = &self.yields {
      return yields_value.kind.infer_type(cache);
    }
    // FIXME: Ensure this logic is correct, and that it will work as expected.
    // BUG: The function to infer return values uses this infer method, so function types CAN be never!
    // If at least one statement evaluates to type never, the type of this
    // block is also never.
    else if self
      .statements
      .iter()
      .any(|statement| statement.kind.infer_flatten_type(cache).is_a_never())
    {
      return ast::Type::Never;
    }

    ast::Type::Unit
  }

  fn check(&self, context: &mut TypeContext, cache: &cache::Cache) {
    for statement in &self.statements {
      statement.kind.check(context, cache);
    }
  }
}

impl Check for ast::Reference {
  fn infer_type(&self, cache: &cache::Cache) -> ast::Type {
    // REVIEW: We should have some sort of caching specifically applied to this construct.
    cache
      .force_get(&self.pattern.target_id.unwrap())
      .infer_type(cache)
  }
}

impl Check for ast::Literal {
  fn infer_type(&self, _cache: &cache::Cache) -> ast::Type {
    ast::Type::Basic(match self {
      ast::Literal::Bool(_) => ast::BasicType::Bool,
      ast::Literal::Char(_) => ast::BasicType::Char,
      ast::Literal::Int(_, size) => ast::BasicType::Int(size.clone()),
      ast::Literal::String(_) => ast::BasicType::String,
      ast::Literal::Nullptr(ty) => return ast::Type::Pointer(Box::new(ty.clone())),
    })
  }
}

impl Check for ast::IfExpr {
  fn infer_type(&self, cache: &cache::Cache) -> ast::Type {
    let then_expr_type = self.then_expr.kind.infer_flatten_type(cache);

    // At least the two main branches must be present in order for a value
    // to possibly evaluate.
    if self.else_expr.is_none() {
      return ast::Type::Unit;
    }

    // TODO: Take into consideration newly-added alternative branches.

    let else_expr = self.else_expr.as_ref().unwrap();
    let else_expr_type = else_expr.kind.infer_flatten_type(cache);

    // Default to type unit if both branches are of incompatible types.
    then_expr_type
      .coercion(&else_expr_type)
      .unwrap_or(ast::Type::Unit)
  }

  fn check(&self, context: &mut TypeContext, cache: &cache::Cache) {
    let condition_type = self.condition.kind.infer_flatten_type(cache);

    if !condition_type.is(&ast::Type::Basic(ast::BasicType::Bool)) {
      context.diagnostics.push(
        codespan_reporting::diagnostic::Diagnostic::error()
          .with_message("if statement condition must evaluate to a boolean"),
      );
    }

    // TODO: Check for type-mismatch between branches (they may yield).

    self.condition.kind.check(context, cache);
    self.then_expr.kind.check(context, cache);

    if let Some(else_block) = &self.else_expr {
      else_block.kind.check(context, cache);
    }
  }
}

impl Check for ast::BinaryExpr {
  fn infer_type(&self, cache: &cache::Cache) -> ast::Type {
    match self.operator {
      ast::OperatorKind::LessThan
      | ast::OperatorKind::GreaterThan
      | ast::OperatorKind::Equality
      | ast::OperatorKind::And
      | ast::OperatorKind::Or
      | ast::OperatorKind::Nand
      | ast::OperatorKind::Nor
      | ast::OperatorKind::Xor => ast::Type::Basic(ast::BasicType::Bool),
      _ => self.left.kind.infer_type(cache),
    }
  }

  fn check(&self, context: &mut TypeContext, cache: &cache::Cache) {
    let left_type = self.left.kind.infer_flatten_type(cache);
    let right_type = self.right.kind.infer_flatten_type(cache);

    // TODO: Also add checks for when using operators with wrong values (ex. less-than or greater-than comparison of booleans).

    if !left_type.is(&right_type) {
      context.diagnostics.push(
        codespan_reporting::diagnostic::Diagnostic::error()
          .with_message("binary expression operands must be the same type"),
      );

      return;
    }

    // REVIEW: Check for mixed operators that don't make sense (ex. addition, then a comparison operator)?

    // NOTE: By this point, it is assumed that both operands are of the same type.
    match self.operator {
      ast::OperatorKind::Add
      | ast::OperatorKind::SubtractOrNegate
      | ast::OperatorKind::MultiplyOrDereference
      | ast::OperatorKind::Divide
      | ast::OperatorKind::LessThan
      | ast::OperatorKind::GreaterThan => {
        // REVIEW: What about floats?
        if !matches!(left_type, ast::Type::Basic(ast::BasicType::Int(_))) {
          context.diagnostics.push(
            codespan_reporting::diagnostic::Diagnostic::error()
              .with_message("binary expression operands must be both integers"),
          );
        }
      }
      // TODO: Equality operator, and others? Implement.
      _ => {}
    };

    self.left.kind.check(context, cache);
    self.right.kind.check(context, cache);
  }
}

impl Check for ast::BreakStmt {
  fn check(&self, context: &mut TypeContext, _cache: &cache::Cache) {
    if !context.in_loop {
      context.diagnostics.push(
        codespan_reporting::diagnostic::Diagnostic::error()
          .with_message("break statement may only occur inside loops"),
      );
    }
  }
}

impl Check for ast::InlineExprStmt {
  fn infer_type(&self, cache: &cache::Cache) -> ast::Type {
    self.expr.kind.infer_type(cache)
  }

  fn check(&self, context: &mut TypeContext, cache: &cache::Cache) {
    self.expr.kind.check(context, cache);
  }
}

impl Check for ast::BindingStmt {
  // BUG: This causes a bug where the string literal is not accessed (left as `i8**`). The let-statement didn't have a type before.
  fn infer_type(&self, cache: &cache::Cache) -> ast::Type {
    self.value.kind.infer_type(cache)
  }

  fn check(&self, context: &mut TypeContext, cache: &cache::Cache) {
    let value_type = self.value.kind.infer_type(cache);
    let ty = self.infer_type(cache);

    // FIXME: This is redundant. The same type is being compared!
    if !ty.flat_is(&value_type, cache) {
      context.diagnostics.push(
        codespan_reporting::diagnostic::Diagnostic::error().with_message(format!(
          "variable declaration of `{}` value and type mismatch",
          self.name
        )),
      );
    }

    self.value.kind.check(context, cache);
  }

  fn report_constraints(&mut self, context: &mut TypeContext, cache: &cache::Cache) {
    // TODO: Abstract this to a method that accepts a type and a list of constraints.
    // ... This way it can be reused for other implementations.
    if !matches!(self.ty, ast::Type::Variable(_)) {
      return;
    }

    context.constraints.push((
      self.ty.clone(),
      self.value.kind.infer_type(cache),
      TypeConstrainKind::Equality,
    ));
  }

  fn post_unification(&mut self, context: &mut TypeContext, _cache: &cache::Cache) {
    let variable_type_id = match self.ty {
      ast::Type::Variable(ref id) => id,
      _ => return,
    };

    // REVIEW: What if there was no substitution defined? Unsafe unwrap?
    // ... Or maybe this is the part where we report diagnostics in this implementation?
    // ... Bingo! The `unify` method is where the diagnostics are reported.
    self.ty = context.substitutions.get(variable_type_id).unwrap().clone();
  }
}

impl Check for ast::ReturnStmt {
  // FIXME: This implies that the return statement may be used as an expression,
  // ... but that's currently not possible. Instead, this type is currently solely
  // ... used to determine if all paths return a value?
  fn infer_type(&self, _cache: &cache::Cache) -> ast::Type {
    ast::Type::Never
  }

  fn check(&self, context: &mut TypeContext, cache: &cache::Cache) {
    let current_function_node = cache.force_get(&context.current_function_id.unwrap());
    let mut name = None;

    let return_type = TypeContext::infer_return_value_type(
      match &current_function_node {
        ast::NodeKind::Function(function) => {
          name = Some(function.name.clone());

          &function.body
        }
        ast::NodeKind::Closure(closure) => &closure.body,
        _ => unreachable!(),
      },
      cache,
    )
    .flatten(cache);

    // REVISE: Whether a function returns is already checked. Limit this to comparing the types only.
    if !return_type.is_a_unit() && self.value.is_none() {
      context.diagnostics.push(
        codespan_reporting::diagnostic::Diagnostic::error()
          .with_message("return statement must return a value"),
      );
    } else if return_type.is_a_unit() && self.value.is_some() {
      context.diagnostics.push(
        codespan_reporting::diagnostic::Diagnostic::error()
          .with_message("return statement must not return a value"),
      );

      // REVIEW: Returning at this point. Is this okay?
      return;
    }

    if let Some(value) = &self.value {
      let value_type = value.kind.infer_flatten_type(cache);

      if !return_type.is(&value_type) {
        context.diagnostics.push(
          codespan_reporting::diagnostic::Diagnostic::error().with_message(format!(
            "return statement value and prototype return type mismatch for {}",
            // REVISE: Change the actual name to this on its initialization.
            if let Some(name) = name {
              format!("function `{}`", name)
            } else {
              "closure".to_string()
            }
          )),
        );
      }

      value.kind.check(context, cache);
    }
  }
}

impl Check for ast::Function {
  fn infer_type(&self, cache: &cache::Cache) -> ast::Type {
    // REVIEW: Why not use annotated return type if defined?
    TypeContext::infer_prototype_type(
      &self.prototype,
      TypeContext::infer_return_value_type(&self.body, cache),
    )
  }

  fn check(&self, context: &mut TypeContext, cache: &cache::Cache) {
    let previous_function_key = context.current_function_id.clone();

    context.current_function_id = Some(self.cache_id);

    if self.prototype.accepts_instance && !context.in_impl {
      context.diagnostics.push(
        codespan_reporting::diagnostic::Diagnostic::error()
          .with_message("cannot accept instance in a non-impl function"),
      );
    }

    // let return_type = self.body.infer_type(cache);

    // if !TypeContext::compare(&return_type, &self.body.infer_type(cache), cache) {
    //   context.diagnostics.push(
    //     codespan_reporting::diagnostic::Diagnostic::error().with_message(format!(
    //       "function body and prototype return type mismatch for function `{}`",
    //       self.name
    //     )),
    //   );
    // }

    if self.prototype.is_variadic {
      context.diagnostics.push(
        codespan_reporting::diagnostic::Diagnostic::error().with_message(format!(
          "function `{}` cannot be variadic; only externs are allowed to be variadic",
          self.name
        )),
      );
    }

    if self.name == lowering::MAIN_FUNCTION_NAME {
      let main_function_type = ast::Type::Function(ast::FunctionType {
        parameter_types: vec![
          ast::Type::Basic(ast::BasicType::Int(ast::IntSize::I32)),
          ast::Type::Pointer(Box::new(ast::Type::Basic(ast::BasicType::String))),
        ],
        return_type: Box::new(ast::Type::Basic(ast::BasicType::Int(ast::IntSize::I32))),
        is_variadic: false,
        is_extern: false,
      });

      if self.infer_type(cache) != main_function_type {
        context.diagnostics.push(
          codespan_reporting::diagnostic::Diagnostic::error()
            .with_message("the `main` function has an invalid signature")
            .with_notes(vec![String::from("should accept a first parameter of type `Int`, a second one of type `*Str`, and the return type should be `Int`"), String::from("cannot be marked as variadic or extern")]),
        );
      }
    }

    let return_type = TypeContext::infer_return_value_type(&self.body, cache);

    if !return_type.is_a_unit() {
      // If at least one statement's type evaluates to never, it
      // means that all paths are covered, because code execution will
      // always return at one point before reaching (or at) the end of the function.
      let all_paths_covered = self
        .body
        .statements
        .iter()
        .any(|statement| statement.kind.infer_flatten_type(cache).is_a_never());

      if !all_paths_covered {
        context.diagnostics.push(
          codespan_reporting::diagnostic::Diagnostic::error()
            // TODO: Function name.
            .with_message("not all paths return a value"),
        );
      }
    }

    self.prototype.check(context, cache);
    self.body.check(context, cache);
    context.current_function_id = previous_function_key;
  }

  fn post_unification(&mut self, context: &mut TypeContext, _cache: &cache::Cache) {
    // TODO: Parameters, etc.

    self.prototype.return_type_annotation =
      context.substitute(self.prototype.return_type_annotation.clone());
  }
}

impl Check for ast::CallExpr {
  fn infer_type(&self, cache: &cache::Cache) -> ast::Type {
    let callee_expr_type = self.callee_expr.kind.infer_type(cache);

    match callee_expr_type {
      ast::Type::Function(callable_type) => callable_type.return_type.as_ref().clone(),
      _ => ast::Type::Error,
    }
  }

  fn check(&self, context: &mut TypeContext, cache: &cache::Cache) {
    self.callee_expr.kind.check(context, cache);

    // REVIEW: Consider adopting a `expected` and `actual` API for diagnostics, when applicable.
    // REVIEW: Need access to the current function?

    // TODO: Isn't there a need to flatten this type?
    let callee_expr_type = self.callee_expr.kind.infer_type(cache);

    if !matches!(callee_expr_type, ast::Type::Function(_)) {
      context.diagnostics.push(
        codespan_reporting::diagnostic::Diagnostic::error()
          .with_message("call expression's callee is not actually callable"),
      );

      // Cannot continue.
      return;
    }

    let callee_type = match callee_expr_type {
      ast::Type::Function(callable_type) => callable_type,
      _ => unreachable!(),
    };

    // REVISE: Better, simpler way of doing this?
    // let attributes;

    if callee_type.is_extern && !context.in_unsafe_block {
      context.diagnostics.push(
        codespan_reporting::diagnostic::Diagnostic::error().with_message(format!(
          "extern function call to `{}` may only occur inside an unsafe block",
          // TODO: Need name.
          "<pending>"
        )),
      );
    }

    // for attribute in attributes {
    //   // TODO: Keep it simple for now, but later, we can improve the attribute system.
    //   match attribute.name.as_str() {
    //     "deprecated" => context
    //       .diagnostic_builder
    //       .warning(format!("function `{}` is deprecated", name)),
    //     _ => context.diagnostic_builder.warning(format!(
    //       "use of unrecognized attribute `{}`",
    //       attribute.name
    //     )),
    //   };
    // }

    let min_arg_count = callee_type.parameter_types.len();
    let actual_arg_count = self.arguments.len();

    // Verify argument count.
    if (!callee_type.is_variadic && actual_arg_count != min_arg_count)
      || (callee_type.is_variadic && actual_arg_count < min_arg_count)
    {
      context.diagnostics.push(
        codespan_reporting::diagnostic::Diagnostic::error()
          .with_message("call expression has an invalid amount of arguments"),
      );
    }

    // Compare argument and parameter types.
    for (parameter_type, argument) in callee_type
      .parameter_types
      .iter()
      .zip(self.arguments.iter())
    {
      let argument_type = argument.kind.infer_type(cache);

      if !parameter_type.flat_is(&argument_type, cache) {
        // TODO: Include callee name in the error message.
        context.diagnostics.push(
          codespan_reporting::diagnostic::Diagnostic::error().with_message(format!(
            "function call argument and parameter `{}` type mismatch",
            // TODO: Parameter name.
            "pending_name"
          )),
        );
      }
    }

    for argument in &self.arguments {
      argument.kind.check(context, cache);
    }
  }
}

impl Check for ast::LoopStmt {
  fn check(&self, context: &mut TypeContext, cache: &cache::Cache) {
    if let Some(condition) = &self.condition {
      let condition_type = condition.kind.infer_flatten_type(cache);

      if !condition_type.is(&ast::Type::Basic(ast::BasicType::Bool)) {
        context.diagnostics.push(
          codespan_reporting::diagnostic::Diagnostic::error()
            .with_message("loop condition must evaluate to a boolean"),
        );
      }

      condition.kind.check(context, cache);
    }

    // REVIEW: To avoid problems with nested cases, save a buffer here, then restore?
    context.in_loop = true;
    self.body.check(context, cache);
    context.in_loop = false;
  }
}

#[cfg(test)]
mod tests {
  use super::*;

  // TODO: Move this test to the `ast` file.
  // #[test]
  // fn is_null_pointer_type() {
  //   let null_ptr_type = ast::Type::Pointer(Box::new(ast::Type::Basic(ast::BasicType::Null)));

  //   assert!(TypeContext::is_null_pointer_type(&null_ptr_type));
  //   assert!(!TypeContext::is_null_pointer_type(&ast::Type::Unit));
  // }

  #[test]
  fn proper_initial_values() {
    let type_context = TypeContext::new();

    assert!(type_context.constraints.is_empty());
    assert!(type_context.substitutions.is_empty());
    assert!(type_context.diagnostics.is_empty());
    assert!(type_context.usings.is_empty());
    assert!(type_context.current_function_id.is_none());
    assert!(!type_context.in_impl);
    assert!(!type_context.in_loop);
    assert!(!type_context.in_unsafe_block);
  }

  #[test]
  fn occurs_in() {
    let mut type_context = TypeContext::new();
    let first_index_id = 0;
    let second_index_id = first_index_id + 1;

    type_context
      .substitutions
      .insert(first_index_id, ast::Type::Variable(first_index_id));

    type_context
      .substitutions
      .insert(second_index_id, ast::Type::Unit);

    let subject_type_variable = ast::Type::Variable(first_index_id);

    assert!(type_context.occurs_in(first_index_id, &subject_type_variable));
    assert!(!type_context.occurs_in(second_index_id, &subject_type_variable));
    assert!(!type_context.occurs_in(first_index_id, &ast::Type::Unit));
  }

  #[test]
  fn create_type_variable() {
    let mut type_context = TypeContext::new();

    assert_eq!(type_context.create_type_variable(), ast::Type::Variable(0));
    assert_eq!(1, type_context.substitutions.len());
  }

  #[test]
  fn solve_constraints() {
    let mut type_context = TypeContext::new();

    // TODO: Add actual constraints to complete this test.

    type_context.solve_constraints();
    assert!(type_context.constraints.is_empty());
  }

  #[test]
  fn substitute() {
    let mut type_context = TypeContext::new();

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
  fn hindley_milner_type_inference() {
    let mut type_context = TypeContext::new();
    let cache = cache::Cache::new();
    let type_variable_id = 0;

    let mut binding_stmt = ast::BindingStmt {
      name: String::from("a"),
      ty: ast::Type::Variable(type_variable_id),
      // TODO: Use `Mock` scaffolding.
      value: Box::new(ast::Node {
        kind: ast::NodeKind::Literal(ast::Literal::Bool(true)),
        cached_type: None,
      }),
      cache_id: 0,
      modifier: ast::BindingModifier::Immutable,
    };

    // TODO: Use the empty array type test.
    // TODO: Also, create a second test for inferring of parameter types.

    binding_stmt.report_constraints(&mut type_context, &cache);
    type_context.solve_constraints();
    binding_stmt.post_unification(&mut type_context, &cache);
    assert_eq!(binding_stmt.ty, ast::Type::Basic(ast::BasicType::Bool));
  }

  // TODO: Add tests for `compare()`, `infer_and_flatten_type()`, `flatten_type()`, and others.
}
