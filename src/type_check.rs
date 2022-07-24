use crate::{
  ast, cache, lowering,
  type_system::{Check, TypeContext},
  visitor::AnalysisVisitor,
};

pub struct TypeCheckContext<'a> {
  pub diagnostics: Vec<codespan_reporting::diagnostic::Diagnostic<usize>>,
  in_unsafe_block: bool,
  in_struct_impl: bool,
  current_function_id: Option<cache::Id>,
  usings: Vec<ast::Using>,
  /// A map from a type variable's id to a type.
  ///
  /// This serves as a buffer for type inference to occur. It is
  /// populated during parsing phase, when type variables are created, and
  /// it also is scope-less/context-free.
  substitutions: std::collections::HashMap<usize, ast::Type>,
  type_cache: std::collections::HashMap<cache::Id, ast::Type>,
  bound_checked_arrays: std::collections::HashSet<cache::Id>,
  cache: &'a cache::Cache,
}

impl<'a> TypeCheckContext<'a> {
  pub fn new(cache: &'a cache::Cache) -> Self {
    Self {
      cache,
      type_cache: std::collections::HashMap::new(),
      diagnostics: Vec::new(),
      in_unsafe_block: false,
      in_struct_impl: false,
      current_function_id: None,
      usings: Vec::new(),
      // constraints: Vec::new(),
      substitutions: std::collections::HashMap::new(),
      bound_checked_arrays: std::collections::HashSet::new(),
    }
  }

  /// Validate a call to ensure its arguments and callee type
  /// are compatible. Any encountered diagnostics will be added to
  /// the context's diagnostics.
  ///
  /// This function will automatically flatten both argument and
  /// parameter types for comparison.
  fn validate_fn_call(
    &mut self,
    argument_types: Vec<ast::Type>,
    callee_type: ast::FunctionType,
    cache: &cache::Cache,
  ) {
    let min_arg_count = callee_type.parameter_types.len();
    let actual_arg_count = argument_types.len();

    // Verify argument count.
    if (!callee_type.is_variadic && actual_arg_count != min_arg_count)
      || (callee_type.is_variadic && actual_arg_count < min_arg_count)
    {
      self.diagnostics.push(
        codespan_reporting::diagnostic::Diagnostic::error()
          .with_message("call expression has an invalid amount of arguments"),
      );
    }

    // Compare argument and parameter types.
    for (parameter_type, argument_type) in callee_type
      .parameter_types
      .iter()
      .zip(argument_types.iter())
    {
      if !parameter_type.flat_is(&argument_type, cache) {
        // TODO: Include callee name in the error message.
        self.diagnostics.push(
          codespan_reporting::diagnostic::Diagnostic::error().with_message(format!(
            "function call argument and parameter `{}` type mismatch",
            // TODO: Parameter name.
            "pending_name"
          )),
        );
      }
    }
  }

  pub fn infer_signature_type(
    signature: &ast::Signature,
    return_type: Option<ast::Type>,
  ) -> ast::Type {
    ast::Type::Function(ast::FunctionType {
      return_type: Box::new(return_type.unwrap_or(ast::Type::Unit)),
      parameter_types: signature
        .parameters
        .iter()
        .map(|parameter| parameter.type_hint.as_ref().unwrap().clone())
        .collect(),
      is_variadic: signature.is_variadic,
      is_extern: signature.is_extern,
    })
  }

  pub fn infer_return_value_type(body: &ast::BlockExpr, cache: &cache::Cache) -> ast::Type {
    let body_type = body.infer_type(cache).flatten(cache);

    if !body_type.is_a_never() {
      return body_type;
    }

    let mut ty = ast::Type::Unit;

    // BUG: Finish re-implementing. This is essential.
    // REVISE: Cloning body. This may be a large AST.
    ast::NodeKind::BlockExpr(body.clone()).traverse(|child| {
      if let ast::NodeKind::ReturnStmt(return_stmt) = child {
        // REVIEW: What if the return statement's value is a block that contains a return statement?
        if let Some(return_value) = &return_stmt.value {
          ty = return_value.infer_type(cache);
        }

        // If the return statement is empty, then the function's return type is unit.
        return false;
      }

      true
    });

    ty
  }

  // TODO: Use an enum to specify error type instead of a string.
  // REVIEW: Consider using `Result` instead of `Option`.
  // pub fn compare_signatures(
  //   signature_a: &ast::signature,
  //   signature_b: &ast::signature,
  //   cache: &cache::Cache,
  // ) -> Option<String> {
  //   if signature_a.parameters.len() != signature_b.parameters.len() {
  //     return Some("parameter count".to_string());
  //   }

  //   let parameter_types = signature_a
  //     .parameters
  //     .iter()
  //     .zip(signature_b.parameters.iter())
  //     .map(|(param_def_a, param_def_b)| (param_def_a.ty.clone(), param_def_b.ty.clone()));

  //   for (param_type_a, param_type_b) in parameter_types {
  //     if !Self::compare(&param_type_a, &param_type_b, cache) {
  //       // TODO: Be more specific.
  //       return Some("parameter type".to_string());
  //     }
  //   }

  //   if !Self::compare(
  //     &signature_a.return_type_annotation,
  //     &signature_b.return_type_annotation,
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
  // fn solve_constraints(&mut self) {
  //   // REVIEW: Any way to avoid cloning?
  //   for constrain in self.constraints.clone() {
  //     self.unify(&constrain.0, &constrain.1);
  //   }

  //   self.constraints.clear();
  // }

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

impl<'a> AnalysisVisitor for TypeCheckContext<'a> {
  fn visit_call_expr(&mut self, call_expr: &ast::CallExpr) {
    // REVIEW: Consider adopting a `expected` and `actual` API for diagnostics, when applicable.
    // REVIEW: Need access to the current function?

    // TODO: Isn't there a need to flatten this type?
    let callee_expr_type = call_expr.callee_expr.infer_type(self.cache);

    if !matches!(callee_expr_type, ast::Type::Function(_)) {
      self.diagnostics.push(
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

    if callee_type.is_extern && !self.in_unsafe_block {
      self.diagnostics.push(
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

    self.validate_fn_call(
      call_expr
        .arguments
        .iter()
        // No need to flatten here.
        .map(|argument| argument.infer_type(self.cache))
        .collect(),
      callee_type,
      self.cache,
    );
  }

  fn enter_struct_impl(&mut self, struct_impl: &ast::StructImpl) {
    // FIXME: We can solve this reliance on flags simply by filling a special flag on the
    // ... constructs that perform these checks during parsing. There aren't many cases and
    // ... this also prevents flag repetition between pass states. Not only that, but also it
    // ... fixes the enter/exit inconsistency! Only con would be if a lot more nodes needed this
    // ... flags during later phases of semantic check passes.
    self.in_struct_impl = true;

    for method in &struct_impl.member_methods {
      // TODO: Abstract this check to contain the for loop.
      if !method.signature.accepts_instance {
        self.diagnostics.push(
          codespan_reporting::diagnostic::Diagnostic::error().with_message(format!(
            "implementation method `{}` is missing the instance parameter `this`",
            method.name
          )),
        )
      }
    }

    let target_node = self.cache.force_get(&struct_impl.target_struct_pattern.id);

    // REVISE: Cleanup.
    if let ast::NodeKind::Struct(_target_struct_type) = &target_node {
      if let Some(trait_pattern) = &struct_impl.trait_pattern {
        let trait_node = self.cache.force_get(&trait_pattern.id);

        if let ast::NodeKind::Trait(trait_type) = &trait_node {
          for trait_method in &trait_type.methods {
            let impl_method_result = struct_impl
              .member_methods
              .iter()
              .find(|impl_method| impl_method.name == trait_method.0);

            if let Some(_impl_method) = impl_method_result {
              // TODO: Finish implementing.
              let signature_unification_result =
                // TypeCheckContext::unify_signatures(&trait_method.1, impl_method, cache);
                Some("pending error".to_string());

              if let Some(error) = signature_unification_result {
                // REVISE: Use expected/got system.
                self.diagnostics.push(
                  codespan_reporting::diagnostic::Diagnostic::error().with_message(format!(
                    "signature of implementation method `{}` for trait `{}` mismatch in {}",
                    "pending impl method name", trait_type.name, error
                  )),
                )
              }
            } else {
              self.diagnostics.push(
                codespan_reporting::diagnostic::Diagnostic::error().with_message(format!(
                  "required method `{}` not implemented",
                  trait_method.0
                )),
              );
            }
          }
        } else {
          self.diagnostics.push(
            codespan_reporting::diagnostic::Diagnostic::error().with_message(format!(
              "cannot implement non-trait `{}`",
              &trait_pattern.base_name
            )),
          );
        }
      }
    } else {
      self.diagnostics.push(
        codespan_reporting::diagnostic::Diagnostic::error().with_message(format!(
          "cannot implement for a non-struct type `{}`",
          struct_impl.target_struct_pattern.base_name
        )),
      );
    }
  }

  fn exit_struct_impl(&mut self, _struct_impl: &ast::StructImpl) {
    self.in_struct_impl = false;
  }

  fn visit_using(&mut self, using: &ast::Using) {
    // FIXME: Can't just push the import once encountered; only when it's actually used.
    self.usings.push(using.clone());
  }

  fn visit_sizeof_intrinsic(&mut self, sizeof_intrinsic: &ast::SizeofIntrinsic) {
    if sizeof_intrinsic.ty.flatten(self.cache).is(&ast::Type::Unit) {
      self.diagnostics.push(
        codespan_reporting::diagnostic::Diagnostic::error()
          .with_message("cannot determine size of unit type"),
      );
    }
  }

  fn visit_member_access(&mut self, member_access: &ast::MemberAccess) {
    let base_expr_type = member_access.base_expr.infer_flatten_type(self.cache);

    let struct_type = match base_expr_type {
      ast::Type::Struct(struct_type) => struct_type,
      // TODO: Implement.
      ast::Type::This(_) => return,
      // REVIEW: Investigate this strategy. Shouldn't we be using `unreachable!()` instead?
      _ => {
        self.diagnostics.push(
          codespan_reporting::diagnostic::Diagnostic::error()
            .with_message("expression is not a struct"),
        );

        return;
      }
    };

    if !struct_type
      .fields
      .iter()
      .any(|x| x.0 == member_access.member_name)
    {
      // TODO:
      // context.diagnostic_builder.error(format!(
      //   "struct type `{}` does not contain a field named `{}`",
      //   struct_type.name, self.member_name
      // ));
    }
  }

  fn visit_closure(&mut self, closure: &ast::Closure) {
    // REVIEW: Might need to mirror `Function`'s type check.
    let previous_function_id = self.current_function_id.clone();

    self.current_function_id = Some(closure.id);

    if closure.signature.accepts_instance {
      self.diagnostics.push(
        codespan_reporting::diagnostic::Diagnostic::error()
          .with_message("closures cannot accept instances"),
      );
    }

    self.current_function_id = previous_function_id;
  }

  fn visit_intrinsic_call(&mut self, intrinsic_call: &ast::IntrinsicCall) {
    // TODO: Redundant to have function return types.
    let target_signature: (Vec<ast::Type>, ast::Type) = match intrinsic_call.kind {
      ast::IntrinsicKind::LengthOf => (
        // Cannot define array type directly. Use the any type for comparison.
        vec![ast::Type::Any],
        ast::Type::Basic(ast::BasicType::Int(ast::IntSize::I32)),
      ),
    };

    let target_function_type = ast::FunctionType {
      is_extern: false,
      is_variadic: false,
      parameter_types: target_signature.0,
      return_type: Box::new(target_signature.1),
    };

    self.validate_fn_call(
      intrinsic_call
        .arguments
        .iter()
        // No need to flatten.
        .map(|argument| argument.infer_type(self.cache))
        .collect(),
      target_function_type,
      self.cache,
    );

    // Special case because of the static array type.
    if matches!(intrinsic_call.kind, ast::IntrinsicKind::LengthOf)
      && intrinsic_call.arguments.len() == 1
    {
      let target_array = intrinsic_call.arguments.first().unwrap();
      let target_array_type = target_array.infer_flatten_type(self.cache);

      if !matches!(target_array_type, ast::Type::Array(..)) {
        self.diagnostics.push(
          codespan_reporting::diagnostic::Diagnostic::error()
            .with_message("cannot determine static length of non-array type"),
        );
      }
    }
  }

  fn visit_range(&mut self, range: &ast::Range) {
    // NOTE: No need to check whether the range's bounds are constant
    // ... expressions, this is ensured by the parser.

    let start_int = match &range.start {
      ast::Literal::Int(value, _) => value,
      _ => unreachable!(),
    };

    let end_int = match &range.end {
      ast::Literal::Int(value, _) => value,
      _ => unreachable!(),
    };

    if start_int > end_int {
      self.diagnostics.push(
        codespan_reporting::diagnostic::Diagnostic::error().with_message(String::from(
          "range start must be less than or equal to end",
        )),
      );
    }
  }

  fn enter_function(&mut self, function: &ast::Function) {
    let previous_function_key = self.current_function_id.clone();

    self.current_function_id = Some(function.id);

    let signature = function.signature;

    if signature.accepts_instance && !self.in_struct_impl {
      self.diagnostics.push(
        codespan_reporting::diagnostic::Diagnostic::error()
          .with_message("cannot accept instance in a non-impl function"),
      );
    }

    if signature.is_variadic {
      self.diagnostics.push(
        codespan_reporting::diagnostic::Diagnostic::error().with_message(format!(
          "function `{}` cannot be variadic; only externs are allowed to be variadic",
          function.name
        )),
      );
    }

    if function.name == lowering::MAIN_FUNCTION_NAME {
      let main_function_type = ast::Type::Function(ast::FunctionType {
        parameter_types: vec![
          ast::Type::Basic(ast::BasicType::Int(ast::IntSize::I32)),
          ast::Type::Pointer(Box::new(ast::Type::Basic(ast::BasicType::String))),
        ],
        return_type: Box::new(ast::Type::Basic(ast::BasicType::Int(ast::IntSize::I32))),
        is_variadic: false,
        is_extern: false,
      });

      if function.infer_type(self.cache) != main_function_type {
        self.diagnostics.push(
          codespan_reporting::diagnostic::Diagnostic::error()
            .with_message("the `main` function has an invalid signature")
            .with_notes(vec![String::from("should accept a first parameter of type `Int`, a second one of type `*Str`, and the return type should be `Int`"), String::from("cannot be marked as variadic or extern")]),
        );
      }
    }

    let body = function.body;
    let return_type = TypeContext::infer_return_value_type(&body, self.cache);

    if !return_type.is_a_unit() {
      // If at least one statement's type evaluates to never, it
      // means that all paths are covered, because code execution will
      // always return at one point before reaching (or at) the end of the function.
      let all_paths_covered = body
        .statements
        .iter()
        .any(|statement| statement.infer_flatten_type(self.cache).is_a_never());

      if !all_paths_covered {
        self.diagnostics.push(
          codespan_reporting::diagnostic::Diagnostic::error()
            // TODO: Function name.
            .with_message("not all paths return a value"),
        );
      }
    }

    self.current_function_id = previous_function_key;
  }

  fn visit_return_stmt(&mut self, return_stmt: &ast::ReturnStmt) {
    let current_function_node = self.cache.force_get(&self.current_function_id.unwrap());
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
      self.cache,
    )
    .flatten(self.cache);

    // REVISE: Whether a function returns is already checked. Limit this to comparing the types only.
    if !return_type.is_a_unit() && return_stmt.value.is_none() {
      self.diagnostics.push(
        codespan_reporting::diagnostic::Diagnostic::error()
          .with_message("return statement must return a value"),
      );
    } else if return_type.is_a_unit() && return_stmt.value.is_some() {
      self.diagnostics.push(
        codespan_reporting::diagnostic::Diagnostic::error()
          .with_message("return statement must not return a value"),
      );

      // REVIEW: Returning at this point. Is this okay?
      return;
    }

    if let Some(value) = &return_stmt.value {
      let value_type = value.infer_flatten_type(self.cache);

      if !return_type.is(&value_type) {
        self.diagnostics.push(
          codespan_reporting::diagnostic::Diagnostic::error().with_message(format!(
            "return statement value and signature return type mismatch for {}",
            // REVISE: Change the actual name to this on its initialization.
            if let Some(name) = name {
              format!("function `{}`", name)
            } else {
              "closure".to_string()
            }
          )),
        );
      }
    }
  }

  fn visit_binding_stmt(&mut self, binding_stmt: &ast::BindingStmt) {
    let value_type = binding_stmt.value.infer_type(self.cache);
    let ty = binding_stmt.infer_type(self.cache);

    // FIXME: This is redundant. The same type is being compared!
    if !ty.flat_is(&value_type, self.cache) {
      self.diagnostics.push(
        codespan_reporting::diagnostic::Diagnostic::error().with_message(format!(
          "variable declaration of `{}` value and type mismatch",
          binding_stmt.name
        )),
      );
    }
  }

  fn visit_binary_expr(&mut self, binary_expr: &ast::BinaryExpr) {
    let left_type = binary_expr.left.infer_flatten_type(self.cache);
    let right_type = binary_expr.right.infer_flatten_type(self.cache);

    // TODO: Also add checks for when using operators with wrong values (ex. less-than or greater-than comparison of booleans).

    if !left_type.is(&right_type) {
      self.diagnostics.push(
        codespan_reporting::diagnostic::Diagnostic::error()
          .with_message("binary expression operands must be the same type"),
      );

      return;
    }

    // REVIEW: Check for mixed operators that don't make sense (ex. addition, then a comparison operator)?

    // NOTE: By this point, it is assumed that both operands are of the same type.
    match binary_expr.operator {
      ast::OperatorKind::Add
      | ast::OperatorKind::SubtractOrNegate
      | ast::OperatorKind::MultiplyOrDereference
      | ast::OperatorKind::Divide
      | ast::OperatorKind::LessThan
      | ast::OperatorKind::GreaterThan => {
        // REVIEW: What about floats?
        if !matches!(left_type, ast::Type::Basic(ast::BasicType::Int(_))) {
          self.diagnostics.push(
            codespan_reporting::diagnostic::Diagnostic::error()
              .with_message("binary expression operands must be both integers"),
          );
        }
      }
      // TODO: Equality operator, and others? Implement.
      _ => {}
    };
  }

  fn visit_if_expr(&mut self, if_expr: &ast::IfExpr) {
    let mut bounded_array_buffer = None;

    if_expr.condition.traverse(|node| {
      // if let ast::NodeKind::BinaryExpr(ast::BinaryExpr {
      //   left: _,
      //   right:
      //     ast::Node {kind: ast::NodeKind::IntrinsicCall(ast::IntrinsicCall {
      //       arguments,
      //       kind: ast::IntrinsicKind::LengthOf,
      //     })), cached_type: _},
      //   operator: ast::OperatorKind::In,
      // }) = &node.flatten()
      // {
      //   //
      // }

      if let ast::NodeKind::BinaryExpr(ast::BinaryExpr {
        left: _,
        right,
        operator: ast::OperatorKind::In,
      }) = &node.flatten()
      {
        if let ast::NodeKind::IntrinsicCall(ast::IntrinsicCall {
          arguments,
          kind: ast::IntrinsicKind::LengthOf,
        }) = &right.flatten()
        {
          if arguments.len() == 1 {
            let first_argument = arguments[0].flatten();

            if let ast::NodeKind::Reference(ast::Reference {
              pattern: ast::Pattern { id: target_id, .. },
              ..
            }) = &first_argument
            {
              if matches!(
                first_argument.infer_flatten_type(self.cache),
                ast::Type::Array(..)
              ) {
                bounded_array_buffer = Some(target_id);

                return true;
              }
            }
          }
        }
      }

      false
    });

    let condition_type = if_expr.condition.infer_flatten_type(self.cache);

    if !condition_type.is(&ast::Type::Basic(ast::BasicType::Bool)) {
      self.diagnostics.push(
        codespan_reporting::diagnostic::Diagnostic::error()
          .with_message("if statement condition must evaluate to a boolean"),
      );
    }

    // TODO: Check for type-mismatch between branches (they may yield).

    // TODO: Simplify.
    // If the condition provides bound checks for a static array,
    // mark the subject array as checked within the `then` expression
    // only.
    if let Some(bounded_array_id) = bounded_array_buffer {
      self
        .bound_checked_arrays
        .insert(bounded_array_id.to_owned());
    }

    // If an array bound check was provided, remove it after the `then`
    // expression has been checked.
    if let Some(bounded_array_id) = bounded_array_buffer {
      self.bound_checked_arrays.remove(bounded_array_id);
    }
  }

  fn visit_reference(&mut self, reference: &ast::Reference) {
    let target_type = self
      .cache
      .force_get(&reference.pattern.id)
      .infer_type(self.cache);

    // FIXME: Investigate how this affects.
    if target_type.is_a_meta() {
      self.diagnostics.push(
        codespan_reporting::diagnostic::Diagnostic::error()
          .with_message("cannot reference a binding that has a meta type; it cannot be evaluated"),
      );
    }
  }

  fn visit_extern_function(&mut self, extern_fn: &ast::ExternFunction) {
    if extern_fn.signature.accepts_instance {
      self.diagnostics.push(
        codespan_reporting::diagnostic::Diagnostic::error()
          .with_message("extern functions cannot accept instances"),
      );
    }
  }

  fn enter_unsafe_expr(&mut self, _unsafe_expr: &ast::UnsafeExpr) {
    // REVIEW: To avoid problems with nested cases, save a buffer here, then restore?
    // ... Maybe there's no need to restore the flag with its previous buffer in this specific case.
    self.in_unsafe_block = true;
  }

  fn exit_unsafe_expr(&mut self, _unsafe_expr: &ast::UnsafeExpr) {
    self.in_unsafe_block = false;
  }

  fn visit_static_array_value(&mut self, static_array_value: &ast::StaticArrayValue) {
    let mut mixed_elements_flag = false;

    let expected_element_type = if let Some(explicit_type) = &static_array_value.explicit_type {
      explicit_type.clone()
    } else {
      static_array_value
        .elements
        .first()
        .unwrap()
        .infer_type(self.cache)
    };

    // TODO: Skip the first element during iteration, as it is redundant.
    for element in &static_array_value.elements {
      // Report this error only once.
      // FIXME: Use type comparison function? And also flatten?
      if !mixed_elements_flag && element.infer_type(self.cache) != expected_element_type {
        self.diagnostics.push(
          codespan_reporting::diagnostic::Diagnostic::error()
            .with_message("array elements must all be of the same type"),
        );

        mixed_elements_flag = true;
      }
    }
  }

  fn visit_indexing_expr(&mut self, indexing_expr: &ast::IndexingExpr) {
    let index_expr_type = indexing_expr.index_expr.infer_flatten_type(self.cache);

    let is_index_proper_type =
      index_expr_type.is(&ast::Type::Basic(ast::BasicType::Int(ast::IntSize::U32)));

    if !is_index_proper_type {
      self.diagnostics.push(
        codespan_reporting::diagnostic::Diagnostic::error()
          .with_message("array index expression must be of type `U32`"),
      );

      // REVIEW: Should we actually not continue?
      // Can't continue if the index expression is not of the proper type.
      return;
    }

    let target_array = self.cache.force_get(&indexing_expr.target_id);
    let target_expr_type = target_array.infer_flatten_type(self.cache);

    // REVIEW: Any way of avoiding nesting?
    if let ast::Type::Array(_, length) = target_expr_type {
      // If the index expression is not a constant expression, then
      // this scope must fall under a bounds check for that index, and
      // the length of the array.
      if !indexing_expr.index_expr.is_constant_expr() {
        // TODO: Support for dynamic index, but require a bounds check.

        self.diagnostics.push(
          codespan_reporting::diagnostic::Diagnostic::error()
            .with_message("array index expression must be a constant expression"),
        );
      } else {
        // FIXME: Why we allow unary expressions on const expressions if we extract their value intact?
        let index_expr_literal = crate::force_match!(
          indexing_expr
            .index_expr
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
          self.diagnostics.push(
            codespan_reporting::diagnostic::Diagnostic::error()
              .with_message("array index expression must be within the bounds of the array"),
          );
        }
      }
    } else {
      self.diagnostics.push(
        codespan_reporting::diagnostic::Diagnostic::error()
          .with_message("can only index into arrays"),
      );
    }
  }

  fn visit_unary_expr(&mut self, unary_expr: &ast::UnaryExpr) {
    let expr_type = &unary_expr.expr.infer_flatten_type(self.cache);

    match unary_expr.operator {
      ast::OperatorKind::MultiplyOrDereference => {
        if !self.in_unsafe_block {
          self.diagnostics.push(
            codespan_reporting::diagnostic::Diagnostic::error()
              .with_message("can only dereference inside an unsafe block"),
          );
        }

        if !matches!(expr_type, ast::Type::Pointer(_)) {
          self.diagnostics.push(
            codespan_reporting::diagnostic::Diagnostic::error()
              .with_message("can only dereference pointers"),
          );
        }
      }
      ast::OperatorKind::Not => {
        if !expr_type.is(&ast::Type::Basic(ast::BasicType::Bool)) {
          self.diagnostics.push(
            codespan_reporting::diagnostic::Diagnostic::error()
              .with_message("can only negate boolean expressions"),
          );
        }
      }
      ast::OperatorKind::SubtractOrNegate => {
        // TODO: Include floats.
        if !matches!(expr_type, ast::Type::Basic(ast::BasicType::Int(_))) {
          // REVISE: Error message too similar to the boolean negation case.
          self.diagnostics.push(
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
          || !matches!(unary_expr.cast_type.as_ref().unwrap(), ast::Type::Basic(_))
        {
          self.diagnostics.push(
            codespan_reporting::diagnostic::Diagnostic::error()
              .with_message("can only cast between primitive types"),
          );
        } else if expr_type.is(unary_expr.cast_type.as_ref().unwrap()) {
          self.diagnostics.push(
            codespan_reporting::diagnostic::Diagnostic::warning()
              .with_message("redundant cast to the same type"),
          );
        }
      }
      _ => unreachable!(),
    };
  }

  fn visit_struct_value(&mut self, struct_value: &ast::StructValue) {
    let struct_type_node = self.cache.force_get(&struct_value.target_id);

    let struct_type = match struct_type_node {
      ast::NodeKind::Struct(struct_type) => struct_type,
      _ => unreachable!(),
    };

    if struct_value.fields.len() != struct_type.fields.len() {
      self.diagnostics.push(
        codespan_reporting::diagnostic::Diagnostic::error()
          .with_message("invalid amount of fields in struct value"),
      );

      return;
    }

    for (index, (value_field, struct_field)) in struct_value
      .fields
      .iter()
      .zip(struct_type.fields.iter())
      .enumerate()
    {
      let value_field_type = value_field.infer_type(self.cache);

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

  // #[test]
  // fn proper_initial_values() {
  //   let type_context = TypeContext::new();

  //   assert!(type_context.constraints.is_empty());
  //   assert!(type_context.substitutions.is_empty());
  //   assert!(type_context.diagnostics.is_empty());
  //   assert!(type_context.usings.is_empty());
  //   assert!(type_context.current_function_id.is_none());
  //   assert!(!type_context.in_impl);
  //   assert!(!type_context.in_unsafe_block);
  // }

  // #[test]
  // fn occurs_in() {
  //   let mut type_context = TypeContext::new();
  //   let first_index_id = 0;
  //   let second_index_id = first_index_id + 1;

  //   type_context
  //     .substitutions
  //     .insert(first_index_id, ast::Type::Variable(first_index_id));

  //   type_context
  //     .substitutions
  //     .insert(second_index_id, ast::Type::Unit);

  //   let subject_type_variable = ast::Type::Variable(first_index_id);

  //   assert!(type_context.occurs_in(first_index_id, &subject_type_variable));
  //   assert!(!type_context.occurs_in(second_index_id, &subject_type_variable));
  //   assert!(!type_context.occurs_in(first_index_id, &ast::Type::Unit));
  // }

  // #[test]
  // fn create_type_variable() {
  //   let mut type_context = TypeContext::new();

  //   assert_eq!(type_context.create_type_variable(), ast::Type::Variable(0));
  //   assert_eq!(1, type_context.substitutions.len());
  // }

  // #[test]
  // fn solve_constraints() {
  //   let mut type_context = TypeContext::new();

  //   // TODO: Add actual constraints to complete this test.

  //   type_context.solve_constraints();
  //   assert!(type_context.constraints.is_empty());
  // }

  // #[test]
  // fn substitute() {
  //   let mut type_context = TypeContext::new();

  //   assert_eq!(ast::Type::Unit, type_context.substitute(ast::Type::Unit));

  //   let type_variable_id = 0;
  //   let non_type_variable = ast::Type::Basic(ast::BasicType::Bool);

  //   type_context
  //     .substitutions
  //     .insert(type_variable_id, non_type_variable.clone());

  //   assert_eq!(
  //     non_type_variable,
  //     type_context.substitute(ast::Type::Variable(type_variable_id))
  //   );
  // }

  // #[test]
  // fn hindley_milner_type_inference() {
  //   let mut type_context = TypeContext::new();
  //   let cache = cache::Cache::new();
  //   let type_variable_id = 0;

  //   let mut binding_stmt = ast::BindingStmt {
  //     name: String::from("a"),
  //     ty: Some(ast::Type::Variable(type_variable_id)),
  //     // TODO: Use `Mock` scaffolding.
  //     value: Box::new(ast::Node {
  //       kind: ast::NodeKind::Literal(ast::Literal::Bool(true)),
  //       cached_type: None,
  //     }),
  //     id: 0,
  //     is_const_expr: false,
  //   };

  //   // TODO: Use the empty array type test.
  //   // TODO: Also, create a second test for inferring of parameter types.

  //   binding_stmt.report_constraints(&mut type_context, &cache);
  //   type_context.solve_constraints();
  //   binding_stmt.post_unification(&mut type_context, &cache);

  //   assert_eq!(
  //     binding_stmt.ty,
  //     Some(ast::Type::Basic(ast::BasicType::Bool))
  //   );
  // }

  // TODO: Add tests for `compare()`, `infer_and_flatten_type()`, `flatten_type()`, and others.
}
