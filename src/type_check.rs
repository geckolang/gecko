use crate::{ast, cache, diagnostic, dispatch, llvm_lowering};

pub struct TypeCheckContext {
  pub diagnostic_builder: diagnostic::DiagnosticBuilder,
  in_loop: bool,
  in_unsafe_block: bool,
  in_impl: bool,
  current_function_key: Option<cache::UniqueId>,
}

impl TypeCheckContext {
  pub fn new() -> Self {
    Self {
      diagnostic_builder: diagnostic::DiagnosticBuilder::new(),
      in_loop: false,
      in_unsafe_block: false,
      in_impl: false,
      current_function_key: None,
    }
  }

  // TODO: Find instances and replace old usages with this function.
  pub fn infer_and_resolve_type(node: &ast::Node, cache: &cache::Cache) -> ast::Type {
    TypeCheckContext::flatten_type(&node.infer_type(cache))
  }

  // TODO: Consider using `Result` instead of `Option`.
  pub fn unify_prototypes(
    prototype_a: &ast::Prototype,
    prototype_b: &ast::Prototype,
  ) -> Option<String> {
    if prototype_a.parameters.len() != prototype_b.parameters.len() {
      return Some("parameter count".to_string());
    }

    let parameter_types = prototype_a
      .parameters
      .iter()
      .zip(prototype_b.parameters.iter())
      .map(|(param_def_a, param_def_b)| (param_def_a.1.clone(), param_def_b.1.clone()));

    for (param_type_a, param_type_b) in parameter_types {
      if !Self::unify(&param_type_a, &param_type_b) {
        // TODO: Be more specific.
        return Some("parameter type".to_string());
      }
    }

    if !Self::unify(
      prototype_a.return_type.as_ref().unwrap(),
      prototype_b.return_type.as_ref().unwrap(),
    ) {
      return Some("return type".to_string());
    }

    None
  }

  // TODO: Create a `finalize` method to ensure that the main function was defined.

  // TODO: Consider making this function recursive (in the case that the user-defined type points to another user-defined type).
  /// Resolve a possible user-defined type, so it can be used properly.
  pub fn flatten_type(ty: &ast::Type) -> ast::Type {
    // TODO: Cleanup.

    // TODO: What if it's a pointer to a user-defined type?
    if let ast::Type::Stub(stub_type) = ty {
      // TODO: No need to clone.
      return stub_type.ty.as_ref().unwrap().as_ref().clone();
    } else if let ast::Type::This(this_type) = ty {
      // TODO: No need to clone.
      return this_type.ty.as_ref().unwrap().as_ref().clone();
    }

    // FIXME: Do not clone by default. Find a better alternative.
    ty.clone()
  }

  /// Compare two types for equality.
  ///
  /// The types passed-in will be resolved if needed before
  /// the comparison takes place.
  pub fn unify(type_a: &ast::Type, type_b: &ast::Type) -> bool {
    let resolved_type_a = Self::flatten_type(type_a);
    let resolved_type_b = Self::flatten_type(type_b);

    // The error type does not unify with anything.
    if matches!(resolved_type_a, ast::Type::Error) || matches!(resolved_type_b, ast::Type::Error) {
      return false;
    }
    // If both types are pointers, and at least one is a null pointer type, then always unify.
    // This is because null pointers unify with any pointer type (any pointer can be null).
    else if matches!(resolved_type_a, ast::Type::Pointer(_))
      && matches!(resolved_type_a, ast::Type::Pointer(_))
      && (Self::is_null_pointer_type(&resolved_type_a)
        || Self::is_null_pointer_type(&resolved_type_b))
    {
      return true;
    }

    resolved_type_a == resolved_type_b
  }

  fn is_null_pointer_type(ty: &ast::Type) -> bool {
    if let ast::Type::Pointer(ty) = ty {
      return matches!(ty.as_ref(), ast::Type::Primitive(ast::PrimitiveType::Null));
    }

    false
  }
}

pub trait TypeCheck {
  // TODO: Consider caching inference results here, if they are indeed costly.
  fn infer_type(&self, _cache: &cache::Cache) -> ast::Type {
    ast::Type::Unit
  }

  fn type_check(&self, _type_context: &mut TypeCheckContext, _cache: &cache::Cache) {
    //
  }
}

impl TypeCheck for ast::Node {
  fn type_check(&self, type_context: &mut TypeCheckContext, cache: &cache::Cache) {
    dispatch!(&self.kind, TypeCheck::type_check, type_context, cache);
  }

  fn infer_type(&self, cache: &cache::Cache) -> ast::Type {
    dispatch!(&self.kind, TypeCheck::infer_type, cache)
  }
}

impl TypeCheck for ast::Trait {
  //
}

impl TypeCheck for ast::StructImpl {
  fn type_check(&self, type_context: &mut TypeCheckContext, cache: &cache::Cache) {
    type_context.in_impl = true;

    for method in &self.methods {
      method.type_check(type_context, cache);
    }

    let target_node = cache.force_get(&self.target_struct_pattern.target_id.unwrap());

    // TODO: Cleanup.
    if let ast::NodeKind::StructType(_target_struct_type) = &target_node.kind {
      if let Some(trait_pattern) = &self.trait_pattern {
        let trait_node = cache.force_get(&trait_pattern.target_id.unwrap());

        if let ast::NodeKind::Trait(trait_type) = &trait_node.kind {
          for trait_method in &trait_type.methods {
            let impl_method_result = self
              .methods
              .iter()
              .find(|impl_method| impl_method.name == trait_method.0);

            if let Some(_impl_method) = impl_method_result {
              // TODO: Finish implementing.
              let prototype_unification_result =
                // TypeCheckContext::unify_prototypes(&trait_method.1, impl_method, cache);
                Some("pending error".to_string());

              if let Some(error) = prototype_unification_result {
                // TODO: Use expected/got system.
                type_context.diagnostic_builder.error(format!(
                  "prototype of implementation method `{}` for trait `{}` mismatch in {}",
                  "pending impl method name", trait_type.name, error
                ));
              }
            } else {
              type_context.diagnostic_builder.error(format!(
                "required method `{}` not implemented",
                trait_method.0
              ));
            }
          }
        } else {
          type_context.diagnostic_builder.error(format!(
            "cannot implement non-trait `{}`",
            &trait_pattern.base_name
          ));
        }
      }
    } else {
      type_context.diagnostic_builder.error(format!(
        "cannot implement for a non-struct type `{}`",
        self.target_struct_pattern.base_name
      ));
    }

    type_context.in_impl = false;
  }
}

impl TypeCheck for ast::MemberAccess {
  fn infer_type(&self, cache: &cache::Cache) -> ast::Type {
    let struct_type = match self.base_expr.infer_type(cache) {
      ast::Type::Struct(struct_type) => struct_type,
      // TODO: Investigate this strategy. Shouldn't we be using `unreachable!()` instead?
      _ => return ast::Type::Error,
    };

    let struct_field_result = struct_type.fields.iter().find(|x| x.0 == self.member_name);

    if let Some(struct_field) = struct_field_result {
      return struct_field.1.clone();
    }

    // TODO: Why not abstract this to the `Reference` node? We're doing the same thing (or very similar at least), correct?
    // TODO: Lookup implementation, and attempt to match a method.
    if let Some(struct_impls) = cache.get_struct_impls(&struct_type.unique_id) {
      for (method_unique_id, method_name) in struct_impls {
        if method_name == &self.member_name {
          return cache.force_get(&method_unique_id).infer_type(cache);
        }
      }
    }

    return ast::Type::Error;
  }

  fn type_check(&self, type_context: &mut TypeCheckContext, cache: &cache::Cache) {
    let struct_type = match self.base_expr.infer_type(cache) {
      ast::Type::Struct(struct_type) => struct_type,
      // TODO: Implement
      ast::Type::This(_) => return,
      // TODO: Investigate this strategy. Shouldn't we be using `unreachable!()` instead?
      _ => {
        type_context
          .diagnostic_builder
          .error("expression is not a struct".to_string());

        return;
      }
    };

    if !struct_type.fields.iter().any(|x| x.0 == self.member_name) {
      // type_context.diagnostic_builder.error(format!(
      //   "struct type `{}` does not contain a field named `{}`",
      //   struct_type.name, self.member_name
      // ));
    }
  }
}

impl TypeCheck for ast::Closure {
  fn infer_type(&self, cache: &cache::Cache) -> ast::Type {
    self.prototype.infer_type(cache)
  }

  fn type_check(&self, type_context: &mut TypeCheckContext, cache: &cache::Cache) {
    // TODO: Might need to mirror `Function`'s type check.

    if self.prototype.accepts_instance {
      type_context
        .diagnostic_builder
        .error("closures cannot accept instances".to_string());
    }

    self.prototype.type_check(type_context, cache);
    self.body.type_check(type_context, cache);
  }
}

impl TypeCheck for ast::TypeAlias {
  // TODO: Don't we need to implement `infer_type` here? Seems like not. Confirm.
}

impl TypeCheck for ast::Pattern {
  //
}

impl TypeCheck for ast::IntrinsicCall {
  // TODO: Implement.
}

impl TypeCheck for ast::ExternStatic {
  //
}

impl TypeCheck for ast::StructValue {
  fn infer_type(&self, cache: &cache::Cache) -> ast::Type {
    let struct_type_node = cache.force_get(&self.target_id.unwrap());

    let struct_type = match &(&*struct_type_node).kind {
      ast::NodeKind::StructType(struct_type) => struct_type,
      _ => unreachable!(),
    };

    // TODO: Is this the correct type? We might need this one in order to unify with the original struct type.
    ast::Type::Struct(struct_type.clone())
  }

  fn type_check(&self, type_context: &mut TypeCheckContext, cache: &cache::Cache) {
    let struct_type_node = cache.force_get(&self.target_id.unwrap());

    let struct_type = match &(&*struct_type_node).kind {
      ast::NodeKind::StructType(struct_type) => struct_type,
      _ => unreachable!(),
    };

    if self.fields.len() != struct_type.fields.len() {
      type_context
        .diagnostic_builder
        .error("invalid amount of fields in struct value".to_string());

      return;
    }

    // FIXME: Giving borrow errors.
    // for (index, (value_field, struct_field)) in self
    //   .fields
    //   .iter()
    //   .zip(struct_type.fields.iter())
    //   .enumerate()
    // {
    //   value_field.type_check(type_context, cache);

    //   let value_field_type = value_field.infer_type(cache).as_ref();

    //   if !unify_option(value_field_type, Some(struct_field.1), cache) {
    //     type_context.diagnostics.error(format!(
    //       "field and value at position `{}` type for struct `{}` mismatch",
    //       index, struct_type.name
    //     ));
    //   }
    // }
  }
}

impl TypeCheck for ast::Prototype {
  fn infer_type(&self, _cache: &cache::Cache) -> ast::Type {
    // TODO: Simplify.
    ast::Type::Callable(ast::CallableType {
      return_type: Box::new(self.return_type.as_ref().unwrap().clone()),
      parameter_types: self
        .parameters
        .iter()
        .map(|parameter| parameter.1.clone())
        .collect(),
      is_variadic: self.is_variadic,
    })
  }

  fn type_check(&self, _type_context: &mut TypeCheckContext, _cache: &cache::Cache) {
    // TODO: Implement?
  }
}

impl TypeCheck for ast::StructType {
  // TODO: Implement.
}

impl TypeCheck for ast::UnaryExpr {
  fn infer_type(&self, cache: &cache::Cache) -> ast::Type {
    let expr_type = self.expr.infer_type(cache);

    // Short-circuit if the expression's type is unit.
    if expr_type.is_unit() {
      return ast::Type::Unit;
    }

    return match self.operator {
      ast::OperatorKind::AddressOf => ast::Type::Pointer(Box::new(expr_type)),
      ast::OperatorKind::Cast => self.cast_type.as_ref().unwrap().clone(),
      ast::OperatorKind::Not => ast::Type::Primitive(ast::PrimitiveType::Bool),
      ast::OperatorKind::SubtractOrNegate => expr_type,
      _ => unreachable!(),
    };
  }

  fn type_check(&self, type_context: &mut TypeCheckContext, cache: &cache::Cache) {
    let expr_type = self.expr.infer_type(cache);

    match self.operator {
      ast::OperatorKind::MultiplyOrDereference => {
        if !type_context.in_unsafe_block {
          type_context
            .diagnostic_builder
            .error("can only dereference inside an unsafe block".to_string());
        }

        if !matches!(expr_type, ast::Type::Pointer(_)) {
          type_context
            .diagnostic_builder
            .error("can only dereference pointers".to_string());
        }
      }
      ast::OperatorKind::Not => {
        if !TypeCheckContext::unify(&expr_type, &ast::Type::Primitive(ast::PrimitiveType::Bool)) {
          type_context
            .diagnostic_builder
            .error("can only negate boolean expressions".to_string());
        }
      }
      ast::OperatorKind::SubtractOrNegate => {
        // TODO: Include floats.
        // FIXME: Shouldn't we be using `unify` here? What about types that need to be resolved? How do we pass-in a variant tho.? Or maybe the inferred type is already at its simplest form? Verify.
        if !matches!(expr_type, ast::Type::Primitive(ast::PrimitiveType::Int(_))) {
          // TODO: Error message too similar to the boolean negation case.
          type_context
            .diagnostic_builder
            .error("can only negate integer or float expressions".to_string());
        }
      }
      ast::OperatorKind::AddressOf => {
        // TODO: Implement.
        // todo!();
      }
      ast::OperatorKind::Cast => {
        // FIXME: What if it's an alias?
        if !matches!(expr_type, ast::Type::Primitive(_))
          || !matches!(self.cast_type.as_ref().unwrap(), ast::Type::Primitive(_))
        {
          type_context
            .diagnostic_builder
            .error("can only cast between primitive types".to_string());
        } else if TypeCheckContext::unify(&expr_type, self.cast_type.as_ref().unwrap()) {
          type_context
            .diagnostic_builder
            .warning("redundant cast to the same type".to_string());
        }
      }
      _ => unreachable!(),
    };
  }
}

impl TypeCheck for ast::Enum {
  //
}

impl TypeCheck for ast::AssignStmt {
  fn type_check(&self, type_context: &mut TypeCheckContext, cache: &cache::Cache) {
    // TODO: Need to unify the value and the target's type.

    let assignee_type = self.assignee_expr.infer_type(cache);

    if matches!(
      TypeCheckContext::flatten_type(&assignee_type),
      ast::Type::Reference(_)
    ) {
      type_context
        .diagnostic_builder
        .error("can't assign to a reference; references cannot be reseated".to_string());

      // TODO: We should continue gathering other diagnostics (ex. immutable)?
      return;
    }

    // NOTE: References cannot be reseated/assigned-to, only pointers.
    let is_pointer = matches!(assignee_type, ast::Type::Pointer(_));

    // FIXME: [!!] Revise: This checks are superficial. They do not
    // consider that expressions may be nested (ie. parentheses expr.).
    let is_array_indexing = matches!(self.assignee_expr.kind, ast::NodeKind::ArrayIndexing(_));
    let is_variable_ref = matches!(self.assignee_expr.kind, ast::NodeKind::Reference(_));

    // TODO: Missing member access (struct fields) support.
    // NOTE: The assignee expression may only be an expression of type `Pointer`
    // or `Reference`, a variable reference, or an array indexing.
    if !is_pointer && !is_variable_ref && !is_array_indexing {
      type_context
        .diagnostic_builder
        .error("assignee must be an expression of pointer or reference type, a variable reference, or an array indexing expression".to_string());
    } else if is_variable_ref {
      // If the assignee is a variable reference, ensure that the variable is mutable.
      match &self.assignee_expr.kind {
        ast::NodeKind::Reference(variable_ref) => {
          let declaration = cache.force_get(&variable_ref.0.target_id.unwrap());

          match &(&*declaration).kind {
            ast::NodeKind::LetStmt(let_stmt) if !let_stmt.is_mutable => {
              type_context
                .diagnostic_builder
                .error("assignee is immutable".to_string());
            }
            // TODO: Parameters should be immutable by default.
            _ => {}
          };
        }
        _ => unreachable!(),
      };
    }

    self.value.type_check(type_context, cache);
  }
}

impl TypeCheck for ast::ContinueStmt {
  fn type_check(&self, type_context: &mut TypeCheckContext, _cache: &cache::Cache) {
    if !type_context.in_loop {
      type_context
        .diagnostic_builder
        .error("continue statement may only occur inside loops".to_string());
    }
  }
}

impl TypeCheck for ast::ArrayIndexing {
  fn infer_type(&self, cache: &cache::Cache) -> ast::Type {
    let target_array_variable = &*cache.force_get(&self.target_id.unwrap());

    let array_type = match &target_array_variable.kind {
      ast::NodeKind::LetStmt(let_stmt) => let_stmt.ty.as_ref().unwrap(),
      ast::NodeKind::Parameter(parameter) => &parameter.1,
      _ => unreachable!(),
    };

    let array_element_type = match array_type {
      ast::Type::Array(element_type, _) => element_type.as_ref().clone(),
      _ => unreachable!(),
    };

    array_element_type
  }

  fn type_check(&self, type_context: &mut TypeCheckContext, cache: &cache::Cache) {
    let index_expr_type = self.index_expr.infer_type(cache);

    let is_unsigned_int_type =
      // TODO: Should we be using `unify` here, instead?
      if let ast::Type::Primitive(ast::PrimitiveType::Int(int_size)) = index_expr_type {
        matches!(int_size, ast::IntSize::U8)
          || matches!(int_size, ast::IntSize::U16)
          || matches!(int_size, ast::IntSize::U32)
          || matches!(int_size, ast::IntSize::U64)
      } else {
        false
      };

    if !is_unsigned_int_type {
      type_context
        .diagnostic_builder
        .error("array index expression must evaluate to an unsigned integer".to_string());
    }

    self.index_expr.type_check(type_context, cache);
  }
}

impl TypeCheck for ast::ArrayValue {
  fn infer_type(&self, cache: &cache::Cache) -> ast::Type {
    // TODO: Temporary, until type-inference is implemented.
    // We assume that the length is `0` if the explicit type is provided, otherwise
    // the array type is determined by the first element.
    let array_element_type = if let Some(explicit_type) = &self.explicit_type {
      explicit_type.clone()
    } else {
      self.elements.first().unwrap().infer_type(cache)
    };

    // TODO: Is the length conversion safe?
    ast::Type::Array(Box::new(array_element_type), self.elements.len() as u32)
  }

  fn type_check(&self, type_context: &mut TypeCheckContext, cache: &cache::Cache) {
    // FIXME: Here, we assume that `explicit_type` is always `Some(_)`. Currently, that might not be the case until type inference is implemented.
    let mut mixed_elements_flag = false;

    let expected_element_type = if let Some(explicit_type) = &self.explicit_type {
      explicit_type.clone()
    } else {
      self.elements.first().unwrap().infer_type(cache)
    };

    // TODO: Skip the first element during iteration, as it is redundant.
    for element in &self.elements {
      // Report this error only once.
      if !mixed_elements_flag && element.infer_type(cache) != expected_element_type {
        type_context
          .diagnostic_builder
          .error("array elements must all be of the same type".to_string());

        mixed_elements_flag = true;
      }

      element.type_check(type_context, cache);
    }
  }
}

impl TypeCheck for ast::UnsafeBlockStmt {
  fn infer_type(&self, cache: &cache::Cache) -> ast::Type {
    self.0.infer_type(cache)
  }

  fn type_check(&self, type_context: &mut TypeCheckContext, cache: &cache::Cache) {
    // TODO: To avoid problems with nested cases, save a buffer here, then restore?
    type_context.in_unsafe_block = true;
    self.0.type_check(type_context, cache);
    type_context.in_unsafe_block = false;
  }
}

impl TypeCheck for ast::ExternFunction {
  fn infer_type(&self, cache: &cache::Cache) -> ast::Type {
    self.prototype.infer_type(cache)
  }

  fn type_check(&self, type_context: &mut TypeCheckContext, _cache: &cache::Cache) {
    if self.prototype.accepts_instance {
      type_context
        .diagnostic_builder
        .error("extern functions cannot accept instances".to_string());
    }
  }
}

impl TypeCheck for ast::Parameter {
  fn infer_type(&self, _cache: &cache::Cache) -> ast::Type {
    self.1.clone()
  }
}

impl TypeCheck for ast::Block {
  fn infer_type(&self, cache: &cache::Cache) -> ast::Type {
    // If the last expression isn't yielded, then the block's type
    // defaults to unit. If there's no statements on the block, the
    // type of the block will logically be unit. Finally, if there is
    // at least a single return statement in the block, the block's type
    // will also be unit. Note that a return statement does not affect
    // the block's type, because the function is terminated, and not the
    // individual block. In other words, the block type is only determined
    // when an expression is yielded.
    if !self.yield_last_expr
      || self.statements.is_empty()
      || self
        .statements
        .iter()
        .any(|x| matches!(x.kind, ast::NodeKind::ReturnStmt(_)))
    {
      return ast::Type::Unit;
    }

    self.statements.last().unwrap().infer_type(cache)
  }

  fn type_check(&self, type_context: &mut TypeCheckContext, cache: &cache::Cache) {
    for statement in &self.statements {
      statement.type_check(type_context, cache);
    }
  }
}

impl TypeCheck for ast::Reference {
  fn infer_type(&self, cache: &cache::Cache) -> ast::Type {
    (&*cache)
      .force_get(&self.0.target_id.unwrap())
      .infer_type(cache)
  }
}

impl TypeCheck for ast::Literal {
  fn infer_type(&self, _cache: &cache::Cache) -> ast::Type {
    ast::Type::Primitive(match self {
      ast::Literal::Bool(_) => ast::PrimitiveType::Bool,
      ast::Literal::Char(_) => ast::PrimitiveType::Char,
      ast::Literal::Int(_, size) => ast::PrimitiveType::Int(size.clone()),
      ast::Literal::String(_) => ast::PrimitiveType::String,
      ast::Literal::Nullptr => {
        return ast::Type::Pointer(Box::new(ast::Type::Primitive(ast::PrimitiveType::Null)))
      }
    })
  }
}

impl TypeCheck for ast::IfStmt {
  fn infer_type(&self, cache: &cache::Cache) -> ast::Type {
    // Both branches must be present in order for a value
    // to possibly evaluate.
    if self.else_block.is_none() {
      return ast::Type::Unit;
    }

    let else_block = self.else_block.as_ref().unwrap();
    let then_block_type = self.then_block.infer_type(cache);

    // FIXME: Perhaps make a special case for let-statement? Its type inference is used internally, but they should yield 'Unit' for the user.
    // In case of a type-mismatch between branches, simply return the unit type.
    if !TypeCheckContext::unify(&then_block_type, &else_block.infer_type(cache)) {
      return ast::Type::Unit;
    }

    then_block_type
  }

  fn type_check(&self, type_context: &mut TypeCheckContext, cache: &cache::Cache) {
    if !TypeCheckContext::unify(
      &self.condition.infer_type(cache),
      &ast::Type::Primitive(ast::PrimitiveType::Bool),
    ) {
      type_context
        .diagnostic_builder
        .error("if statement condition must evaluate to a boolean".to_string());
    }

    self.condition.type_check(type_context, cache);
    self.then_block.type_check(type_context, cache);

    if let Some(else_block) = &self.else_block {
      else_block.type_check(type_context, cache);
    }
  }
}

impl TypeCheck for ast::BinaryExpr {
  fn infer_type(&self, cache: &cache::Cache) -> ast::Type {
    match self.operator {
      ast::OperatorKind::LessThan
      | ast::OperatorKind::GreaterThan
      | ast::OperatorKind::Equality
      | ast::OperatorKind::And
      | ast::OperatorKind::Or
      | ast::OperatorKind::Nand
      | ast::OperatorKind::Nor
      | ast::OperatorKind::Xor => ast::Type::Primitive(ast::PrimitiveType::Bool),
      _ => self.left.infer_type(cache),
    }
  }

  fn type_check(&self, type_context: &mut TypeCheckContext, cache: &cache::Cache) {
    let left_type = self.left.infer_type(cache);
    let right_type = self.right.infer_type(cache);

    // TODO: Also add checks for when using operators with wrong values (ex. less-than or greater-than comparison of booleans).

    // TODO: If we require both operands to  be of the same type, then operator overloading isn't possible with mixed operands as parameters.
    if !TypeCheckContext::unify(&left_type, &right_type) {
      type_context
        .diagnostic_builder
        .error("binary expression operands must be the same type".to_string());

      return;
    }

    // TODO: Check for mixed operators that don't make sense (ex. addition, then a comparison operator)?

    // NOTE: By this point, it is assumed that both operands are of the same type.
    match self.operator {
      ast::OperatorKind::Add
      | ast::OperatorKind::SubtractOrNegate
      | ast::OperatorKind::MultiplyOrDereference
      | ast::OperatorKind::Divide
      | ast::OperatorKind::LessThan
      | ast::OperatorKind::GreaterThan => {
        // TODO: What about floats?
        if !matches!(left_type, ast::Type::Primitive(ast::PrimitiveType::Int(_))) {
          type_context
            .diagnostic_builder
            .error("binary expression operands must be both integers".to_string());
        }
      }
      // TODO: Equality operator, and others? Implement.
      _ => {}
    };

    self.left.type_check(type_context, cache);
    self.right.type_check(type_context, cache);
  }
}

impl TypeCheck for ast::BreakStmt {
  fn type_check(&self, type_context: &mut TypeCheckContext, _cache: &cache::Cache) {
    if !type_context.in_loop {
      type_context
        .diagnostic_builder
        .error("break statement may only occur inside loops".to_string());
    }
  }
}

impl TypeCheck for ast::Definition {
  fn infer_type(&self, cache: &cache::Cache) -> ast::Type {
    self.node_ref_cell.borrow().infer_type(cache)
  }

  fn type_check(&self, type_context: &mut TypeCheckContext, cache: &cache::Cache) {
    let node = self.node_ref_cell.borrow();

    if matches!(
      (&*node).kind,
      ast::NodeKind::Function(_) | ast::NodeKind::Closure(_)
    ) {
      type_context.current_function_key = Some(self.unique_id);
    }

    node.type_check(type_context, cache);
  }
}

impl TypeCheck for ast::InlineExprStmt {
  fn infer_type(&self, cache: &cache::Cache) -> ast::Type {
    self.expr.infer_type(cache)
  }

  fn type_check(&self, type_context: &mut TypeCheckContext, cache: &cache::Cache) {
    self.expr.type_check(type_context, cache);
  }
}

impl TypeCheck for ast::LetStmt {
  // FIXME: [!] This causes a bug where the string literal is not accessed (left as `i8**`). The let-statement didn't have a type before.
  fn infer_type(&self, cache: &cache::Cache) -> ast::Type {
    self.value.infer_type(&cache)
  }

  fn type_check(&self, type_context: &mut TypeCheckContext, cache: &cache::Cache) {
    let value_type = self.value.infer_type(cache);

    if !TypeCheckContext::unify(self.ty.as_ref().unwrap(), &value_type) {
      type_context.diagnostic_builder.error(format!(
        "variable declaration of `{}` value and type mismatch",
        self.name
      ));
    }

    self.value.type_check(type_context, cache);
  }
}

impl TypeCheck for ast::ReturnStmt {
  fn type_check(&self, type_context: &mut TypeCheckContext, cache: &cache::Cache) {
    let current_function_node = cache
      .declarations
      .get(&type_context.current_function_key.unwrap())
      .unwrap()
      .borrow();

    let mut name = None;
    let prototype;

    match &(&*current_function_node).kind {
      ast::NodeKind::Function(function) => {
        name = Some(function.name.clone());
        prototype = &function.prototype;
      }
      ast::NodeKind::Closure(closure) => {
        prototype = &closure.prototype;
      }
      _ => unreachable!(),
    };

    let return_type = prototype.return_type.as_ref().unwrap();

    // TODO: Whether a function returns is already checked. Limit this to unifying the types only.
    if !return_type.is_unit() && self.value.is_none() {
      type_context
        .diagnostic_builder
        .error("return statement must return a value".to_string());
    } else if return_type.is_unit() && self.value.is_some() {
      type_context
        .diagnostic_builder
        .error("return statement must not return a value".to_string());

      // TODO: Returning at this point. Is this okay?
      return;
    }

    if let Some(value) = &self.value {
      let value_type = value.infer_type(cache);

      if !TypeCheckContext::unify(return_type, &value_type) {
        type_context.diagnostic_builder.error(format!(
          "return statement value and prototype return type mismatch for {}",
          if let Some(name) = name {
            format!("function `{}`", name)
          } else {
            "closure".to_string()
          }
        ));
      }

      value.type_check(type_context, cache);
    }
  }
}

impl TypeCheck for ast::Function {
  fn infer_type(&self, cache: &cache::Cache) -> ast::Type {
    self.prototype.infer_type(cache)
  }

  fn type_check(&self, type_context: &mut TypeCheckContext, cache: &cache::Cache) {
    if self.prototype.accepts_instance && !type_context.in_impl {
      type_context
        .diagnostic_builder
        .error("cannot accept instance in a non-impl function".to_string());
    }

    let return_type = self.prototype.return_type.as_ref().unwrap();

    // TODO: Special case for the `main` function. Unify expected signature.
    // If applicable, the function's body must return a value.
    if !return_type.is_unit()
      && !self.body.yield_last_expr
      && !self
        .body
        .statements
        .iter()
        .any(|x| matches!(x.kind, ast::NodeKind::ReturnStmt(_)))
    {
      type_context.diagnostic_builder.error(format!(
        "the body of function `{}` must return a value",
        self.name
      ));
    }

    if self.prototype.is_variadic {
      type_context.diagnostic_builder.error(format!(
        "function `{}` cannot be variadic; only externs are allowed to be variadic",
        self.name
      ));
    }

    if self.body.yield_last_expr
      && !TypeCheckContext::unify(return_type, &self.body.infer_type(cache))
    {
      // TODO: Improve error message.
      type_context.diagnostic_builder.error(format!(
        "function body of `{}` yielded value type-mismatch",
        self.name
      ));
    }

    if self.name == llvm_lowering::MAIN_FUNCTION_NAME {
      let main_prototype = ast::Prototype {
        // TODO: Parameters. Also, the comparison should ignore parameter names.
        parameters: vec![],
        return_type: Some(ast::Type::Primitive(ast::PrimitiveType::Int(
          ast::IntSize::I32,
        ))),
        is_variadic: false,
        accepts_instance: false,
        instance_type_id: None,
        this_parameter: None,
      };

      // TODO: Simplify.
      if self.prototype.infer_type(cache) != main_prototype.infer_type(cache) {
        type_context
          .diagnostic_builder
          .error(format!("the `main` function has an invalid signature"));
      }
    }

    self.prototype.type_check(type_context, cache);
    self.body.type_check(type_context, cache);
  }
}

impl TypeCheck for ast::CallExpr {
  fn infer_type(&self, cache: &cache::Cache) -> ast::Type {
    let callee_expr_type = self.callee_expr.infer_type(cache);

    match callee_expr_type {
      ast::Type::Callable(callable_type) => callable_type.return_type.as_ref().clone(),
      _ => ast::Type::Error,
    }
  }

  fn type_check(&self, type_context: &mut TypeCheckContext, cache: &cache::Cache) {
    self.callee_expr.type_check(type_context, cache);

    // TODO: Consider adopting a `expected` and `actual` API for diagnostics, when applicable.
    // TODO: Need access to the current function?

    let callee_expr_type = self.callee_expr.infer_type(cache);

    if !matches!(callee_expr_type, ast::Type::Callable(_)) {
      type_context
        .diagnostic_builder
        .error("call expression's callee is not actually callable".to_string());

      // Cannot continue.
      return;
    }

    let callee_type = match callee_expr_type {
      ast::Type::Callable(callable_type) => callable_type,
      _ => unreachable!(),
    };

    // TODO: Better, simpler way of doing this?
    // let attributes;

    // TODO: Need names.
    // match callee_type {
    //   ast::NodeKind::ExternFunction(extern_) => {
    //     if !type_context.in_unsafe_block {
    //       type_context.diagnostic_builder.error(format!(
    //         "extern function call to `{}` may only occur inside an unsafe block",
    //         extern_.name
    //       ));
    //     }

    //     name = &extern_.name;
    //     prototype = &extern_.prototype;
    //     attributes = &extern_.attributes;
    //   }
    //   ast::NodeKind::Function(function) => {
    //     name = &function.name;
    //     prototype = &function.prototype;
    //     attributes = &function.attributes;
    //   }
    //   _ => unreachable!(),
    // };

    // for attribute in attributes {
    //   // TODO: Keep it simple for now, but later, we can improve the attribute system.
    //   match attribute.name.as_str() {
    //     "deprecated" => type_context
    //       .diagnostic_builder
    //       .warning(format!("function `{}` is deprecated", name)),
    //     _ => type_context.diagnostic_builder.warning(format!(
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
      type_context
        .diagnostic_builder
        .error("call expression has an invalid amount of arguments".to_string());
    }

    // FIXME: Straight up broken. Need to re-verify and fix.
    // FIXME: Different amount of arguments and parameters (due to variadic parameters) may affect this.
    // Unify argument and parameter types.
    // for (parameter, argument) in prototype.parameters.iter().zip(self.arguments.iter()) {
    //   let parameter_type = parameter.infer_type(cache);
    //   let argument_type = argument.infer_type(cache);

    //   if !TypeCheckContext::unify_option(parameter_type.as_ref(), argument_type.as_ref(), cache) {
    //     // TODO: Include callee name in the error message.
    //     type_context.diagnostics.error(format!(
    //       "function call argument and parameter `{}` type mismatch",
    //       parameter.0
    //     ));
    //   }
    // }
  }
}

impl TypeCheck for ast::LoopStmt {
  fn type_check(&self, type_context: &mut TypeCheckContext, cache: &cache::Cache) {
    if let Some(condition) = &self.condition {
      if !TypeCheckContext::unify(
        &condition.infer_type(cache),
        &ast::Type::Primitive(ast::PrimitiveType::Bool),
      ) {
        type_context
          .diagnostic_builder
          .error("loop condition must evaluate to a boolean".to_string());
      }

      condition.type_check(type_context, cache);
    }

    // TODO: To avoid problems with nested cases, save a buffer here, then restore?
    type_context.in_loop = true;
    self.body.type_check(type_context, cache);
    type_context.in_loop = false;
  }
}
