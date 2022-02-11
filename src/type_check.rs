use crate::{ast, cache, diagnostic, dispatch};

pub struct TypeCheckContext {
  pub diagnostic_builder: diagnostic::DiagnosticBuilder,
  in_loop: bool,
  in_unsafe_block: bool,
  current_function_key: Option<cache::DefinitionKey>,
}

impl TypeCheckContext {
  pub fn new() -> Self {
    Self {
      diagnostic_builder: diagnostic::DiagnosticBuilder::new(),
      in_loop: false,
      in_unsafe_block: false,
      current_function_key: None,
    }
  }

  // TODO: Create a `finalize` method to ensure that the main function was defined.

  // TODO: Consider making this function recursive (in the case that the user-defined type points to another user-defined type).
  /// Resolve a possible user-defined type, so it can be used properly.
  pub fn resolve_type(ty: &ast::Type, cache: &cache::Cache) -> ast::Type {
    // TODO: What if it's a pointer to a user-defined type?
    if let ast::Type::Stub(stub_type) = ty {
      let target_type = cache.get(&stub_type.target_key.unwrap());

      return match &*target_type {
        // TODO: Cloning struct type.
        ast::NodeKind::StructType(struct_type) => ast::Type::Struct(struct_type.clone()),
        ast::NodeKind::TypeAlias(type_alias) => type_alias.ty.clone(),
        _ => unreachable!(),
      };
    }

    // FIXME: Do not clone. Find a better alternative.
    ty.clone()
  }

  /// Compare two types for equality.
  ///
  /// The types passed-in will be resolved if needed before
  /// the comparison takes place.
  pub fn unify(type_a: &ast::Type, type_b: &ast::Type, cache: &cache::Cache) -> bool {
    let resolved_type_a = Self::resolve_type(type_a, cache);
    let resolved_type_b = Self::resolve_type(type_b, cache);

    // If both types are pointers, and at least one is a null pointer type, then always unify.
    // This is because null pointers unify with any pointer type (any pointer can be null).
    if matches!(resolved_type_a, ast::Type::Pointer(_))
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
  fn infer_type(&self, _cache: &cache::Cache) -> ast::Type {
    ast::Type::Unit
  }

  fn type_check(&self, _type_context: &mut TypeCheckContext, _cache: &cache::Cache) {
    //
  }
}

impl TypeCheck for ast::NodeKind {
  fn type_check(&self, type_context: &mut TypeCheckContext, cache: &cache::Cache) {
    dispatch!(self, TypeCheck::type_check, type_context, cache);
  }

  fn infer_type(&self, cache: &cache::Cache) -> ast::Type {
    dispatch!(self, TypeCheck::infer_type, cache)
  }
}

impl TypeCheck for ast::Closure {
  fn infer_type(&self, _cache: &cache::Cache) -> ast::Type {
    let parameters = self
      .prototype
      .parameters
      .iter()
      .map(|x| x.1.clone())
      .collect::<Vec<_>>();

    let return_type = self.prototype.return_type.clone();

    ast::Type::Callable(ast::CallableType {
      parameters,
      return_type: Box::new(return_type),
      is_variadic: false,
    })
  }

  fn type_check(&self, type_context: &mut TypeCheckContext, cache: &cache::Cache) {
    // TODO: Might need to mirror `Function`'s type check.

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
    let struct_type_node = cache.get(&self.target_key.unwrap());

    let struct_type = match &*struct_type_node {
      ast::NodeKind::StructType(struct_type) => struct_type,
      _ => unreachable!(),
    };

    // TODO: Is this the correct type? We might need this one in order to unify with the original struct type.
    ast::Type::Struct(struct_type.clone())
  }

  fn type_check(&self, type_context: &mut TypeCheckContext, cache: &cache::Cache) {
    let struct_type_node = cache.get(&self.target_key.unwrap());

    let struct_type = match &*struct_type_node {
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
    ast::Type::Callable(ast::CallableType {
      return_type: Box::new(self.return_type.clone()),
      parameters: self
        .parameters
        .iter()
        .cloned()
        .map(|x| x.1.clone())
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
    let expr_type = self.expr.kind.infer_type(cache);

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
    let expr_type = self.expr.kind.infer_type(cache);

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
        if !TypeCheckContext::unify(
          &expr_type,
          &ast::Type::Primitive(ast::PrimitiveType::Bool),
          cache,
        ) {
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
            .error("can only negate integers or float expressions".to_string());
        }
      }
      ast::OperatorKind::AddressOf => {
        // TODO: Implement.
        // todo!();
      }
      ast::OperatorKind::Cast => {
        if !matches!(expr_type, ast::Type::Primitive(_))
          || !matches!(self.cast_type.as_ref().unwrap(), ast::Type::Primitive(_))
        {
          type_context
            .diagnostic_builder
            .error("can only cast between primitive types".to_string());
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
    // TODO: Need to unify the value and the target's type, as well as ensuring that the target is mutable.

    let assignee_type = self.assignee_expr.kind.infer_type(cache);
    let is_pointer_or_ref_expr = matches!(assignee_type, ast::Type::Pointer(_));
    let is_array_indexing = matches!(self.assignee_expr.kind, ast::NodeKind::ArrayIndexing(_));

    let is_variable_ref = matches!(
      self.assignee_expr.kind,
      ast::NodeKind::VariableOrMemberRef(_)
    );

    // TODO: Missing member access (struct fields) support.
    // NOTE: The assignee expression may only be an expression of type `Pointer`
    // or `Reference`, a variable reference, or an array indexing.
    if !is_pointer_or_ref_expr && !is_variable_ref && !is_array_indexing {
      type_context
        .diagnostic_builder
        .error("assignee must be an expression of pointer or reference type, a variable reference, or an array indexing".to_string());
    } else if is_variable_ref {
      // If the assignee is a variable reference, ensure that the variable is mutable.
      match &self.assignee_expr.kind {
        ast::NodeKind::VariableOrMemberRef(variable_ref) => {
          let declaration = cache.get(&variable_ref.0.target_key.unwrap());

          match &*declaration {
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

    self.value.kind.type_check(type_context, cache);
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
    let target_array_variable = &*cache.get(&self.target_key.unwrap());

    let array_type = match target_array_variable {
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

  fn type_check(&self, _type_context: &mut TypeCheckContext, _cache: &cache::Cache) {
    // TODO: Implement.
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
      self.elements.first().unwrap().kind.infer_type(cache)
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
      self.elements.first().unwrap().kind.infer_type(cache)
    };

    // TODO: Skip the first element during iteration, as it is redundant.
    for element in &self.elements {
      // Report this error only once.
      if !mixed_elements_flag && element.kind.infer_type(cache) != expected_element_type {
        type_context
          .diagnostic_builder
          .error("array elements must all be of the same type".to_string());

        mixed_elements_flag = true;
      }

      element.kind.type_check(type_context, cache);
    }
  }
}

impl TypeCheck for ast::UnsafeBlockStmt {
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
}

impl TypeCheck for ast::Parameter {
  fn infer_type(&self, _cache: &cache::Cache) -> ast::Type {
    self.1.clone()
  }
}

impl TypeCheck for ast::Block {
  fn infer_type(&self, cache: &cache::Cache) -> ast::Type {
    // If the last expression is not to be yielded, there are
    // no statements, or there is a return statement present,
    // the block will not yield a value.
    if !self.yield_last_expr
      || self.statements.is_empty()
      || self
        .statements
        .iter()
        .any(|x| matches!(x.kind, ast::NodeKind::ReturnStmt(_)))
    {
      return ast::Type::Unit;
    }

    return self.statements.last().unwrap().kind.infer_type(cache);
  }

  fn type_check(&self, type_context: &mut TypeCheckContext, cache: &cache::Cache) {
    for statement in &self.statements {
      statement.kind.type_check(type_context, cache);
    }
  }
}

impl TypeCheck for ast::VariableOrMemberRef {
  fn infer_type(&self, cache: &cache::Cache) -> ast::Type {
    (&*cache).get(&self.0.target_key.unwrap()).infer_type(cache)
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

    // In case of a type-mismatch between branches, simply return the unit type.
    if !TypeCheckContext::unify(&then_block_type, &else_block.infer_type(cache), cache) {
      return ast::Type::Unit;
    }

    then_block_type
  }

  fn type_check(&self, type_context: &mut TypeCheckContext, cache: &cache::Cache) {
    if !TypeCheckContext::unify(
      &self.condition.kind.infer_type(cache),
      &ast::Type::Primitive(ast::PrimitiveType::Bool),
      cache,
    ) {
      type_context
        .diagnostic_builder
        .error("if statement condition must evaluate to a boolean".to_string());
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
      _ => self.left.kind.infer_type(cache),
    }
  }

  fn type_check(&self, type_context: &mut TypeCheckContext, cache: &cache::Cache) {
    let left_type = self.left.kind.infer_type(cache);
    let right_type = self.right.kind.infer_type(cache);

    // TODO: Also add checks for when using operators with wrong values (ex. less-than or greater-than comparison of booleans).

    // TODO: If we require both operands to  be of the same type, then operator overloading isn't possible with mixed operands as parameters.
    if !TypeCheckContext::unify(&left_type, &right_type, cache) {
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

    self.left.kind.type_check(type_context, cache);
    self.right.kind.type_check(type_context, cache);
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

    if let ast::NodeKind::Function(_) = &*node {
      type_context.current_function_key = Some(self.definition_key);
    }

    node.type_check(type_context, cache);
  }
}

impl TypeCheck for ast::InlineExprStmt {
  fn infer_type(&self, cache: &cache::Cache) -> ast::Type {
    self.expr.kind.infer_type(cache)
  }

  fn type_check(&self, type_context: &mut TypeCheckContext, cache: &cache::Cache) {
    self.expr.kind.type_check(type_context, cache);
  }
}

impl TypeCheck for ast::LetStmt {
  // FIXME: [!] This causes a bug where the string literal is not accessed (left as `i8**`). The let-statement didn't have a type before.
  fn infer_type(&self, cache: &cache::Cache) -> ast::Type {
    self.value.kind.infer_type(&cache)
  }

  fn type_check(&self, type_context: &mut TypeCheckContext, cache: &cache::Cache) {
    let value_type = self.value.kind.infer_type(cache);

    if !TypeCheckContext::unify(self.ty.as_ref().unwrap(), &value_type, cache) {
      type_context.diagnostic_builder.error(format!(
        "variable declaration of `{}` value and type mismatch",
        self.name
      ));
    }

    self.value.kind.type_check(type_context, cache);
  }
}

impl TypeCheck for ast::ReturnStmt {
  fn type_check(&self, type_context: &mut TypeCheckContext, cache: &cache::Cache) {
    let current_function_node = cache
      .declarations
      .get(&type_context.current_function_key.unwrap())
      .unwrap()
      .borrow();

    let current_function = match &*current_function_node {
      ast::NodeKind::Function(function) => function,
      _ => unreachable!(),
    };

    // TODO: Whether a function returns is already checked. Limit this to unifying the types only.
    if !current_function.prototype.return_type.is_unit() && self.value.is_none() {
      type_context
        .diagnostic_builder
        .error("return statement must return a value".to_string());
    } else if current_function.prototype.return_type.is_unit() && self.value.is_some() {
      type_context
        .diagnostic_builder
        .error("return statement must not return a value".to_string());

      // TODO: Returning at this point. Is this okay?
      return;
    }

    if let Some(value) = &self.value {
      let value_type = value.kind.infer_type(cache);

      if !TypeCheckContext::unify(&current_function.prototype.return_type, &value_type, cache) {
        type_context.diagnostic_builder.error(format!(
          "return statement value and function return type mismatch for function `{}`",
          current_function.name
        ));
      }

      value.kind.type_check(type_context, cache);
    }
  }
}

impl TypeCheck for ast::Function {
  fn infer_type(&self, cache: &cache::Cache) -> ast::Type {
    self.prototype.infer_type(cache)
  }

  fn type_check(&self, type_context: &mut TypeCheckContext, cache: &cache::Cache) {
    // TODO: Special case for the `main` function. Unify expected signature.

    // If applicable, the function's body must return a value.
    if !self.prototype.return_type.is_unit()
      && !self.body.yield_last_expr
        & !self
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

    self.prototype.type_check(type_context, cache);
    self.body.type_check(type_context, cache);
  }
}

impl TypeCheck for ast::CallExpr {
  fn infer_type(&self, cache: &cache::Cache) -> ast::Type {
    let callee_expr_type = self.callee_expr.kind.infer_type(cache);

    match callee_expr_type {
      ast::Type::Callable(callable_type) => callable_type.return_type.as_ref().clone(),
      _ => unreachable!(),
    }
  }

  fn type_check(&self, type_context: &mut TypeCheckContext, cache: &cache::Cache) {
    // TODO: Consider adopting a `expected` and `actual` API for diagnostics, when applicable.
    // TODO: Need access to the current function.
    // TODO: Ensure externs and unsafe function are only called from unsafe functions.

    let callee_type = match self.callee_expr.kind.infer_type(cache) {
      ast::Type::Callable(callable_type) => callable_type,
      _ => unreachable!(),
    };

    // TODO: Better, simpler way of doing this?
    let name = "pending";
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

    let min_arg_count = callee_type.parameters.len();
    let actual_arg_count = self.arguments.len();

    // Verify argument count.
    if (!callee_type.is_variadic && actual_arg_count != min_arg_count)
      || (callee_type.is_variadic && actual_arg_count < min_arg_count)
    {
      type_context.diagnostic_builder.error(format!(
        "function call to `{}` has an invalid amount of arguments",
        name
      ));
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
        &condition.kind.infer_type(cache),
        &ast::Type::Primitive(ast::PrimitiveType::Bool),
        cache,
      ) {
        type_context
          .diagnostic_builder
          .error("loop condition must evaluate to a boolean".to_string());
      }

      condition.kind.type_check(type_context, cache);
    }

    // TODO: To avoid problems with nested cases, save a buffer here, then restore?
    type_context.in_loop = true;
    self.body.type_check(type_context, cache);
    type_context.in_loop = false;
  }
}
