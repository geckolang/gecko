use crate::{ast, cache, diagnostic, dispatch, llvm_lowering};

pub struct SemanticCheckContext {
  pub diagnostic_builder: diagnostic::DiagnosticBuilder,
  in_unsafe_block: bool,
  current_function_key: Option<cache::UniqueId>,
}

impl SemanticCheckContext {
  pub fn new() -> Self {
    Self {
      diagnostic_builder: diagnostic::DiagnosticBuilder::new(),
      in_unsafe_block: false,
      current_function_key: None,
    }
  }

  // TODO: Find instances and replace old usages with this function.
  pub fn infer_and_resolve_type(node: &ast::Node, cache: &cache::Cache) -> ast::Type {
    SemanticCheckContext::flatten_type(&node.infer_type(cache))
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
      .map(|(param_def_a, param_def_b)| (param_def_a.ty.clone(), param_def_b.ty.clone()));

    for (param_type_a, param_type_b) in parameter_types {
      if !Self::unify(&param_type_a, &param_type_b) {
        // TODO: Be more specific.
        return Some("parameter type".to_string());
      }
    }

    if !Self::unify(&prototype_a.return_type, &prototype_b.return_type) {
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
      return matches!(ty.as_ref(), ast::Type::Primitive(ast::BasicType::Null));
    }

    false
  }
}

pub trait SemanticCheck {
  // TODO: Consider caching inference results here, if they are indeed costly.
  fn infer_type(&self, _cache: &cache::Cache) -> ast::Type {
    ast::Type::Unit
  }

  fn check(&self, _context: &mut SemanticCheckContext, _cache: &cache::Cache) {
    //
  }
}

impl SemanticCheck for ast::Node {
  fn check(&self, context: &mut SemanticCheckContext, cache: &cache::Cache) {
    dispatch!(&self.kind, SemanticCheck::check, context, cache);
  }

  fn infer_type(&self, cache: &cache::Cache) -> ast::Type {
    dispatch!(&self.kind, SemanticCheck::infer_type, cache)
  }
}

impl SemanticCheck for ast::ParenthesesExpr {
  fn infer_type(&self, cache: &cache::Cache) -> ast::Type {
    self.0.infer_type(cache)
  }

  fn check(&self, context: &mut SemanticCheckContext, cache: &cache::Cache) {
    self.0.check(context, cache);
  }
}

impl SemanticCheck for ast::Closure {
  fn infer_type(&self, cache: &cache::Cache) -> ast::Type {
    self.prototype.infer_type(cache)
  }

  fn check(&self, context: &mut SemanticCheckContext, cache: &cache::Cache) {
    // TODO: Might need to mirror `Function`'s type check.

    self.prototype.check(context, cache);
    self.body_value.check(context, cache);
  }
}

impl SemanticCheck for ast::TypeDef {
  // TODO: Don't we need to implement `infer_type` here? Seems like not. Confirm.
}

impl SemanticCheck for ast::Pattern {
  //
}

impl SemanticCheck for ast::ExternStatic {
  //
}

impl SemanticCheck for ast::Record {
  fn infer_type(&self, cache: &cache::Cache) -> ast::Type {
    let struct_type_node = cache.force_get(&self.target_id.unwrap());

    let struct_type = match &(&*struct_type_node).kind {
      ast::NodeKind::StructType(struct_type) => struct_type,
      _ => unreachable!(),
    };

    // TODO: Is this the correct type? We might need this one in order to unify with the original struct type.
    ast::Type::Struct(struct_type.clone())
  }

  fn check(&self, context: &mut SemanticCheckContext, cache: &cache::Cache) {
    let struct_type_node = cache.force_get(&self.target_id.unwrap());

    let struct_type = match &(&*struct_type_node).kind {
      ast::NodeKind::StructType(struct_type) => struct_type,
      _ => unreachable!(),
    };

    if self.fields.len() != struct_type.fields.len() {
      context
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
    //   value_field.type_check(context, cache);

    //   let value_field_type = value_field.infer_type(cache).as_ref();

    //   if !unify_option(value_field_type, Some(struct_field.1), cache) {
    //     context.diagnostics.error(format!(
    //       "field and value at position `{}` type for struct `{}` mismatch",
    //       index, struct_type.name
    //     ));
    //   }
    // }
  }
}

impl SemanticCheck for ast::Prototype {
  fn infer_type(&self, _cache: &cache::Cache) -> ast::Type {
    // TODO: Simplify.
    ast::Type::Callable(ast::FunctionType {
      return_type: Box::new(self.return_type.clone()),
      parameter_types: self
        .parameters
        .iter()
        .map(|parameter| parameter.ty.clone())
        .collect(),
      is_variadic: self.is_variadic,
    })
  }

  fn check(&self, _context: &mut SemanticCheckContext, _cache: &cache::Cache) {
    // TODO: Implement?
  }
}

impl SemanticCheck for ast::RecordType {
  // TODO: Implement.
}

impl SemanticCheck for ast::UnaryExpr {
  fn infer_type(&self, cache: &cache::Cache) -> ast::Type {
    let expr_type = self.expr.infer_type(cache);

    // Short-circuit if the expression's type is unit.
    if expr_type.is_unit() {
      return ast::Type::Unit;
    }

    return match self.operator {
      ast::OperatorKind::AddressOf => ast::Type::Pointer(Box::new(expr_type)),
      ast::OperatorKind::Cast => self.cast_type.as_ref().unwrap().clone(),
      ast::OperatorKind::Not => ast::Type::Primitive(ast::BasicType::Bool),
      ast::OperatorKind::SubtractOrNegate => expr_type,
      _ => unreachable!(),
    };
  }

  fn check(&self, context: &mut SemanticCheckContext, cache: &cache::Cache) {
    let expr_type = self.expr.infer_type(cache);

    match self.operator {
      ast::OperatorKind::MultiplyOrDereference => {
        if !context.in_unsafe_block {
          context
            .diagnostic_builder
            .error("can only dereference inside an unsafe block".to_string());
        }

        if !matches!(expr_type, ast::Type::Pointer(_)) {
          context
            .diagnostic_builder
            .error("can only dereference pointers".to_string());
        }
      }
      ast::OperatorKind::Not => {
        if !SemanticCheckContext::unify(&expr_type, &ast::Type::Primitive(ast::BasicType::Bool)) {
          context
            .diagnostic_builder
            .error("can only negate boolean expressions".to_string());
        }
      }
      ast::OperatorKind::SubtractOrNegate => {
        // TODO: Include floats.
        // FIXME: Shouldn't we be using `unify` here? What about types that need to be resolved? How do we pass-in a variant tho.? Or maybe the inferred type is already at its simplest form? Verify.
        if !matches!(expr_type, ast::Type::Primitive(ast::BasicType::Int(_))) {
          // TODO: Error message too similar to the boolean negation case.
          context
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
          context
            .diagnostic_builder
            .error("can only cast between primitive types".to_string());
        } else if SemanticCheckContext::unify(&expr_type, self.cast_type.as_ref().unwrap()) {
          context
            .diagnostic_builder
            .warning("redundant cast to the same type".to_string());
        }
      }
      _ => unreachable!(),
    };
  }
}

impl SemanticCheck for ast::Enum {
  //
}

impl SemanticCheck for ast::ArrayIndexing {
  fn infer_type(&self, cache: &cache::Cache) -> ast::Type {
    let target_array_variable = &*cache.force_get(&self.target_id.unwrap());

    let array_type = match &target_array_variable.kind {
      ast::NodeKind::LetStmt(let_stmt) => &let_stmt.ty,
      ast::NodeKind::Parameter(parameter) => &parameter.ty,
      _ => unreachable!(),
    };

    let array_element_type = match array_type {
      ast::Type::Array(element_type, _) => element_type.as_ref().clone(),
      _ => unreachable!(),
    };

    array_element_type
  }

  fn check(&self, context: &mut SemanticCheckContext, cache: &cache::Cache) {
    let index_expr_type = self.index_expr.infer_type(cache);

    let is_unsigned_int_type =
      // TODO: Should we be using `unify` here, instead?
      if let ast::Type::Primitive(ast::BasicType::Int(int_size)) = index_expr_type {
        matches!(int_size, ast::IntSize::U8)
          || matches!(int_size, ast::IntSize::U16)
          || matches!(int_size, ast::IntSize::U32)
          || matches!(int_size, ast::IntSize::U64)
      } else {
        false
      };

    if !is_unsigned_int_type {
      context
        .diagnostic_builder
        .error("array index expression must evaluate to an unsigned integer".to_string());
    }

    self.index_expr.check(context, cache);
  }
}

impl SemanticCheck for ast::ArrayValue {
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

  fn check(&self, context: &mut SemanticCheckContext, cache: &cache::Cache) {
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
        context
          .diagnostic_builder
          .error("array elements must all be of the same type".to_string());

        mixed_elements_flag = true;
      }

      element.check(context, cache);
    }
  }
}

impl SemanticCheck for ast::UnsafeBlockStmt {
  fn infer_type(&self, cache: &cache::Cache) -> ast::Type {
    self.0.infer_type(cache)
  }

  fn check(&self, context: &mut SemanticCheckContext, cache: &cache::Cache) {
    // TODO: To avoid problems with nested cases, save a buffer here, then restore?
    context.in_unsafe_block = true;
    self.0.check(context, cache);
    context.in_unsafe_block = false;
  }
}

impl SemanticCheck for ast::ExternFunction {
  fn infer_type(&self, cache: &cache::Cache) -> ast::Type {
    self.prototype.infer_type(cache)
  }
}

impl SemanticCheck for ast::Parameter {
  fn infer_type(&self, _cache: &cache::Cache) -> ast::Type {
    self.ty.clone()
  }
}

impl SemanticCheck for ast::BlockExpr {
  fn infer_type(&self, cache: &cache::Cache) -> ast::Type {
    self.statements.last().unwrap().infer_type(cache)
  }

  fn check(&self, context: &mut SemanticCheckContext, cache: &cache::Cache) {
    for statement in &self.statements {
      statement.check(context, cache);
    }
  }
}

impl SemanticCheck for ast::Reference {
  fn infer_type(&self, cache: &cache::Cache) -> ast::Type {
    (&*cache)
      .force_get(&self.0.target_id.unwrap())
      .infer_type(cache)
  }
}

impl SemanticCheck for ast::Literal {
  fn infer_type(&self, _cache: &cache::Cache) -> ast::Type {
    ast::Type::Primitive(match self {
      ast::Literal::Bool(_) => ast::BasicType::Bool,
      ast::Literal::Char(_) => ast::BasicType::Char,
      ast::Literal::Int(_, size) => ast::BasicType::Int(size.clone()),
      ast::Literal::String(_) => ast::BasicType::String,
      ast::Literal::Nullptr => {
        return ast::Type::Pointer(Box::new(ast::Type::Primitive(ast::BasicType::Null)))
      }
    })
  }
}

impl SemanticCheck for ast::IfExpr {
  fn infer_type(&self, cache: &cache::Cache) -> ast::Type {
    // Both branches must be present in order for a value
    // to possibly evaluate.
    if self.else_value.is_none() {
      return ast::Type::Unit;
    }

    let else_block = self.else_value.as_ref().unwrap();
    let then_block_type = self.then_value.infer_type(cache);

    // FIXME: Perhaps make a special case for let-statement? Its type inference is used internally, but they should yield 'Unit' for the user.
    // In case of a type-mismatch between branches, simply return the unit type.
    if !SemanticCheckContext::unify(&then_block_type, &else_block.infer_type(cache)) {
      return ast::Type::Unit;
    }

    then_block_type
  }

  fn check(&self, context: &mut SemanticCheckContext, cache: &cache::Cache) {
    if !SemanticCheckContext::unify(
      &self.condition.infer_type(cache),
      &ast::Type::Primitive(ast::BasicType::Bool),
    ) {
      context
        .diagnostic_builder
        .error("if statement condition must evaluate to a boolean".to_string());
    }

    self.condition.check(context, cache);
    self.then_value.check(context, cache);

    if let Some(else_block) = &self.else_value {
      else_block.check(context, cache);
    }
  }
}

impl SemanticCheck for ast::BinaryExpr {
  fn infer_type(&self, cache: &cache::Cache) -> ast::Type {
    match self.operator {
      ast::OperatorKind::LessThan
      | ast::OperatorKind::GreaterThan
      | ast::OperatorKind::Equality
      | ast::OperatorKind::And
      | ast::OperatorKind::Or
      | ast::OperatorKind::Nand
      | ast::OperatorKind::Nor
      | ast::OperatorKind::Xor => ast::Type::Primitive(ast::BasicType::Bool),
      _ => self.left.infer_type(cache),
    }
  }

  fn check(&self, context: &mut SemanticCheckContext, cache: &cache::Cache) {
    let left_type = self.left.infer_type(cache);
    let right_type = self.right.infer_type(cache);

    // TODO: Also add checks for when using operators with wrong values (ex. less-than or greater-than comparison of booleans).

    // TODO: If we require both operands to  be of the same type, then operator overloading isn't possible with mixed operands as parameters.
    if !SemanticCheckContext::unify(&left_type, &right_type) {
      context
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
        if !matches!(left_type, ast::Type::Primitive(ast::BasicType::Int(_))) {
          context
            .diagnostic_builder
            .error("binary expression operands must be both integers".to_string());
        }
      }
      // TODO: Equality operator, and others? Implement.
      _ => {}
    };

    self.left.check(context, cache);
    self.right.check(context, cache);
  }
}

impl SemanticCheck for ast::InlineExprStmt {
  fn infer_type(&self, cache: &cache::Cache) -> ast::Type {
    self.expr.infer_type(cache)
  }

  fn check(&self, context: &mut SemanticCheckContext, cache: &cache::Cache) {
    self.expr.check(context, cache);
  }
}

impl SemanticCheck for ast::LetStmt {
  // FIXME: [!] This causes a bug where the string literal is not accessed (left as `i8**`). The let-statement didn't have a type before.
  fn infer_type(&self, cache: &cache::Cache) -> ast::Type {
    self.value.infer_type(&cache)
  }

  fn check(&self, context: &mut SemanticCheckContext, cache: &cache::Cache) {
    let value_type = self.value.infer_type(cache);

    if !SemanticCheckContext::unify(&self.ty, &value_type) {
      context.diagnostic_builder.error(format!(
        "variable declaration of `{}` value and type mismatch",
        self.name
      ));
    }

    self.value.check(context, cache);
  }
}

impl SemanticCheck for ast::Function {
  fn infer_type(&self, cache: &cache::Cache) -> ast::Type {
    self.prototype.infer_type(cache)
  }

  fn check(&self, context: &mut SemanticCheckContext, cache: &cache::Cache) {
    context.current_function_key = Some(self.unique_id);

    // TODO: Special case for the `main` function. Unify expected signature?

    if !SemanticCheckContext::unify(
      &self.prototype.return_type,
      &self.body_value.infer_type(cache),
    ) {
      context.diagnostic_builder.error(format!(
        "function body and prototype return type mismatch for function `{}`",
        self.name
      ));
    }

    if self.prototype.is_variadic {
      context.diagnostic_builder.error(format!(
        "function `{}` cannot be variadic; only externs are allowed to be variadic",
        self.name
      ));
    }

    if self.name == llvm_lowering::MAIN_FUNCTION_NAME {
      let main_prototype = ast::Prototype {
        // TODO: Parameters. Also, the comparison should ignore parameter names.
        parameters: vec![],
        return_type: ast::Type::Primitive(ast::BasicType::Int(ast::IntSize::I32)),
        is_variadic: false,
      };

      // TODO: Simplify.
      if self.prototype.infer_type(cache) != main_prototype.infer_type(cache) {
        context
          .diagnostic_builder
          .error(format!("the `main` function has an invalid signature"));
      }
    }

    self.prototype.check(context, cache);
    self.body_value.check(context, cache);
    context.current_function_key = None;
  }
}

impl SemanticCheck for ast::CallExpr {
  fn infer_type(&self, cache: &cache::Cache) -> ast::Type {
    let callee_expr_type = self.callee_expr.infer_type(cache);

    match callee_expr_type {
      ast::Type::Callable(callable_type) => callable_type.return_type.as_ref().clone(),
      _ => ast::Type::Error,
    }
  }

  fn check(&self, context: &mut SemanticCheckContext, cache: &cache::Cache) {
    self.callee_expr.check(context, cache);

    // TODO: Consider adopting a `expected` and `actual` API for diagnostics, when applicable.
    // TODO: Need access to the current function?

    let callee_expr_type = self.callee_expr.infer_type(cache);

    if !matches!(callee_expr_type, ast::Type::Callable(_)) {
      context
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
    //     if !context.in_unsafe_block {
    //       context.diagnostic_builder.error(format!(
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
      context
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
    //     context.diagnostics.error(format!(
    //       "function call argument and parameter `{}` type mismatch",
    //       parameter.0
    //     ));
    //   }
    // }
  }
}
