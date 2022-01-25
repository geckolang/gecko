use crate::{ast, cache, diagnostic, dispatch};

pub struct TypeCheckContext {
  pub diagnostics: diagnostic::DiagnosticBuilder,
  in_loop: bool,
  in_unsafe_block: bool,
  does_function_return: bool,
}

impl TypeCheckContext {
  pub fn new() -> Self {
    Self {
      diagnostics: diagnostic::DiagnosticBuilder::new(),
      in_loop: false,
      in_unsafe_block: false,
      does_function_return: false,
    }
  }

  /// Compare two types for equality.
  fn unify(type_a: &ast::Type, type_b: &ast::Type, cache: &cache::Cache) -> bool {
    let unboxed_type_a = Self::unbox_type(type_a, cache);
    let unboxed_type_b = Self::unbox_type(type_b, cache);

    unboxed_type_a == unboxed_type_b
  }

  // FIXME: Does this work as expected? Generated by GitHub Codepilot.
  /// Compare two optional types for equality.
  fn unify_option(
    type_a: Option<&ast::Type>,
    type_b: Option<&ast::Type>,
    cache: &cache::Cache,
  ) -> bool {
    match (type_a, type_b) {
      (Some(type_a), Some(type_b)) => Self::unify(&type_a, &type_b, cache),
      _ => type_a == type_b,
    }
  }

  /// Shortcut for unifying an optional type with a primitive type.
  fn unify_with_primitive(ty: Option<ast::Type>, primitive: ast::PrimitiveType) -> bool {
    ty == Some(ast::Type::Primitive(primitive))
  }

  /// Attempt to unbox a user-defined type, to be used for comparison.
  fn unbox_type(ty: &ast::Type, cache: &cache::Cache) -> ast::Type {
    match ty {
      ast::Type::UserDefined(user_defined_type) => {
        let target_type = cache.get(&user_defined_type.target_key.unwrap());

        match &*target_type {
          // TODO: Cloning struct type.
          ast::Node::StructType(struct_type) => ast::Type::Struct(struct_type.clone()),
          _ => unreachable!(),
        }
      }
      // TODO: Cloning type on most cases! Inefficient.
      _ => ty.clone(),
    }
  }
}

pub trait TypeCheck {
  fn infer_type(&self, _cache: &cache::Cache) -> Option<ast::Type> {
    None
  }

  fn type_check(&self, _type_context: &mut TypeCheckContext, _cache: &mut cache::Cache) {
    //
  }
}

impl TypeCheck for ast::Node {
  fn type_check(&self, type_context: &mut TypeCheckContext, cache: &mut cache::Cache) {
    dispatch!(self, TypeCheck::type_check, type_context, cache);
  }

  fn infer_type(&self, cache: &cache::Cache) -> Option<ast::Type> {
    dispatch!(self, TypeCheck::infer_type, cache)
  }
}

impl TypeCheck for ast::StructValue {
  fn infer_type(&self, cache: &cache::Cache) -> Option<ast::Type> {
    let struct_type_node = cache.get(&self.target_key.unwrap());

    let struct_type = match &*struct_type_node {
      ast::Node::StructType(struct_type) => struct_type,
      _ => unreachable!(),
    };

    // TODO: Is this the correct type? We might need this one in order to unify with the original struct type.
    Some(ast::Type::Struct(struct_type.clone()))
  }

  fn type_check(&self, type_context: &mut TypeCheckContext, cache: &mut cache::Cache) {
    let struct_type_node = cache.get(&self.target_key.unwrap());

    let struct_type = match &*struct_type_node {
      ast::Node::StructType(struct_type) => struct_type,
      _ => unreachable!(),
    };

    if self.fields.len() != struct_type.fields.len() {
      type_context
        .diagnostics
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
  fn type_check(&self, _type_context: &mut TypeCheckContext, _cache: &mut cache::Cache) {
    // TODO: Implement?
  }
}

impl TypeCheck for ast::StructType {
  // TODO: Implement.
}

impl TypeCheck for ast::UnaryExpr {
  fn type_check(&self, type_context: &mut TypeCheckContext, cache: &mut cache::Cache) {
    match self.operator {
      ast::OperatorKind::MultiplyOrDereference => {
        if !type_context.in_unsafe_block {
          type_context
            .diagnostics
            .error("can only dereference inside an unsafe block".to_string());
        }
      }
      ast::OperatorKind::Not => {
        let expr_type = self.expr.infer_type(cache);

        if !TypeCheckContext::unify_with_primitive(expr_type, ast::PrimitiveType::Bool) {
          type_context
            .diagnostics
            .error("can only negate boolean expressions".to_string());
        }
      }
      ast::OperatorKind::SubtractOrNegate => {
        let expr_type = self.expr.infer_type(cache);

        // TODO: Include floats.
        if !matches!(
          expr_type,
          Some(ast::Type::Primitive(ast::PrimitiveType::Int(_)))
        ) {
          // TODO: Error message too similar to the boolean negation case.
          type_context
            .diagnostics
            .error("can only negate integers or float expressions".to_string());
        }
      }
      ast::OperatorKind::AddressOf => {
        // TODO: Implement.
        todo!();
      }
      _ => unreachable!(),
    };
  }

  fn infer_type(&self, cache: &cache::Cache) -> Option<ast::Type> {
    self.expr.infer_type(cache)
  }
}

impl TypeCheck for ast::Enum {
  //
}

impl TypeCheck for ast::AssignStmt {
  fn type_check(&self, type_context: &mut TypeCheckContext, cache: &mut cache::Cache) {
    // TODO: Need to unify the value and the target's type, as well as ensuring that the target is mutable.

    let assignee_type = self.assignee_expr.infer_type(cache);
    let is_pointer_or_ref_expr = matches!(assignee_type, Some(ast::Type::Pointer(_)));
    let is_variable_ref = matches!(self.assignee_expr.as_ref(), ast::Node::VariableRef(_));
    let is_array_indexing = matches!(self.assignee_expr.as_ref(), ast::Node::ArrayIndexing(_));

    // TODO: Missing member access (struct fields) support.
    // NOTE: The assignee expression may only be an expression of type `Pointer`
    // or `Reference`, a variable reference, or an array indexing.
    if !is_pointer_or_ref_expr && !is_variable_ref && !is_array_indexing {
      type_context
        .diagnostics
        .error("assignee must be an expression of pointer or reference type, a variable reference, or an array indexing".to_string());
    } else if is_variable_ref {
      // If the assignee is a variable reference, ensure that the variable is mutable.
      match self.assignee_expr.as_ref() {
        ast::Node::VariableRef(variable_ref) => {
          let declaration = cache.get(&variable_ref.target_key.unwrap());

          match &*declaration {
            ast::Node::LetStmt(let_stmt) if !let_stmt.is_mutable => {
              type_context
                .diagnostics
                .error("assignee is immutable".to_string());
            }
            // TODO: Parameters should be immutable by default.
            _ => {}
          };
        }
        _ => unreachable!(),
      };
    }
  }
}

impl TypeCheck for ast::ContinueStmt {
  fn type_check(&self, type_context: &mut TypeCheckContext, _cache: &mut cache::Cache) {
    if !type_context.in_loop {
      type_context
        .diagnostics
        .error("continue statement may only occur inside loops".to_string());
    }
  }
}

impl TypeCheck for ast::ArrayIndexing {
  fn infer_type(&self, cache: &cache::Cache) -> Option<ast::Type> {
    let target_array_variable = &*cache.get(&self.target_key.unwrap());

    let array_type = match target_array_variable {
      ast::Node::LetStmt(let_stmt) => &let_stmt.ty,
      ast::Node::Parameter(parameter) => &parameter.1,
      _ => unreachable!(),
    };

    let array_element_type = match array_type {
      ast::Type::Array(element_type, _) => element_type.as_ref().clone(),
      _ => unreachable!(),
    };

    Some(array_element_type)
  }

  fn type_check(&self, _type_context: &mut TypeCheckContext, _cache: &mut cache::Cache) {
    // TODO: Implement.
  }
}

impl TypeCheck for ast::ArrayValue {
  fn infer_type(&self, cache: &cache::Cache) -> Option<ast::Type> {
    // TODO: Temporary, until type-inference is implemented.
    // We assume that the length is `0` if the explicit type is provided, otherwise
    // the array type is determined by the first element.
    let array_element_type = if let Some(explicit_type) = &self.explicit_type {
      explicit_type.clone()
    } else {
      self.elements.first().unwrap().infer_type(cache).unwrap()
    };

    // TODO: Is the length conversion safe?
    let array_type = ast::Type::Array(Box::new(array_element_type), self.elements.len() as u32);

    Some(array_type)
  }

  fn type_check(&self, type_context: &mut TypeCheckContext, cache: &mut cache::Cache) {
    // FIXME: Here, we assume that `explicit_type` is always `Some(_)`. Currently, that might not be the case until type inference is implemented.
    let mut mixed_elements_flag = false;

    let expected_element_type = Some(if let Some(explicit_type) = &self.explicit_type {
      explicit_type.clone()
    } else {
      self.elements.first().unwrap().infer_type(cache).unwrap()
    });

    // TODO: Skip the first element during iteration, as it is redundant.
    for element in &self.elements {
      // Report this error only once.
      if !mixed_elements_flag && element.infer_type(cache) != expected_element_type {
        type_context
          .diagnostics
          .error("array elements must all be of the same type".to_string());

        mixed_elements_flag = true;
      }

      element.type_check(type_context, cache);
    }
  }
}

impl TypeCheck for ast::UnsafeBlockStmt {
  fn type_check(&self, type_context: &mut TypeCheckContext, cache: &mut cache::Cache) {
    // TODO: To avoid problems with nested cases, save a buffer here, then restore?
    type_context.in_unsafe_block = true;
    self.0.type_check(type_context, cache);
    type_context.in_unsafe_block = false;
  }
}

impl TypeCheck for ast::Extern {
  //
}

impl TypeCheck for ast::Parameter {
  //
}

impl TypeCheck for ast::Block {
  fn type_check(&self, type_context: &mut TypeCheckContext, cache: &mut cache::Cache) {
    for statement in &self.statements {
      statement.type_check(type_context, cache);
    }
  }
}

impl TypeCheck for ast::VariableRef {
  fn infer_type(&self, cache: &cache::Cache) -> Option<ast::Type> {
    // TODO: Simplify.
    let target_variable = &*cache.get(&self.target_key.unwrap());

    let variable_type = match target_variable {
      ast::Node::LetStmt(let_stmt) => &let_stmt.ty,
      ast::Node::Parameter(parameter) => &parameter.1,
      _ => unreachable!(),
    };

    Some(variable_type.clone())
  }
}

impl TypeCheck for ast::Literal {
  fn infer_type(&self, _cache: &cache::Cache) -> Option<ast::Type> {
    Some(ast::Type::Primitive(match self {
      ast::Literal::Bool(_) => ast::PrimitiveType::Bool,
      ast::Literal::Char(_) => ast::PrimitiveType::Char,
      ast::Literal::Int(_, size) => ast::PrimitiveType::Int(size.clone()),
      ast::Literal::String(_) => ast::PrimitiveType::String,
    }))
  }
}

impl TypeCheck for ast::IfStmt {
  fn type_check(&self, type_context: &mut TypeCheckContext, cache: &mut cache::Cache) {
    if !TypeCheckContext::unify_with_primitive(
      self.condition.infer_type(cache),
      ast::PrimitiveType::Bool,
    ) {
      type_context
        .diagnostics
        .error("if statement condition must evaluate to a boolean".to_string());
    }
  }
}

impl TypeCheck for ast::BinaryExpr {
  fn infer_type(&self, cache: &cache::Cache) -> Option<ast::Type> {
    // TODO: What if the binary expression is comparing? Then it would be bool, not the type of the left arm.
    self.left.infer_type(cache)
  }

  fn type_check(&self, type_context: &mut TypeCheckContext, cache: &mut cache::Cache) {
    let left_type = self.left.infer_type(cache);
    let right_type = self.right.infer_type(cache);

    // TODO: Also add checks for when using operators with wrong values (ex. less-than or greater-than comparison of booleans).

    // TODO: If we require both operands to  be of the same type, then operator overloading isn't possible with mixed operands as parameters.
    if left_type != right_type {
      type_context
        .diagnostics
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
        if !matches!(
          left_type,
          Some(ast::Type::Primitive(ast::PrimitiveType::Int(_)))
        ) {
          type_context
            .diagnostics
            .error("binary expression operands must be both integers".to_string());
        }
      }
      // TODO: Equality operator, and others?
      _ => todo!(),
    };

    self.left.type_check(type_context, cache);
    self.right.type_check(type_context, cache);
  }
}

impl TypeCheck for ast::BreakStmt {
  fn type_check(&self, type_context: &mut TypeCheckContext, _cache: &mut cache::Cache) {
    if !type_context.in_loop {
      type_context
        .diagnostics
        .error("break statement may only occur inside loops".to_string());
    }
  }
}

impl TypeCheck for ast::Definition {
  fn type_check(&self, type_context: &mut TypeCheckContext, cache: &mut cache::Cache) {
    self.node.borrow().type_check(type_context, cache);
  }
}

impl TypeCheck for ast::ExprStmt {
  fn type_check(&self, type_context: &mut TypeCheckContext, cache: &mut cache::Cache) {
    self.expr.type_check(type_context, cache);
  }
}

impl TypeCheck for ast::LetStmt {
  fn type_check(&self, type_context: &mut TypeCheckContext, cache: &mut cache::Cache) {
    let value_type = self.value.infer_type(cache);

    // TODO: Ensure this comparison works as expected (especially for complex types).
    // TODO: Comparing references?
    if !TypeCheckContext::unify_option(Some(&self.ty), value_type.as_ref(), cache) {
      type_context
        .diagnostics
        .error("let statement value and type mismatch".to_string());
    }

    self.value.type_check(type_context, cache);
  }
}

impl TypeCheck for ast::ReturnStmt {
  fn type_check(&self, type_context: &mut TypeCheckContext, cache: &mut cache::Cache) {
    if type_context.does_function_return && self.value.is_none() {
      type_context
        .diagnostics
        .error("return statement must return a value".to_string());
    } else if !type_context.does_function_return && self.value.is_some() {
      type_context
        .diagnostics
        .error("return statement must not return a value".to_string());
    }

    if let Some(value) = &self.value {
      // TODO: Unify prototype return type with the value's return type.

      value.type_check(type_context, cache);
    }
  }
}

impl TypeCheck for ast::Function {
  fn type_check(&self, type_context: &mut TypeCheckContext, cache: &mut cache::Cache) {
    type_context.does_function_return = self.prototype.return_type.is_some();

    // NOTE: No need to type-check parameters.
    self.body.type_check(type_context, cache);

    // TODO: Special case for the `main` function. Unify expected signature.
    // TODO: If it must return value, ensure a value was returned.
  }
}

impl TypeCheck for ast::FunctionCall {
  fn infer_type(&self, cache: &cache::Cache) -> Option<ast::Type> {
    // TODO: Is this cloning? Simplify this messy code section.
    let function_or_extern = &*cache.get(&self.target_key.unwrap());

    let prototype = match function_or_extern {
      ast::Node::Function(function) => &function.prototype,
      ast::Node::Extern(extern_) => &extern_.prototype,
      _ => unreachable!(),
    };

    prototype.return_type.clone()
  }

  fn type_check(&self, type_context: &mut TypeCheckContext, cache: &mut cache::Cache) {
    // TODO: Need access to the current function.
    // TODO: Ensure externs and unsafe function are only called from unsafe functions.

    let callee = &*cache.get(self.target_key.as_ref().unwrap());

    // TODO: Better, simpler way of doing this?
    let prototype: &ast::Prototype = match callee {
      ast::Node::Extern(extern_) => {
        if !type_context.in_unsafe_block {
          type_context
            .diagnostics
            .error("extern function calls may only occur inside an unsafe block".to_string());
        }

        &extern_.prototype
      }
      ast::Node::Function(function) => &function.prototype,
      _ => unreachable!(),
    };

    let min_arg_count = prototype.parameters.len();
    let actual_arg_count = self.arguments.len();
    const ARG_COUNT_MISMATCH: &str = "function call argument count mismatch";

    // Verify argument count.
    if !prototype.is_variadic && actual_arg_count != min_arg_count {
      type_context
        .diagnostics
        .error(ARG_COUNT_MISMATCH.to_string());
    } else if prototype.is_variadic && actual_arg_count < min_arg_count {
      type_context
        .diagnostics
        .error(ARG_COUNT_MISMATCH.to_string());
    }

    // FIXME: Different amount of arguments and parameters (due to variadic parameters) may affect this.
    // Unify argument and parameter types.
    for (parameter, argument) in prototype.parameters.iter().zip(self.arguments.iter()) {
      let parameter_type = parameter.infer_type(cache);
      let argument_type = argument.infer_type(cache);

      if argument_type != parameter_type {
        // TODO: Include callee name in the error message.
        type_context.diagnostics.error(format!(
          "function call argument and parameter `{}` type mismatch",
          parameter.0
        ));
      }
    }
  }
}

impl TypeCheck for ast::WhileStmt {
  fn type_check(&self, type_context: &mut TypeCheckContext, cache: &mut cache::Cache) {
    if !TypeCheckContext::unify_with_primitive(
      self.condition.infer_type(cache),
      ast::PrimitiveType::Bool,
    ) {
      type_context
        .diagnostics
        .error("while statement condition must evaluate to a boolean".to_string());
    }

    // TODO: To avoid problems with nested cases, save a buffer here, then restore?
    type_context.in_loop = true;
    self.condition.type_check(type_context, cache);
    self.body.type_check(type_context, cache);
    type_context.in_loop = false;
  }
}
