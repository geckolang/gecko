use crate::{ast, cache, type_system::Check};

pub trait ReadonlyVisitor {
  fn visit(&mut self, node: &ast::NodeKind) {
    match &node {
      ast::NodeKind::InlineExprStmt(inline_expr_stmt) => {
        self.visit_inline_expr_stmt(inline_expr_stmt);
        self.visit(&inline_expr_stmt.expr.kind);
      }
      ast::NodeKind::BinaryExpr(binary_expr) => {
        self.visit_binary_expr(binary_expr);
        self.visit(&binary_expr.left.kind);
        self.visit(&binary_expr.right.kind);
      }
      ast::NodeKind::BindingStmt(binding_stmt) => {
        self.visit_binding_stmt(binding_stmt);
        self.visit(&binding_stmt.value.kind);
      }
      ast::NodeKind::BlockExpr(block_expr) => {
        self.visit_block_expr(block_expr);

        for statement in &block_expr.statements {
          self.visit(&statement.kind);
        }
      }
      ast::NodeKind::CallExpr(call_expr) => {
        self.visit_call_expr(call_expr);
        self.visit(&call_expr.callee_expr.kind);

        for arg in &call_expr.arguments {
          self.visit(&arg.kind);
        }
      }
      ast::NodeKind::Closure(closure) => {
        self.visit_closure(closure);
        self.visit_prototype(&closure.prototype);
        self.visit_block_expr(&closure.body);
      }
      ast::NodeKind::Enum(enum_) => {
        self.visit_enum(enum_);
      }
      ast::NodeKind::ExternFunction(extern_function) => {
        self.visit_extern_function(extern_function);
      }
      ast::NodeKind::ExternStatic(extern_static) => {
        self.visit_extern_static(extern_static);
      }
      ast::NodeKind::Function(function) => {
        self.visit_prototype(&function.prototype);
        self.visit_block_expr(&function.body);
      }
      ast::NodeKind::IfExpr(if_expr) => {
        self.visit(&if_expr.condition.kind);
        self.visit(&if_expr.then_expr.kind);

        for alternative_branches in &if_expr.alternative_branches {
          self.visit(&alternative_branches.0.kind);
          self.visit(&alternative_branches.1.kind);
        }

        if let Some(else_expr) = &if_expr.else_expr {
          self.visit(&else_expr.kind);
        }
      }
      ast::NodeKind::IndexingExpr(indexing_expr) => {
        self.visit(&indexing_expr.index_expr.kind);
      }
      ast::NodeKind::Literal(literal) => {
        self.visit_literal(literal);
      }
      ast::NodeKind::MemberAccess(member_expr) => {
        self.visit_member_access(member_expr);
        self.visit(&member_expr.base_expr.kind);
      }
      ast::NodeKind::Parameter(parameter) => {
        self.visit_parameter(parameter);
      }
      ast::NodeKind::ParenthesesExpr(parentheses_expr) => {
        self.visit_parentheses_expr(parentheses_expr);
        self.visit(&parentheses_expr.0.kind);
      }
      ast::NodeKind::Prototype(prototype) => {
        self.visit_prototype(prototype);

        for parameter in &prototype.parameters {
          self.visit_parameter(parameter);
        }
      }
      ast::NodeKind::ReturnStmt(return_expr) => {
        self.visit_return_stmt(return_expr);

        if let Some(return_value) = &return_expr.value {
          self.visit(&return_value.kind);
        }
      }
      ast::NodeKind::IntrinsicCall(intrinsic_call) => {
        self.visit_intrinsic_call(intrinsic_call);

        for arg in &intrinsic_call.arguments {
          self.visit(&arg.kind);
        }
      }
      ast::NodeKind::Reference(reference) => {
        self.visit_reference(reference);
      }
      ast::NodeKind::StructImpl(struct_impl) => {
        self.visit_struct_impl(struct_impl);
      }
      ast::NodeKind::UnaryExpr(unary_expr) => {
        self.visit_unary_expr(unary_expr);
        self.visit(&unary_expr.expr.kind);
      }
      ast::NodeKind::UnsafeExpr(unsafe_expr) => {
        self.visit_unsafe_expr(unsafe_expr);
        self.visit(&unsafe_expr.0.kind);
      }
      ast::NodeKind::StaticArrayValue(static_array_value) => {
        self.visit_static_array_value(static_array_value);

        for element in &static_array_value.elements {
          self.visit(&element.kind);
        }
      }
      ast::NodeKind::StructType(struct_type) => {
        self.visit_struct_type(struct_type);
      }
      ast::NodeKind::StructValue(struct_value) => {
        self.visit_struct_value(struct_value);

        for field in &struct_value.fields {
          self.visit(&field.kind);
        }
      }
      ast::NodeKind::Pattern(pattern) => {
        self.visit_pattern(pattern);
      }
      ast::NodeKind::Using(using) => {
        self.visit_using(using);
      }
      ast::NodeKind::TypeAlias(type_alias) => {
        self.visit_type_alias(type_alias);
      }
      ast::NodeKind::Trait(trait_) => {
        self.visit_trait(trait_);
      }
      ast::NodeKind::SizeofIntrinsic(sizeof_intrinsic) => {
        self.visit_sizeof_intrinsic(sizeof_intrinsic);
      }
      ast::NodeKind::Range(range) => {
        self.visit_range(range);
        self.visit(&range.start.kind);
        self.visit(&range.end.kind);
      }
    };
  }

  fn visit_literal(&mut self, _node: &ast::Literal) {
    //
  }

  fn visit_extern_function(&mut self, _node: &ast::ExternFunction) {
    //
  }

  fn visit_prototype(&mut self, _node: &ast::Prototype) {
    //
  }

  fn visit_extern_static(&mut self, _node: &ast::ExternStatic) {
    //
  }

  fn visit_inline_expr_stmt(&mut self, _node: &ast::InlineExprStmt) {
    //
  }

  fn visit_block_expr(&mut self, _node: &ast::BlockExpr) {
    //
  }

  fn visit_closure(&mut self, _node: &ast::Closure) {
    //
  }

  fn visit_enum(&mut self, _node: &ast::Enum) {
    //
  }

  fn visit_parameter(&mut self, _node: &ast::Parameter) {
    //
  }

  fn visit_intrinsic_call(&mut self, _node: &ast::IntrinsicCall) {
    //
  }

  fn visit_reference(&mut self, _node: &ast::Reference) {
    //
  }

  fn visit_struct_impl(&mut self, _node: &ast::StructImpl) {
    //
  }

  fn visit_unary_expr(&mut self, _node: &ast::UnaryExpr) {
    //
  }

  fn visit_struct_type(&mut self, _node: &ast::StructType) {
    //
  }

  fn visit_pattern(&mut self, _node: &ast::Pattern) {
    //
  }

  fn visit_using(&mut self, _node: &ast::Using) {
    //
  }

  fn visit_type_alias(&mut self, _node: &ast::TypeAlias) {
    //
  }

  fn visit_trait(&mut self, _node: &ast::Trait) {
    //
  }

  fn visit_sizeof_intrinsic(&mut self, _node: &ast::SizeofIntrinsic) {
    //
  }

  fn visit_range(&mut self, _node: &ast::Range) {
    //
  }

  fn visit_struct_value(&mut self, _node: &ast::StructValue) {
    //
  }

  fn visit_static_array_value(&mut self, _node: &ast::StaticArrayValue) {
    //
  }

  fn visit_unsafe_expr(&mut self, _node: &ast::UnsafeExpr) {
    //
  }

  fn visit_return_stmt(&mut self, _node: &ast::ReturnStmt) {
    //
  }

  fn visit_parentheses_expr(&mut self, _node: &ast::ParenthesesExpr) {
    //
  }

  fn visit_member_access(&mut self, _node: &ast::MemberAccess) {
    //
  }

  fn visit_binary_expr(&mut self, _node: &ast::BinaryExpr) {
    //
  }

  fn visit_binding_stmt(&mut self, _node: &ast::BindingStmt) {
    //
  }

  fn visit_call_expr(&mut self, _node: &ast::CallExpr) {
    //
  }
}

struct TypeCheckVisitor<'a> {
  diagnostics: Vec<codespan_reporting::diagnostic::Diagnostic<usize>>,
  in_unsafe_block: bool,
  in_impl: bool,
  current_function_id: Option<cache::Id>,
  // REVISE: Make use-of or discard.
  _types_cache: std::collections::HashMap<cache::Id, ast::Type>,
  usings: Vec<ast::Using>,
  /// A map from a type variable's id to a type.
  ///
  /// This serves as a buffer for type inference to occur. It is
  /// populated during parsing phase, when type variables are created, and
  /// it also is scope-less/context-free.
  substitutions: std::collections::HashMap<usize, ast::Type>,
  bound_checked_arrays: std::collections::HashSet<cache::Id>,
  cache: &'a cache::Cache,
}

impl<'a> TypeCheckVisitor<'a> {
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
}

impl<'a> ReadonlyVisitor for TypeCheckVisitor<'a> {
  fn visit_call_expr(&mut self, _node: &ast::CallExpr) {
    self.in_unsafe_block = true;
  }

  fn visit_struct_impl(&mut self, node: &ast::StructImpl) {
    self.in_impl = true;

    for method in &node.member_methods {
      if !method.prototype.accepts_instance {
        self.diagnostics.push(
          codespan_reporting::diagnostic::Diagnostic::error().with_message(format!(
            "implementation method `{}` is missing the instance parameter `this`",
            method.name
          )),
        )
      }
    }

    let target_node = self
      .cache
      .force_get(&node.target_struct_pattern.target_id.unwrap());

    // REVISE: Cleanup.
    if let ast::NodeKind::StructType(_target_struct_type) = &target_node {
      if let Some(trait_pattern) = &node.trait_pattern {
        let trait_node = self.cache.force_get(&trait_pattern.target_id.unwrap());

        if let ast::NodeKind::Trait(trait_type) = &trait_node {
          for trait_method in &trait_type.methods {
            let impl_method_result = node
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
                self.diagnostics.push(
                  codespan_reporting::diagnostic::Diagnostic::error().with_message(format!(
                    "prototype of implementation method `{}` for trait `{}` mismatch in {}",
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
          node.target_struct_pattern.base_name
        )),
      );
    }

    self.in_impl = false;
  }

  fn visit_using(&mut self, node: &ast::Using) {
    // FIXME: Can't just push the import once encountered; only when it's actually used.
    self.usings.push(node.clone());
  }

  fn visit_sizeof_intrinsic(&mut self, node: &ast::SizeofIntrinsic) {
    if node.ty.flatten(self.cache).is(&ast::Type::Unit) {
      self.diagnostics.push(
        codespan_reporting::diagnostic::Diagnostic::error()
          .with_message("cannot determine size of unit type"),
      );
    }
  }

  fn visit_member_access(&mut self, node: &ast::MemberAccess) {
    let base_expr_type = node.base_expr.kind.infer_flatten_type(self.cache);

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

    if !struct_type.fields.iter().any(|x| x.0 == node.member_name) {
      // TODO:
      // context.diagnostic_builder.error(format!(
      //   "struct type `{}` does not contain a field named `{}`",
      //   struct_type.name, self.member_name
      // ));
    }
  }

  fn visit_closure(&mut self, node: &ast::Closure) {
    // REVIEW: Might need to mirror `Function`'s type check.
    let previous_function_id = self.current_function_id.clone();

    self.current_function_id = Some(node.id);

    if node.prototype.accepts_instance {
      self.diagnostics.push(
        codespan_reporting::diagnostic::Diagnostic::error()
          .with_message("closures cannot accept instances"),
      );
    }

    self.current_function_id = previous_function_id;
  }

  fn visit_intrinsic_call(&mut self, node: &ast::IntrinsicCall) {
    // TODO: Redundant to have function return types.
    let target_prototype_sig: (Vec<ast::Type>, ast::Type) = match node.kind {
      ast::IntrinsicKind::LengthOf => (
        // Cannot define array type directly. Use the any type for comparison.
        vec![ast::Type::Any],
        ast::Type::Basic(ast::BasicType::Int(ast::IntSize::I32)),
      ),
    };

    let target_function_type = ast::FunctionType {
      is_extern: false,
      is_variadic: false,
      parameter_types: target_prototype_sig.0,
      return_type: Box::new(target_prototype_sig.1),
    };

    self.validate_fn_call(
      node
        .arguments
        .iter()
        // No need to flatten.
        .map(|argument| argument.kind.infer_type(self.cache))
        .collect(),
      target_function_type,
      self.cache,
    );

    // Special case because of the static array type.
    if matches!(node.kind, ast::IntrinsicKind::LengthOf) && node.arguments.len() == 1 {
      let target_array = node.arguments.first().unwrap();
      let target_array_type = target_array.kind.infer_flatten_type(self.cache);

      if !matches!(target_array_type, ast::Type::Array(..)) {
        self.diagnostics.push(
          codespan_reporting::diagnostic::Diagnostic::error()
            .with_message("cannot determine static length of non-array type"),
        );
      }
    }
  }

  fn visit_range(&mut self, node: &ast::Range) {
    // NOTE: No need to check whether the range's bounds are constant
    // ... expressions, this is ensured by the parser.

    let start_literal = crate::force_match!(&node.start.kind, ast::NodeKind::Literal);
    let end_literal = crate::force_match!(&node.end.kind, ast::NodeKind::Literal);

    let start_int = match start_literal {
      ast::Literal::Int(value, _) => value,
      _ => unreachable!(),
    };

    let end_int = match end_literal {
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
}

// TODO: Find a way to be able to chain multiple readonly visitors in a single pass.
