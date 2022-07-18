use crate::{ast, cache};

pub struct TypeContext {
  diagnostics: Vec<codespan_reporting::diagnostic::Diagnostic<usize>>,
}

impl TypeContext {
  pub fn run(
    ast: &Vec<ast::Node>,
    cache: &cache::Cache,
  ) -> Vec<codespan_reporting::diagnostic::Diagnostic<usize>> {
    let mut type_context = TypeContext::new();

    // TODO: What about constraint reports, and post-unification?

    // for top_level_node in inner_ast.iter_mut() {
    //   top_level_node
    //     .kind
    //     .post_unification(&mut type_context, &cache);
    // }

    for node in ast {
      // node.kind.check(&mut type_context, cache);
      // node.report_constraints(&mut semantic_check_context, cache);
    }

    type_context.diagnostics
  }

  pub fn new() -> Self {
    Self {
      diagnostics: Vec::new(),
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
          ty = return_value.kind.infer_type(cache);
        }

        // If the return statement is empty, then the function's return type is unit.
        return false;
      }

      true
    });

    ty
  }
}

pub trait Check {
  // REVIEW: Consider caching inference results here, if they are indeed costly.
  fn infer_type(&self, _cache: &cache::Cache) -> ast::Type {
    ast::Type::Unit
  }
}

impl Check for ast::NodeKind {
  fn infer_type(&self, _cache: &cache::Cache) -> ast::Type {
    // dispatch!(&self, Check::infer_type, cache)
    // TODO: Temporary.
    ast::Type::Unit
  }
}

impl Check for ast::SizeofIntrinsic {
  fn infer_type(&self, _cache: &cache::Cache) -> ast::Type {
    ast::Type::Basic(ast::BasicType::Int(ast::IntSize::I64))
  }
}

impl Check for ast::Trait {
  //
}

impl Check for ast::MemberAccess {
  fn infer_type(&self, cache: &cache::Cache) -> ast::Type {
    let base_expr_type = self.base_expr.kind.infer_flatten_type(cache);

    let struct_type = match base_expr_type {
      ast::Type::Struct(struct_type) => struct_type,
      // REVIEW: Investigate this strategy. Shouldn't we be using `unreachable!()` instead?
      // ... But this point may be reachable from the user-side. Need to somehow properly
      // ... handle this case.
      _ => todo!(),
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

    todo!();
  }
}

impl Check for ast::Closure {
  fn infer_type(&self, cache: &cache::Cache) -> ast::Type {
    TypeContext::infer_signature_type(
      &self.signature,
      Some(TypeContext::infer_return_value_type(&self.body, cache)),
    )
  }
}

impl Check for ast::TypeAlias {
  // REVIEW: Don't we need to implement `infer_type` here? Seems like not. Confirm.
}

impl Check for ast::Pattern {
  //
}

impl Check for ast::IntrinsicCall {
  fn infer_type(&self, _cache: &cache::Cache) -> ast::Type {
    match self.kind {
      ast::IntrinsicKind::LengthOf => ast::Type::Basic(ast::BasicType::Int(ast::IntSize::I32)),
    }
  }
}

impl Check for ast::ExternStatic {
  fn infer_type(&self, _cache: &cache::Cache) -> ast::Type {
    self.ty.clone()
  }
}

impl Check for ast::StructValue {
  fn infer_type(&self, cache: &cache::Cache) -> ast::Type {
    let struct_type = match cache.force_get(&self.target_id) {
      ast::NodeKind::Struct(struct_type) => struct_type,
      _ => unreachable!(),
    };

    // REVIEW: Is this the correct type? We might need this one in order to unify with the original struct type.
    ast::Type::Struct(struct_type.clone())
  }
}

impl Check for ast::Signature {
  //
}

impl Check for ast::Struct {
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
}

impl Check for ast::Enum {
  // REVIEW: Isn't there a need for its variants to be considered integer types?
}

impl Check for ast::IndexingExpr {
  fn infer_type(&self, cache: &cache::Cache) -> ast::Type {
    let target_array = cache.force_get(&self.target_id);
    let array_type = target_array.infer_flatten_type(cache);

    // TODO: In the future, add support for when indexing strings.
    let element_type = match array_type {
      ast::Type::Array(element_type, _) => element_type.as_ref().clone(),
      _ => unreachable!(),
    };

    element_type
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
}

impl Check for ast::UnsafeExpr {
  fn infer_type(&self, cache: &cache::Cache) -> ast::Type {
    self.0.kind.infer_type(cache)
  }
}

impl Check for ast::ExternFunction {
  fn infer_type(&self, _cache: &cache::Cache) -> ast::Type {
    TypeContext::infer_signature_type(&self.signature, self.signature.return_type_hint.clone())
  }
}

impl Check for ast::Parameter {
  fn infer_type(&self, _cache: &cache::Cache) -> ast::Type {
    self.type_hint.as_ref().unwrap().clone()
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
}

impl Check for ast::Reference {
  fn infer_type(&self, cache: &cache::Cache) -> ast::Type {
    // REVIEW: We should have some sort of caching specifically applied to this construct.
    cache.force_get(&self.pattern.id).infer_type(cache)
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
    let then_expr_type = self.then_value.kind.infer_flatten_type(cache);

    // At least the two main branches must be present in order for a value
    // to possibly evaluate.
    if self.else_value.is_none() {
      return ast::Type::Unit;
    }

    // TODO: Take into consideration newly-added alternative branches.

    let else_expr = self.else_value.as_ref().unwrap();
    let else_expr_type = else_expr.kind.infer_flatten_type(cache);

    // Default to type unit if both branches are of incompatible types.
    then_expr_type
      .coercion(&else_expr_type)
      .unwrap_or(ast::Type::Unit)
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
      | ast::OperatorKind::Xor
      | ast::OperatorKind::In => ast::Type::Basic(ast::BasicType::Bool),
      _ => self.left.kind.infer_type(cache),
    }
  }
}

impl Check for ast::InlineExprStmt {
  fn infer_type(&self, cache: &cache::Cache) -> ast::Type {
    self.expr.kind.infer_type(cache)
  }
}

impl Check for ast::BindingStmt {
  // BUG: This causes a bug where the string literal is not accessed (left as `i8**`). The let-statement didn't have a type before.
  fn infer_type(&self, cache: &cache::Cache) -> ast::Type {
    self.value.kind.infer_type(cache)
  }
}

impl Check for ast::ReturnStmt {
  // FIXME: This implies that the return statement may be used as an expression,
  // ... but that's currently not possible. Instead, this type is currently solely
  // ... used to determine if all paths return a value?
  fn infer_type(&self, _cache: &cache::Cache) -> ast::Type {
    ast::Type::Never
  }
}

impl Check for ast::Function {
  fn infer_type(&self, cache: &cache::Cache) -> ast::Type {
    // REVIEW: Why not use annotated return type if defined?
    todo!()
    // TypeContext::infer_signature_type(
    //   &self.signature,
    //   Some(TypeContext::infer_return_value_type(&self.body, cache)),
    // )
  }
}

impl Check for ast::CallExpr {
  fn infer_type(&self, cache: &cache::Cache) -> ast::Type {
    let callee_expr_type = self.callee_expr.kind.infer_type(cache);

    match callee_expr_type {
      ast::Type::Function(callable_type) => callable_type.return_type.as_ref().clone(),
      _ => todo!(),
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

  //   assert!(type_context.diagnostics.is_empty());
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
  //     type_hint: Some(ast::Type::Variable(type_variable_id)),
  //     // TODO: Use `Mock` scaffolding.
  //     value: std::rc::Rc::new(ast::Node {
  //       kind: ast::NodeKind::Literal(ast::Literal::Bool(true)),
  //       cached_type: None,
  //       id: 0,
  //     }),
  //     cache_id: 0,
  //     is_const_expr: false,
  //   };

  //   // TODO: Use the empty array type test.
  //   // TODO: Also, create a second test for inferring of parameter types.

  //   binding_stmt.report_constraints(&mut type_context, &cache);
  //   type_context.solve_constraints();
  //   binding_stmt.post_unification(&mut type_context, &cache);

  //   assert_eq!(
  //     binding_stmt.type_hint,
  //     Some(ast::Type::Basic(ast::BasicType::Bool))
  //   );
  // }

  // TODO: Add tests for `compare()`, `infer_and_flatten_type()`, `flatten_type()`, and others.
}
