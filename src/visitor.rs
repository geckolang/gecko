use crate::ast;

macro_rules! define_visitor {
  (@ $name:ident $(<$lt:lifetime>)?, $return_type:ty, $default_value:expr) => {
    pub trait $name $(<$lt>)? {
      fn dispatch(&mut self, node: &ast::NodeKind) -> $return_type {
        self.before_dispatch(node);

        match &node {
          ast::NodeKind::Literal(literal) => self.visit_literal(&literal),
          ast::NodeKind::ExternFunction(extern_fn) => self.visit_extern_function(&extern_fn),
          ast::NodeKind::ExternStatic(extern_static) => self.visit_extern_static(&extern_static),
          ast::NodeKind::Function(function) => self.enter_function(&function),
          ast::NodeKind::BlockExpr(block_expr) => self.enter_block_expr(&block_expr),
          ast::NodeKind::ReturnStmt(return_stmt) => self.visit_return_stmt(&return_stmt),
          ast::NodeKind::BindingStmt(binding_stmt) => self.visit_binding_stmt(&binding_stmt),
          ast::NodeKind::IfExpr(if_expr) => self.visit_if_expr(&if_expr),
          ast::NodeKind::CallExpr(call_expr) => self.visit_call_expr(&call_expr),
          ast::NodeKind::IntrinsicCall(intrinsic_call) => self.visit_intrinsic_call(&intrinsic_call),
          ast::NodeKind::InlineExprStmt(inline_expr_stmt) => self.visit_inline_expr_stmt(&inline_expr_stmt),
          ast::NodeKind::Reference(reference) => self.visit_reference(&reference),
          ast::NodeKind::BinaryExpr(binary_expr) => self.visit_binary_expr(&binary_expr),
          ast::NodeKind::UnaryExpr(unary_expr) => self.visit_unary_expr(&unary_expr),
          ast::NodeKind::Parameter(parameter) => self.visit_parameter(&parameter),
          ast::NodeKind::UnsafeExpr(unsafe_expr) => self.enter_unsafe_expr(&unsafe_expr),
          ast::NodeKind::Array(array) => self.visit_array(&array),
          ast::NodeKind::IndexingExpr(indexing_expr) => self.visit_indexing_expr(&indexing_expr),
          ast::NodeKind::Enum(enum_) => self.visit_enum(&enum_),
          ast::NodeKind::Struct(struct_) => self.visit_struct(&struct_),
          ast::NodeKind::Signature(signature) => self.visit_signature(&signature),
          ast::NodeKind::StructValue(struct_value) => self.visit_struct_value(&struct_value),
          ast::NodeKind::Pattern(pattern) => self.visit_pattern(&pattern),
          ast::NodeKind::TypeAlias(type_alias) => self.visit_type_alias(&type_alias),
          ast::NodeKind::Closure(closure) => self.visit_closure(&closure),
          ast::NodeKind::MemberAccess(member_access) => self.visit_member_access(&member_access),
          ast::NodeKind::StructImpl(struct_impl) => self.enter_struct_impl(&struct_impl),
          ast::NodeKind::Trait(trait_) => self.visit_trait(&trait_),
          ast::NodeKind::ParenthesesExpr(parentheses_expr) => self.visit_parentheses_expr(&parentheses_expr),
          ast::NodeKind::Import(import) => self.visit_import(&import),
          ast::NodeKind::SizeofIntrinsic(sizeof_intrinsic) => self.visit_sizeof_intrinsic(&sizeof_intrinsic),
          ast::NodeKind::Range(range) => self.visit_range(&range),
          ast::NodeKind::Type(ty) => self.visit_type(&ty),
          ast::NodeKind::CastExpr(cast_expr) => self.visit_cast_expr(&cast_expr)
        }
      }

      fn before_dispatch(&mut self, _node: &ast::NodeKind) {
        //
      }

      fn visit_literal(&mut self, _literal: &ast::Literal) -> $return_type {
        $default_value
      }

      fn visit_extern_function(
        &mut self,
        _extern_fn: &ast::ExternFunction
      ) -> $return_type {
        $default_value
      }

      fn visit_signature(
        &mut self,
        _signature: &ast::Signature
      ) -> $return_type {
        $default_value
      }

      fn visit_extern_static(
        &mut self,
        _extern_static: &ast::ExternStatic
      ) -> $return_type {
        $default_value
      }

      fn visit_inline_expr_stmt(
        &mut self,
        _inline_expr: &ast::InlineExprStmt
      ) -> $return_type {
        $default_value
      }

      fn enter_block_expr(&mut self, _block: &ast::BlockExpr) -> $return_type {
        $default_value
      }

      fn exit_block_expr(&mut self, _block: &ast::BlockExpr) -> $return_type {
        $default_value
      }

      fn visit_closure(&mut self, _closure: &ast::Closure) -> $return_type {
        $default_value
      }

      fn visit_enum(&mut self, _enum: &ast::Enum) -> $return_type {
        $default_value
      }

      fn visit_parameter(
        &mut self,
        _parameter: &ast::Parameter
      ) -> $return_type {
        $default_value
      }

      fn visit_intrinsic_call(
        &mut self,
        _intrinsic_call: &ast::IntrinsicCall
      ) -> $return_type {
        $default_value
      }

      fn visit_reference(
        &mut self,
        _reference: &ast::Reference
      ) -> $return_type {
        $default_value
      }

      fn enter_struct_impl(
        &mut self,
        _struct_impl: &ast::StructImpl
      ) -> $return_type {
        $default_value
      }

      fn exit_struct_impl(
        &mut self,
        _struct_impl: &ast::StructImpl
      ) -> $return_type {
        $default_value
      }

      fn visit_unary_expr(
        &mut self,
        _unary_expr: &ast::UnaryExpr
      ) -> $return_type {
        $default_value
      }

      fn visit_struct(
        &mut self,
        _struct: &ast::Struct
      ) -> $return_type {
        $default_value
      }

      fn visit_pattern(&mut self, _pattern: &ast::Pattern) -> $return_type {
        $default_value
      }

      fn visit_import(&mut self, _using: &ast::Import) -> $return_type {
        $default_value
      }

      fn visit_type_alias(
        &mut self,
        _type_alias: &ast::TypeAlias
      ) -> $return_type {
        $default_value
      }

      fn visit_trait(&mut self, _trait: &ast::Trait) -> $return_type {
        $default_value
      }

      fn visit_sizeof_intrinsic(
        &mut self,
        _sizeof_intrinsic: &ast::SizeofIntrinsic
      ) -> $return_type {
        $default_value
      }

      fn visit_range(&mut self, _range: &ast::Range) -> $return_type {
        $default_value
      }

      fn visit_struct_value(
        &mut self,
        _struct_value: &ast::StructValue
      ) -> $return_type {
        $default_value
      }

      fn visit_array(
        &mut self,
        _array_value: &ast::Array
      ) -> $return_type {
        $default_value
      }

      fn enter_unsafe_expr(
        &mut self,
        _unsafe_expr: &ast::UnsafeExpr
      ) -> $return_type {
        $default_value
      }

      fn exit_unsafe_expr(
        &mut self,
        _unsafe_expr: &ast::UnsafeExpr
      ) -> $return_type {
        $default_value
      }

      fn visit_return_stmt(
        &mut self,
        _return_stmt: &ast::ReturnStmt
      ) -> $return_type {
        $default_value
      }

      fn visit_parentheses_expr(
        &mut self,
        _parentheses_expr: &ast::ParenthesesExpr
      ) -> $return_type {
        $default_value
      }

      fn visit_member_access(
        &mut self,
        _member_access: &ast::MemberAccess
      ) -> $return_type {
        $default_value
      }

      fn visit_binary_expr(
        &mut self,
        _binary_expr: &ast::BinaryExpr
      ) -> $return_type {
        $default_value
      }

      fn visit_binding_stmt(
        &mut self,
        _binding_stmt: &ast::BindingStmt
      ) -> $return_type {
        $default_value
      }

      fn visit_call_expr(&mut self, _call_expr: &ast::CallExpr) -> $return_type {
        $default_value
      }

      fn enter_function(&mut self, _function: &ast::Function) -> $return_type {
        $default_value
      }

      fn exit_function(&mut self, _function: &ast::Function) -> $return_type {
        $default_value
      }

      fn visit_if_expr(&mut self, _if_expr: &ast::IfExpr) -> $return_type {
        $default_value
      }

      fn visit_indexing_expr(
        &mut self,
        _indexing_expr: &ast::IndexingExpr
      ) -> $return_type {
        $default_value
      }

      fn visit_type(&mut self, _ty: &ast::Type) -> $return_type {
        $default_value
      }

      fn visit_cast_expr(&mut self, _cast_expr: &ast::CastExpr) -> $return_type {
        $default_value
      }
    }
  };

  ($name:ident < $lt:lifetime >, $return_type:ty, $default_value:expr) => {
    define_visitor!(@ $name<$lt>, $return_type, $default_value);
  };

  ($name:ident, $return_type:ty, $default_value:expr) => {
    define_visitor!(@ $name, $return_type, $default_value);
  };
}

define_visitor!(
  LoweringVisitor<'ctx>,
  Option<inkwell::values::BasicValueEnum<'ctx>>,
  None
);

define_visitor!(AnalysisVisitor, (), ());

fn traverse_block_expr(block_expr: &ast::BlockExpr, visitor: &mut impl AnalysisVisitor) {
  for statement in &block_expr.statements {
    traverse(statement, visitor);
  }

  // NOTE: Yields are not part of statements. They won't be visited twice.
  if let Some(yields) = &block_expr.yields {
    traverse(yields, visitor);
  }

  visitor.exit_block_expr(&block_expr);
}

// TODO: Accept `&std::rc::Rc<ast::Node>` instead.
pub fn traverse(node: &ast::NodeKind, visitor: &mut impl AnalysisVisitor) {
  visitor.dispatch(&node);

  // TODO: Simplify with the addition of the dispatch method.
  // BUG: Why are there some missing `exit_function`? And also parameters?
  match &node {
    ast::NodeKind::InlineExprStmt(inline_expr_stmt) => {
      traverse(&inline_expr_stmt.expr, visitor);
    }
    ast::NodeKind::BinaryExpr(binary_expr) => {
      traverse(&binary_expr.left_operand, visitor);
      traverse(&binary_expr.right_operand, visitor);
    }
    ast::NodeKind::BindingStmt(binding_stmt) => {
      traverse(&binding_stmt.value, visitor);
    }
    ast::NodeKind::BlockExpr(block_expr) => traverse_block_expr(block_expr, visitor),
    ast::NodeKind::CallExpr(call_expr) => {
      traverse(&call_expr.callee_expr, visitor);

      for argument in &call_expr.arguments {
        traverse(&argument, visitor);
      }
    }
    ast::NodeKind::Closure(closure) => {
      visitor.visit_signature(&closure.signature);

      for parameter in &closure.signature.parameters {
        visitor.visit_parameter(&parameter);
      }

      visitor.enter_block_expr(&closure.body);
      traverse_block_expr(&closure.body, visitor);

      // REVIEW: No `exit_closure`?
    }
    ast::NodeKind::Function(function) => {
      visitor.visit_signature(&function.signature);

      for parameter in &function.signature.parameters {
        visitor.visit_parameter(&parameter);
      }

      visitor.enter_block_expr(&function.body);
      traverse_block_expr(&function.body, visitor);
      visitor.exit_function(function);
    }
    ast::NodeKind::IfExpr(if_expr) => {
      traverse(&if_expr.condition, visitor);
      traverse(&if_expr.then_branch, visitor);

      for alternative_branches in &if_expr.alternative_branches {
        traverse(&alternative_branches.0, visitor);
        traverse(&alternative_branches.1, visitor);
      }

      if let Some(else_expr) = &if_expr.else_branch {
        traverse(&else_expr, visitor);
      }
    }
    ast::NodeKind::IndexingExpr(indexing_expr) => {
      traverse(&indexing_expr.target_expr, visitor);
      traverse(&indexing_expr.index_expr, visitor);
    }
    ast::NodeKind::MemberAccess(member_expr) => {
      traverse(&member_expr.base_expr, visitor);
    }
    ast::NodeKind::ParenthesesExpr(parentheses_expr) => {
      traverse(&parentheses_expr.0, visitor);
    }
    ast::NodeKind::Signature(signature) => {
      for parameter in &signature.parameters {
        visitor.visit_parameter(&parameter);
      }
    }
    ast::NodeKind::ReturnStmt(return_expr) => {
      if let Some(return_value) = &return_expr.value {
        traverse(&return_value, visitor);
      }
    }
    ast::NodeKind::IntrinsicCall(intrinsic_call) => {
      for argument in &intrinsic_call.arguments {
        traverse(&argument, visitor);
      }
    }
    ast::NodeKind::StructImpl(struct_impl) => {
      for static_method in &struct_impl.static_methods {
        visitor.enter_function(static_method);
      }

      for member_method in &struct_impl.member_methods {
        visitor.enter_function(member_method);
      }

      visitor.exit_struct_impl(struct_impl);
    }
    ast::NodeKind::UnaryExpr(unary_expr) => {
      traverse(&unary_expr.operand, visitor);
    }
    ast::NodeKind::UnsafeExpr(unsafe_expr) => {
      traverse(&unsafe_expr.0, visitor);
      visitor.exit_unsafe_expr(unsafe_expr);
    }
    ast::NodeKind::Array(array) => {
      for element in &array.elements {
        traverse(&element, visitor);
      }
    }
    ast::NodeKind::StructValue(struct_value) => {
      for field in &struct_value.fields {
        traverse(&field, visitor);
      }
    }
    ast::NodeKind::Range(range) => {
      visitor.visit_literal(&range.start);
      visitor.visit_literal(&range.end);
    }
    ast::NodeKind::Reference(reference) => {
      visitor.visit_pattern(&reference.pattern);
    }
    ast::NodeKind::CastExpr(cast_expr) => {
      traverse(&cast_expr.operand, visitor);
    }
    ast::NodeKind::Enum(_)
    | ast::NodeKind::ExternFunction(_)
    | ast::NodeKind::ExternStatic(_)
    | ast::NodeKind::Literal(_)
    | ast::NodeKind::Parameter(_)
    | ast::NodeKind::Struct(_)
    | ast::NodeKind::Pattern(_)
    | ast::NodeKind::Import(_)
    | ast::NodeKind::SizeofIntrinsic(_)
    | ast::NodeKind::Trait(_)
    | ast::NodeKind::TypeAlias(_)
    | ast::NodeKind::Type(_) => {
      //
    }
  };
}
