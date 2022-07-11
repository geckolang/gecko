use crate::ast;

macro_rules! define_visitor {
  (@ $name:ident $(<$lt:lifetime>)?, $return_type:ty, $default_value:expr) => {
    pub trait $name $(<$lt>)? {
      fn visit_literal(&mut self, _literal: &ast::Literal, _node: &ast::Node) -> $return_type {
        $default_value
      }

      fn visit_extern_function(
        &mut self,
        _extern_fn: &ast::ExternFunction,
        _node: &ast::Node,
      ) -> $return_type {
        $default_value
      }

      fn visit_prototype(
        &mut self,
        _prototype: &ast::Prototype,
        _node: &ast::Node,
      ) -> $return_type {
        $default_value
      }

      fn visit_extern_static(
        &mut self,
        _extern_static: &ast::ExternStatic,
        _node: &ast::Node,
      ) -> $return_type {
        $default_value
      }

      fn visit_inline_expr_stmt(
        &mut self,
        _inline_expr: &ast::InlineExprStmt,
        _node: &ast::Node,
      ) -> $return_type {
        $default_value
      }

      fn visit_block_expr(&mut self, _block: &ast::BlockExpr, _node: &ast::Node) -> $return_type {
        $default_value
      }

      fn visit_closure(&mut self, _closure: &ast::Closure, _node: &ast::Node) -> $return_type {
        $default_value
      }

      fn visit_enum(&mut self, _enum: &ast::Enum, _node: &ast::Node) -> $return_type {
        $default_value
      }

      fn visit_parameter(
        &mut self,
        _parameter: &ast::Parameter,
        _node: &ast::Node,
      ) -> $return_type {
        $default_value
      }

      fn visit_intrinsic_call(
        &mut self,
        _intrinsic_call: &ast::IntrinsicCall,
        _node: &ast::Node,
      ) -> $return_type {
        $default_value
      }

      fn visit_reference(
        &mut self,
        _reference: &ast::Reference,
        _node: &ast::Node,
      ) -> $return_type {
        $default_value
      }

      fn enter_struct_impl(
        &mut self,
        _struct_impl: &ast::StructImpl,
        _node: &ast::Node,
      ) -> $return_type {
        $default_value
      }

      fn exit_struct_impl(
        &mut self,
        _struct_impl: &ast::StructImpl,
        _node: &ast::Node,
      ) -> $return_type {
        $default_value
      }

      fn visit_unary_expr(
        &mut self,
        _unary_expr: &ast::UnaryExpr,
        _node: &ast::Node,
      ) -> $return_type {
        $default_value
      }

      fn visit_struct_type(
        &mut self,
        _struct_type: &ast::StructType,
        _node: &ast::Node,
      ) -> $return_type {
        $default_value
      }

      fn visit_pattern(&mut self, _pattern: &ast::Pattern, _node: &ast::Node) -> $return_type {
        $default_value
      }

      fn visit_using(&mut self, _using: &ast::Using, _node: &ast::Node) -> $return_type {
        $default_value
      }

      fn visit_type_alias(
        &mut self,
        _type_alias: &ast::TypeAlias,
        _node: &ast::Node,
      ) -> $return_type {
        $default_value
      }

      fn visit_trait(&mut self, _trait: &ast::Trait, _node: &ast::Node) -> $return_type {
        $default_value
      }

      fn visit_sizeof_intrinsic(
        &mut self,
        _sizeof_intrinsic: &ast::SizeofIntrinsic,
        _node: &ast::Node,
      ) -> $return_type {
        $default_value
      }

      fn visit_range(&mut self, _range: &ast::Range, _node: &ast::Node) -> $return_type {
        $default_value
      }

      fn visit_struct_value(
        &mut self,
        _struct_value: &ast::StructValue,
        _node: &ast::Node,
      ) -> $return_type {
        $default_value
      }

      fn visit_static_array_value(
        &mut self,
        _array_value: &ast::StaticArrayValue,
        _node: &ast::Node,
      ) -> $return_type {
        $default_value
      }

      fn enter_unsafe_expr(
        &mut self,
        _unsafe_expr: &ast::UnsafeExpr,
        _node: &ast::Node,
      ) -> $return_type {
        $default_value
      }

      fn exit_unsafe_expr(
        &mut self,
        _unsafe_expr: &ast::UnsafeExpr,
        _node: &ast::Node,
      ) -> $return_type {
        $default_value
      }

      fn visit_return_stmt(
        &mut self,
        _return_stmt: &ast::ReturnStmt,
        _node: &ast::Node,
      ) -> $return_type {
        $default_value
      }

      fn visit_parentheses_expr(
        &mut self,
        _parentheses_expr: &ast::ParenthesesExpr,
        _node: &ast::Node,
      ) -> $return_type {
        $default_value
      }

      fn visit_member_access(
        &mut self,
        _member_access: &ast::MemberAccess,
        _node: &ast::Node,
      ) -> $return_type {
        $default_value
      }

      fn visit_binary_expr(
        &mut self,
        _binary_expr: &ast::BinaryExpr,
        _node: &ast::Node,
      ) -> $return_type {
        $default_value
      }

      fn visit_binding_stmt(
        &mut self,
        _binding_stmt: &ast::BindingStmt,
        _node: &ast::Node,
      ) -> $return_type {
        $default_value
      }

      fn visit_call_expr(&mut self, _call_expr: &ast::CallExpr, _node: &ast::Node) -> $return_type {
        $default_value
      }

      fn visit_function(&mut self, _function: &ast::Function, _node: &ast::Node) -> $return_type {
        $default_value
      }

      fn visit_if_expr(&mut self, _if_expr: &ast::IfExpr, _node: &ast::Node) -> $return_type {
        $default_value
      }

      fn visit_indexing_expr(
        &mut self,
        _indexing_expr: &ast::IndexingExpr,
        _node: &ast::Node,
      ) -> $return_type {
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

trait Visitor {
  type VisitResult;
}

fn traverse(node: &ast::Node, visitor: &mut impl AnalysisVisitor) {
  match &node.kind {
    ast::NodeKind::InlineExprStmt(inline_expr_stmt) => {
      visitor.visit_inline_expr_stmt(inline_expr_stmt, node);
      traverse(&inline_expr_stmt.expr, visitor);
    }
    ast::NodeKind::BinaryExpr(binary_expr) => {
      visitor.visit_binary_expr(binary_expr, node);
      traverse(&binary_expr.left, visitor);
      traverse(&binary_expr.right, visitor);
    }
    ast::NodeKind::BindingStmt(binding_stmt) => {
      visitor.visit_binding_stmt(binding_stmt, node);
      traverse(&binding_stmt.value, visitor);
    }
    ast::NodeKind::BlockExpr(block_expr) => {
      visitor.visit_block_expr(block_expr, node);

      for statement in &block_expr.statements {
        traverse(&statement, visitor);
      }
    }
    ast::NodeKind::CallExpr(call_expr) => {
      visitor.visit_call_expr(call_expr, node);
      traverse(&call_expr.callee_expr, visitor);

      for argument in &call_expr.arguments {
        traverse(&argument, visitor);
      }
    }
    ast::NodeKind::Closure(closure) => {
      visitor.visit_closure(closure, node);
      visitor.visit_prototype(&closure.prototype, node);
      visitor.visit_block_expr(&closure.body, node);
    }
    ast::NodeKind::Enum(enum_) => {
      visitor.visit_enum(enum_, node);
    }
    ast::NodeKind::ExternFunction(extern_function) => {
      visitor.visit_extern_function(extern_function, node);
    }
    ast::NodeKind::ExternStatic(extern_static) => {
      visitor.visit_extern_static(extern_static, node);
    }
    ast::NodeKind::Function(function) => {
      visitor.visit_function(function, node);
      visitor.visit_prototype(&function.prototype, node);
      visitor.visit_block_expr(&function.body, node);
    }
    ast::NodeKind::IfExpr(if_expr) => {
      visitor.visit_if_expr(if_expr, node);
      traverse(&if_expr.condition, visitor);
      traverse(&if_expr.then_expr, visitor);

      for alternative_branches in &if_expr.alternative_branches {
        traverse(&alternative_branches.0, visitor);
        traverse(&alternative_branches.1, visitor);
      }

      if let Some(else_expr) = &if_expr.else_expr {
        traverse(&else_expr, visitor);
      }
    }
    ast::NodeKind::IndexingExpr(indexing_expr) => {
      traverse(&indexing_expr.index_expr, visitor);
    }
    ast::NodeKind::Literal(literal) => {
      visitor.visit_literal(literal, node);
    }
    ast::NodeKind::MemberAccess(member_expr) => {
      visitor.visit_member_access(member_expr, node);
      traverse(&member_expr.base_expr, visitor);
    }
    ast::NodeKind::Parameter(parameter) => {
      visitor.visit_parameter(parameter, node);
    }
    ast::NodeKind::ParenthesesExpr(parentheses_expr) => {
      visitor.visit_parentheses_expr(parentheses_expr, node);
      traverse(&parentheses_expr.0, visitor);
    }
    ast::NodeKind::Prototype(prototype) => {
      visitor.visit_prototype(prototype, node);

      for parameter in &prototype.parameters {
        visitor.visit_parameter(parameter, node);
      }
    }
    ast::NodeKind::ReturnStmt(return_expr) => {
      visitor.visit_return_stmt(return_expr, node);

      if let Some(return_value) = &return_expr.value {
        traverse(&return_value, visitor);
      }
    }
    ast::NodeKind::IntrinsicCall(intrinsic_call) => {
      visitor.visit_intrinsic_call(intrinsic_call, node);

      for argument in &intrinsic_call.arguments {
        traverse(&argument, visitor);
      }
    }
    ast::NodeKind::Reference(reference) => {
      visitor.visit_reference(reference, node);
    }
    ast::NodeKind::StructImpl(struct_impl) => {
      visitor.enter_struct_impl(struct_impl, node);

      for static_method in &struct_impl.static_methods {
        visitor.visit_function(static_method, node);
      }

      for member_method in &struct_impl.member_methods {
        visitor.visit_function(member_method, node);
      }

      visitor.exit_struct_impl(struct_impl, node);
    }
    ast::NodeKind::UnaryExpr(unary_expr) => {
      visitor.visit_unary_expr(unary_expr, node);
      traverse(&unary_expr.expr, visitor);
    }
    ast::NodeKind::UnsafeExpr(unsafe_expr) => {
      visitor.enter_unsafe_expr(unsafe_expr, node);
      traverse(&unsafe_expr.0, visitor);
      visitor.exit_unsafe_expr(unsafe_expr, node);
    }
    ast::NodeKind::StaticArrayValue(static_array_value) => {
      visitor.visit_static_array_value(static_array_value, node);

      for element in &static_array_value.elements {
        traverse(&element, visitor);
      }
    }
    ast::NodeKind::StructType(struct_type) => {
      visitor.visit_struct_type(struct_type, node);
    }
    ast::NodeKind::StructValue(struct_value) => {
      visitor.visit_struct_value(struct_value, node);

      for field in &struct_value.fields {
        traverse(&field, visitor);
      }
    }
    ast::NodeKind::Pattern(pattern) => {
      visitor.visit_pattern(pattern, node);
    }
    ast::NodeKind::Using(using) => {
      visitor.visit_using(using, node);
    }
    ast::NodeKind::TypeAlias(type_alias) => {
      visitor.visit_type_alias(type_alias, node);
    }
    ast::NodeKind::Trait(trait_) => {
      visitor.visit_trait(trait_, node);
    }
    ast::NodeKind::SizeofIntrinsic(sizeof_intrinsic) => {
      visitor.visit_sizeof_intrinsic(sizeof_intrinsic, node);
    }
    ast::NodeKind::Range(range) => {
      visitor.visit_range(range, node);
      traverse(&range.start, visitor);
      traverse(&range.end, visitor);
    }
  };
}

/// Used to perform various AST traversals at the same time, while
/// invoking all the registered visitors per node.
///
/// This is more performant than traversing the AST per visitor, since
/// with this visitor we can invoke multiple others at the cost of a single pass.
struct AggregateVisitor<'a> {
  visitors: Vec<&'a mut dyn AnalysisVisitor>,
}

impl<'a> AnalysisVisitor for AggregateVisitor<'a> {
  fn visit_literal(&mut self, literal: &ast::Literal, node: &ast::Node) {
    for visitor in &mut self.visitors {
      visitor.visit_literal(literal, node);
    }
  }

  fn visit_extern_function(&mut self, extern_fn: &ast::ExternFunction, node: &ast::Node) {
    for visitor in &mut self.visitors {
      visitor.visit_extern_function(extern_fn, node);
    }
  }

  fn visit_prototype(&mut self, prototype: &ast::Prototype, node: &ast::Node) {
    for visitor in &mut self.visitors {
      visitor.visit_prototype(prototype, node);
    }
  }

  fn visit_extern_static(&mut self, extern_static: &ast::ExternStatic, node: &ast::Node) {
    for visitor in &mut self.visitors {
      visitor.visit_extern_static(extern_static, node);
    }
  }

  fn visit_inline_expr_stmt(&mut self, inline_expr: &ast::InlineExprStmt, node: &ast::Node) {
    for visitor in &mut self.visitors {
      visitor.visit_inline_expr_stmt(inline_expr, node);
    }
  }

  fn visit_block_expr(&mut self, block: &ast::BlockExpr, node: &ast::Node) {
    for visitor in &mut self.visitors {
      visitor.visit_block_expr(block, node);
    }
  }

  fn visit_closure(&mut self, closure: &ast::Closure, node: &ast::Node) {
    for visitor in &mut self.visitors {
      visitor.visit_closure(closure, node);
    }
  }

  fn visit_enum(&mut self, enum_: &ast::Enum, node: &ast::Node) {
    for visitor in &mut self.visitors {
      visitor.visit_enum(enum_, node);
    }
  }

  fn visit_parameter(&mut self, parameter: &ast::Parameter, node: &ast::Node) {
    for visitor in &mut self.visitors {
      visitor.visit_parameter(parameter, node);
    }
  }

  fn visit_intrinsic_call(&mut self, intrinsic_call: &ast::IntrinsicCall, node: &ast::Node) {
    for visitor in &mut self.visitors {
      visitor.visit_intrinsic_call(intrinsic_call, node);
    }
  }

  fn visit_reference(&mut self, reference: &ast::Reference, node: &ast::Node) {
    for visitor in &mut self.visitors {
      visitor.visit_reference(reference, node);
    }
  }

  fn enter_struct_impl(&mut self, struct_impl: &ast::StructImpl, node: &ast::Node) {
    for visitor in &mut self.visitors {
      visitor.enter_struct_impl(struct_impl, node);
    }
  }

  fn exit_struct_impl(&mut self, struct_impl: &ast::StructImpl, node: &ast::Node) {
    for visitor in &mut self.visitors {
      visitor.exit_struct_impl(struct_impl, node);
    }
  }

  fn visit_unary_expr(&mut self, unary_expr: &ast::UnaryExpr, node: &ast::Node) {
    for visitor in &mut self.visitors {
      visitor.visit_unary_expr(unary_expr, node);
    }
  }

  fn visit_struct_type(&mut self, struct_type: &ast::StructType, node: &ast::Node) {
    for visitor in &mut self.visitors {
      visitor.visit_struct_type(struct_type, node);
    }
  }

  fn visit_pattern(&mut self, pattern: &ast::Pattern, node: &ast::Node) {
    for visitor in &mut self.visitors {
      visitor.visit_pattern(pattern, node);
    }
  }

  fn visit_using(&mut self, using: &ast::Using, node: &ast::Node) {
    for visitor in &mut self.visitors {
      visitor.visit_using(using, node);
    }
  }

  fn visit_type_alias(&mut self, type_alias: &ast::TypeAlias, node: &ast::Node) {
    for visitor in &mut self.visitors {
      visitor.visit_type_alias(type_alias, node);
    }
  }

  fn visit_trait(&mut self, trait_: &ast::Trait, node: &ast::Node) {
    for visitor in &mut self.visitors {
      visitor.visit_trait(trait_, node);
    }
  }

  fn visit_sizeof_intrinsic(&mut self, sizeof_intrinsic: &ast::SizeofIntrinsic, node: &ast::Node) {
    for visitor in &mut self.visitors {
      visitor.visit_sizeof_intrinsic(sizeof_intrinsic, node);
    }
  }

  fn visit_range(&mut self, range: &ast::Range, node: &ast::Node) {
    for visitor in &mut self.visitors {
      visitor.visit_range(range, node);
    }
  }

  fn visit_struct_value(&mut self, struct_value: &ast::StructValue, node: &ast::Node) {
    for visitor in &mut self.visitors {
      visitor.visit_struct_value(struct_value, node);
    }
  }

  fn visit_static_array_value(
    &mut self,
    static_array_value: &ast::StaticArrayValue,
    node: &ast::Node,
  ) {
    for visitor in &mut self.visitors {
      visitor.visit_static_array_value(static_array_value, node);
    }
  }
}
