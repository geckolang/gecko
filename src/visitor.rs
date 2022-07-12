use crate::ast;

macro_rules! define_visitor {
  (@ $name:ident $(<$lt:lifetime>)?, $return_type:ty, $default_value:expr) => {
    pub trait $name $(<$lt>)? {
      fn visit_literal(&mut self, _literal: &ast::Literal, _node: std::rc::Rc<ast::Node>) -> $return_type {
        $default_value
      }

      fn visit_extern_function(
        &mut self,
        _extern_fn: &ast::ExternFunction,
        _node: std::rc::Rc<ast::Node>,
      ) -> $return_type {
        $default_value
      }

      fn visit_prototype(
        &mut self,
        _prototype: &ast::Prototype,
        _node: std::rc::Rc<ast::Node>,
      ) -> $return_type {
        $default_value
      }

      fn visit_extern_static(
        &mut self,
        _extern_static: &ast::ExternStatic,
        _node: std::rc::Rc<ast::Node>,
      ) -> $return_type {
        $default_value
      }

      fn visit_inline_expr_stmt(
        &mut self,
        _inline_expr: &ast::InlineExprStmt,
        _node: std::rc::Rc<ast::Node>,
      ) -> $return_type {
        $default_value
      }

      fn enter_block_expr(&mut self, _block: &ast::BlockExpr, _node: std::rc::Rc<ast::Node>) -> $return_type {
        $default_value
      }

      fn exit_block_expr(&mut self, _block: &ast::BlockExpr, _node: std::rc::Rc<ast::Node>) -> $return_type {
        $default_value
      }

      fn visit_closure(&mut self, _closure: &ast::Closure, _node: std::rc::Rc<ast::Node>) -> $return_type {
        $default_value
      }

      fn visit_enum(&mut self, _enum: &ast::Enum, _node: std::rc::Rc<ast::Node>) -> $return_type {
        $default_value
      }

      fn visit_parameter(
        &mut self,
        _parameter: &ast::Parameter,
        _node: std::rc::Rc<ast::Node>,
      ) -> $return_type {
        $default_value
      }

      fn visit_intrinsic_call(
        &mut self,
        _intrinsic_call: &ast::IntrinsicCall,
        _node: std::rc::Rc<ast::Node>,
      ) -> $return_type {
        $default_value
      }

      fn visit_reference(
        &mut self,
        _reference: &ast::Reference,
        _node: std::rc::Rc<ast::Node>,
      ) -> $return_type {
        $default_value
      }

      fn enter_struct_impl(
        &mut self,
        _struct_impl: &ast::StructImpl,
        _node: std::rc::Rc<ast::Node>,
      ) -> $return_type {
        $default_value
      }

      fn exit_struct_impl(
        &mut self,
        _struct_impl: &ast::StructImpl,
        _node: std::rc::Rc<ast::Node>,
      ) -> $return_type {
        $default_value
      }

      fn visit_unary_expr(
        &mut self,
        _unary_expr: &ast::UnaryExpr,
        _node: std::rc::Rc<ast::Node>,
      ) -> $return_type {
        $default_value
      }

      fn visit_struct_type(
        &mut self,
        _struct_type: &ast::StructType,
        _node: std::rc::Rc<ast::Node>,
      ) -> $return_type {
        $default_value
      }

      fn visit_pattern(&mut self, _pattern: &ast::Pattern, _node: std::rc::Rc<ast::Node>) -> $return_type {
        $default_value
      }

      fn visit_using(&mut self, _using: &ast::Using, _node: std::rc::Rc<ast::Node>) -> $return_type {
        $default_value
      }

      fn visit_type_alias(
        &mut self,
        _type_alias: &ast::TypeAlias,
        _node: std::rc::Rc<ast::Node>,
      ) -> $return_type {
        $default_value
      }

      fn visit_trait(&mut self, _trait: &ast::Trait, _node: std::rc::Rc<ast::Node>) -> $return_type {
        $default_value
      }

      fn visit_sizeof_intrinsic(
        &mut self,
        _sizeof_intrinsic: &ast::SizeofIntrinsic,
        _node: std::rc::Rc<ast::Node>,
      ) -> $return_type {
        $default_value
      }

      fn visit_range(&mut self, _range: &ast::Range, _node: std::rc::Rc<ast::Node>) -> $return_type {
        $default_value
      }

      fn visit_struct_value(
        &mut self,
        _struct_value: &ast::StructValue,
        _node: std::rc::Rc<ast::Node>,
      ) -> $return_type {
        $default_value
      }

      fn visit_static_array_value(
        &mut self,
        _array_value: &ast::StaticArrayValue,
        _node: std::rc::Rc<ast::Node>,
      ) -> $return_type {
        $default_value
      }

      fn enter_unsafe_expr(
        &mut self,
        _unsafe_expr: &ast::UnsafeExpr,
        _node: std::rc::Rc<ast::Node>,
      ) -> $return_type {
        $default_value
      }

      fn exit_unsafe_expr(
        &mut self,
        _unsafe_expr: &ast::UnsafeExpr,
        _node: std::rc::Rc<ast::Node>,
      ) -> $return_type {
        $default_value
      }

      fn visit_return_stmt(
        &mut self,
        _return_stmt: &ast::ReturnStmt,
        _node: std::rc::Rc<ast::Node>,
      ) -> $return_type {
        $default_value
      }

      fn visit_parentheses_expr(
        &mut self,
        _parentheses_expr: &ast::ParenthesesExpr,
        _node: std::rc::Rc<ast::Node>,
      ) -> $return_type {
        $default_value
      }

      fn visit_member_access(
        &mut self,
        _member_access: &ast::MemberAccess,
        _node: std::rc::Rc<ast::Node>,
      ) -> $return_type {
        $default_value
      }

      fn visit_binary_expr(
        &mut self,
        _binary_expr: &ast::BinaryExpr,
        _node: std::rc::Rc<ast::Node>,
      ) -> $return_type {
        $default_value
      }

      fn visit_binding_stmt(
        &mut self,
        _binding_stmt: &ast::BindingStmt,
        _node: std::rc::Rc<ast::Node>,
      ) -> $return_type {
        $default_value
      }

      fn visit_call_expr(&mut self, _call_expr: &ast::CallExpr, _node: std::rc::Rc<ast::Node>) -> $return_type {
        $default_value
      }

      fn enter_function(&mut self, _function: &ast::Function, _node: std::rc::Rc<ast::Node>) -> $return_type {
        $default_value
      }

      fn exit_function(&mut self, _function: &ast::Function, _node: std::rc::Rc<ast::Node>) -> $return_type {
        $default_value
      }

      fn visit_if_expr(&mut self, _if_expr: &ast::IfExpr, _node: std::rc::Rc<ast::Node>) -> $return_type {
        $default_value
      }

      fn visit_indexing_expr(
        &mut self,
        _indexing_expr: &ast::IndexingExpr,
        _node: std::rc::Rc<ast::Node>,
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

fn traverse(node: std::rc::Rc<ast::Node>, visitor: &mut impl AnalysisVisitor) {
  match &node.kind {
    ast::NodeKind::InlineExprStmt(inline_expr_stmt) => {
      visitor.visit_inline_expr_stmt(inline_expr_stmt, std::rc::Rc::clone(&node));
      traverse(std::rc::Rc::clone(&inline_expr_stmt.expr), visitor);
    }
    ast::NodeKind::BinaryExpr(binary_expr) => {
      visitor.visit_binary_expr(binary_expr, std::rc::Rc::clone(&node));
      traverse(std::rc::Rc::clone(&binary_expr.left), visitor);
      traverse(std::rc::Rc::clone(&binary_expr.right), visitor);
    }
    ast::NodeKind::BindingStmt(binding_stmt) => {
      visitor.visit_binding_stmt(binding_stmt, std::rc::Rc::clone(&node));
      traverse(std::rc::Rc::clone(&binding_stmt.value), visitor);
    }
    ast::NodeKind::BlockExpr(block_expr) => {
      visitor.enter_block_expr(block_expr, std::rc::Rc::clone(&node));

      for statement in &block_expr.statements {
        traverse(std::rc::Rc::clone(&statement), visitor);
      }

      visitor.exit_block_expr(block_expr, std::rc::Rc::clone(&node));
    }
    ast::NodeKind::CallExpr(call_expr) => {
      visitor.visit_call_expr(call_expr, std::rc::Rc::clone(&node));
      traverse(std::rc::Rc::clone(&call_expr.callee_expr), visitor);

      for argument in &call_expr.arguments {
        traverse(std::rc::Rc::clone(&argument), visitor);
      }
    }
    ast::NodeKind::Closure(closure) => {
      visitor.visit_closure(closure, std::rc::Rc::clone(&node));
      visitor.visit_prototype(&closure.prototype, std::rc::Rc::clone(&node));
      visitor.enter_block_expr(&closure.body, std::rc::Rc::clone(&node));
      visitor.exit_block_expr(&closure.body, std::rc::Rc::clone(&node));
    }
    ast::NodeKind::Enum(enum_) => {
      visitor.visit_enum(enum_, std::rc::Rc::clone(&node));
    }
    ast::NodeKind::ExternFunction(extern_function) => {
      visitor.visit_extern_function(extern_function, std::rc::Rc::clone(&node));
    }
    ast::NodeKind::ExternStatic(extern_static) => {
      visitor.visit_extern_static(extern_static, std::rc::Rc::clone(&node));
    }
    ast::NodeKind::Function(function) => {
      visitor.enter_function(function, std::rc::Rc::clone(&node));
      visitor.visit_prototype(&function.prototype, std::rc::Rc::clone(&node));
      visitor.enter_block_expr(&function.body, std::rc::Rc::clone(&node));
      visitor.exit_block_expr(&function.body, std::rc::Rc::clone(&node));
    }
    ast::NodeKind::IfExpr(if_expr) => {
      visitor.visit_if_expr(if_expr, std::rc::Rc::clone(&node));
      traverse(std::rc::Rc::clone(&if_expr.condition), visitor);
      traverse(std::rc::Rc::clone(&if_expr.then_value), visitor);

      for alternative_branches in &if_expr.alternative_branches {
        traverse(std::rc::Rc::clone(&alternative_branches.0), visitor);
        traverse(std::rc::Rc::clone(&alternative_branches.1), visitor);
      }

      if let Some(else_expr) = &if_expr.else_value {
        traverse(std::rc::Rc::clone(&else_expr), visitor);
      }
    }
    ast::NodeKind::IndexingExpr(indexing_expr) => {
      traverse(std::rc::Rc::clone(&indexing_expr.index_expr), visitor);
    }
    ast::NodeKind::Literal(literal) => {
      visitor.visit_literal(literal, std::rc::Rc::clone(&node));
    }
    ast::NodeKind::MemberAccess(member_expr) => {
      visitor.visit_member_access(member_expr, std::rc::Rc::clone(&node));
      traverse(std::rc::Rc::clone(&member_expr.base_expr), visitor);
    }
    ast::NodeKind::Parameter(parameter) => {
      visitor.visit_parameter(parameter, std::rc::Rc::clone(&node));
    }
    ast::NodeKind::ParenthesesExpr(parentheses_expr) => {
      visitor.visit_parentheses_expr(parentheses_expr, std::rc::Rc::clone(&node));
      traverse(std::rc::Rc::clone(&parentheses_expr.0), visitor);
    }
    ast::NodeKind::Prototype(prototype) => {
      visitor.visit_prototype(prototype, std::rc::Rc::clone(&node));

      for parameter in &prototype.parameters {
        visitor.visit_parameter(parameter, std::rc::Rc::clone(&node));
      }
    }
    ast::NodeKind::ReturnStmt(return_expr) => {
      visitor.visit_return_stmt(return_expr, std::rc::Rc::clone(&node));

      if let Some(return_value) = &return_expr.value {
        traverse(std::rc::Rc::clone(&return_value), visitor);
      }
    }
    ast::NodeKind::IntrinsicCall(intrinsic_call) => {
      visitor.visit_intrinsic_call(intrinsic_call, std::rc::Rc::clone(&node));

      for argument in &intrinsic_call.arguments {
        traverse(std::rc::Rc::clone(&argument), visitor);
      }
    }
    ast::NodeKind::Reference(reference) => {
      visitor.visit_reference(reference, std::rc::Rc::clone(&node));
    }
    ast::NodeKind::StructImpl(struct_impl) => {
      visitor.enter_struct_impl(struct_impl, std::rc::Rc::clone(&node));

      for static_method in &struct_impl.static_methods {
        visitor.enter_function(static_method, std::rc::Rc::clone(&node));
      }

      for member_method in &struct_impl.member_methods {
        visitor.enter_function(member_method, std::rc::Rc::clone(&node));
      }

      visitor.exit_struct_impl(struct_impl, std::rc::Rc::clone(&node));
    }
    ast::NodeKind::UnaryExpr(unary_expr) => {
      visitor.visit_unary_expr(unary_expr, std::rc::Rc::clone(&node));
      traverse(std::rc::Rc::clone(&unary_expr.expr), visitor);
    }
    ast::NodeKind::UnsafeExpr(unsafe_expr) => {
      visitor.enter_unsafe_expr(unsafe_expr, std::rc::Rc::clone(&node));
      traverse(std::rc::Rc::clone(&unsafe_expr.0), visitor);
      visitor.exit_unsafe_expr(unsafe_expr, std::rc::Rc::clone(&node));
    }
    ast::NodeKind::StaticArrayValue(static_array_value) => {
      visitor.visit_static_array_value(static_array_value, std::rc::Rc::clone(&node));

      for element in &static_array_value.elements {
        traverse(std::rc::Rc::clone(&element), visitor);
      }
    }
    ast::NodeKind::StructType(struct_type) => {
      visitor.visit_struct_type(struct_type, std::rc::Rc::clone(&node));
    }
    ast::NodeKind::StructValue(struct_value) => {
      visitor.visit_struct_value(struct_value, std::rc::Rc::clone(&node));

      for field in &struct_value.fields {
        traverse(std::rc::Rc::clone(&field), visitor);
      }
    }
    ast::NodeKind::Pattern(pattern) => {
      visitor.visit_pattern(pattern, std::rc::Rc::clone(&node));
    }
    ast::NodeKind::Using(using) => {
      visitor.visit_using(using, std::rc::Rc::clone(&node));
    }
    ast::NodeKind::TypeAlias(type_alias) => {
      visitor.visit_type_alias(type_alias, std::rc::Rc::clone(&node));
    }
    ast::NodeKind::Trait(trait_) => {
      visitor.visit_trait(trait_, std::rc::Rc::clone(&node));
    }
    ast::NodeKind::SizeofIntrinsic(sizeof_intrinsic) => {
      visitor.visit_sizeof_intrinsic(sizeof_intrinsic, std::rc::Rc::clone(&node));
    }
    ast::NodeKind::Range(range) => {
      visitor.visit_range(range, std::rc::Rc::clone(&node));
      traverse(std::rc::Rc::clone(&range.start), visitor);
      traverse(std::rc::Rc::clone(&range.end), visitor);
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
  fn visit_literal(&mut self, literal: &ast::Literal, node: std::rc::Rc<ast::Node>) {
    for visitor in &mut self.visitors {
      visitor.visit_literal(literal, std::rc::Rc::clone(&node));
    }
  }

  fn visit_extern_function(
    &mut self,
    extern_fn: &ast::ExternFunction,
    node: std::rc::Rc<ast::Node>,
  ) {
    for visitor in &mut self.visitors {
      visitor.visit_extern_function(extern_fn, std::rc::Rc::clone(&node));
    }
  }

  fn visit_prototype(&mut self, prototype: &ast::Prototype, node: std::rc::Rc<ast::Node>) {
    for visitor in &mut self.visitors {
      visitor.visit_prototype(prototype, std::rc::Rc::clone(&node));
    }
  }

  fn visit_extern_static(
    &mut self,
    extern_static: &ast::ExternStatic,
    node: std::rc::Rc<ast::Node>,
  ) {
    for visitor in &mut self.visitors {
      visitor.visit_extern_static(extern_static, std::rc::Rc::clone(&node));
    }
  }

  fn visit_inline_expr_stmt(
    &mut self,
    inline_expr: &ast::InlineExprStmt,
    node: std::rc::Rc<ast::Node>,
  ) {
    for visitor in &mut self.visitors {
      visitor.visit_inline_expr_stmt(inline_expr, std::rc::Rc::clone(&node));
    }
  }

  fn enter_block_expr(&mut self, block: &ast::BlockExpr, node: std::rc::Rc<ast::Node>) {
    for visitor in &mut self.visitors {
      visitor.enter_block_expr(block, std::rc::Rc::clone(&node));
      visitor.exit_block_expr(block, std::rc::Rc::clone(&node));
    }
  }

  fn visit_closure(&mut self, closure: &ast::Closure, node: std::rc::Rc<ast::Node>) {
    for visitor in &mut self.visitors {
      visitor.visit_closure(closure, std::rc::Rc::clone(&node));
    }
  }

  fn visit_enum(&mut self, enum_: &ast::Enum, node: std::rc::Rc<ast::Node>) {
    for visitor in &mut self.visitors {
      visitor.visit_enum(enum_, std::rc::Rc::clone(&node));
    }
  }

  fn visit_parameter(&mut self, parameter: &ast::Parameter, node: std::rc::Rc<ast::Node>) {
    for visitor in &mut self.visitors {
      visitor.visit_parameter(parameter, std::rc::Rc::clone(&node));
    }
  }

  fn visit_intrinsic_call(
    &mut self,
    intrinsic_call: &ast::IntrinsicCall,
    node: std::rc::Rc<ast::Node>,
  ) {
    for visitor in &mut self.visitors {
      visitor.visit_intrinsic_call(intrinsic_call, std::rc::Rc::clone(&node));
    }
  }

  fn visit_reference(&mut self, reference: &ast::Reference, node: std::rc::Rc<ast::Node>) {
    for visitor in &mut self.visitors {
      visitor.visit_reference(reference, std::rc::Rc::clone(&node));
    }
  }

  fn enter_struct_impl(&mut self, struct_impl: &ast::StructImpl, node: std::rc::Rc<ast::Node>) {
    for visitor in &mut self.visitors {
      visitor.enter_struct_impl(struct_impl, std::rc::Rc::clone(&node));
    }
  }

  fn exit_struct_impl(&mut self, struct_impl: &ast::StructImpl, node: std::rc::Rc<ast::Node>) {
    for visitor in &mut self.visitors {
      visitor.exit_struct_impl(struct_impl, std::rc::Rc::clone(&node));
    }
  }

  fn visit_unary_expr(&mut self, unary_expr: &ast::UnaryExpr, node: std::rc::Rc<ast::Node>) {
    for visitor in &mut self.visitors {
      visitor.visit_unary_expr(unary_expr, std::rc::Rc::clone(&node));
    }
  }

  fn visit_struct_type(&mut self, struct_type: &ast::StructType, node: std::rc::Rc<ast::Node>) {
    for visitor in &mut self.visitors {
      visitor.visit_struct_type(struct_type, std::rc::Rc::clone(&node));
    }
  }

  fn visit_pattern(&mut self, pattern: &ast::Pattern, node: std::rc::Rc<ast::Node>) {
    for visitor in &mut self.visitors {
      visitor.visit_pattern(pattern, std::rc::Rc::clone(&node));
    }
  }

  fn visit_using(&mut self, using: &ast::Using, node: std::rc::Rc<ast::Node>) {
    for visitor in &mut self.visitors {
      visitor.visit_using(using, std::rc::Rc::clone(&node));
    }
  }

  fn visit_type_alias(&mut self, type_alias: &ast::TypeAlias, node: std::rc::Rc<ast::Node>) {
    for visitor in &mut self.visitors {
      visitor.visit_type_alias(type_alias, std::rc::Rc::clone(&node));
    }
  }

  fn visit_trait(&mut self, trait_: &ast::Trait, node: std::rc::Rc<ast::Node>) {
    for visitor in &mut self.visitors {
      visitor.visit_trait(trait_, std::rc::Rc::clone(&node));
    }
  }

  fn visit_sizeof_intrinsic(
    &mut self,
    sizeof_intrinsic: &ast::SizeofIntrinsic,
    node: std::rc::Rc<ast::Node>,
  ) {
    for visitor in &mut self.visitors {
      visitor.visit_sizeof_intrinsic(sizeof_intrinsic, std::rc::Rc::clone(&node));
    }
  }

  fn visit_range(&mut self, range: &ast::Range, node: std::rc::Rc<ast::Node>) {
    for visitor in &mut self.visitors {
      visitor.visit_range(range, std::rc::Rc::clone(&node));
    }
  }

  fn visit_struct_value(&mut self, struct_value: &ast::StructValue, node: std::rc::Rc<ast::Node>) {
    for visitor in &mut self.visitors {
      visitor.visit_struct_value(struct_value, std::rc::Rc::clone(&node));
    }
  }

  fn visit_static_array_value(
    &mut self,
    static_array_value: &ast::StaticArrayValue,
    node: std::rc::Rc<ast::Node>,
  ) {
    for visitor in &mut self.visitors {
      visitor.visit_static_array_value(static_array_value, std::rc::Rc::clone(&node));
    }
  }

  // TODO: Add missing visit methods.
}
