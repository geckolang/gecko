use crate::ast;

pub trait AnalysisVisitor {
  fn traverse(&mut self, node: &ast::Node) {
    match &node.kind {
      ast::NodeKind::InlineExprStmt(inline_expr_stmt) => {
        self.visit_inline_expr_stmt(inline_expr_stmt, node);
        self.traverse(&inline_expr_stmt.expr);
      }
      ast::NodeKind::BinaryExpr(binary_expr) => {
        self.visit_binary_expr(binary_expr, node);
        self.traverse(&binary_expr.left);
        self.traverse(&binary_expr.right);
      }
      ast::NodeKind::BindingStmt(binding_stmt) => {
        self.visit_binding_stmt(binding_stmt, node);
        self.traverse(&binding_stmt.value);
      }
      ast::NodeKind::BlockExpr(block_expr) => {
        self.visit_block_expr(block_expr, node);

        for statement in &block_expr.statements {
          self.traverse(&statement);
        }
      }
      ast::NodeKind::CallExpr(call_expr) => {
        self.visit_call_expr(call_expr, node);
        self.traverse(&call_expr.callee_expr);

        for arg in &call_expr.arguments {
          self.traverse(&arg);
        }
      }
      ast::NodeKind::Closure(closure) => {
        self.visit_closure(closure, node);
        self.visit_prototype(&closure.prototype, node);
        self.visit_block_expr(&closure.body, node);
      }
      ast::NodeKind::Enum(enum_) => {
        self.visit_enum(enum_, node);
      }
      ast::NodeKind::ExternFunction(extern_function) => {
        self.visit_extern_function(extern_function, node);
      }
      ast::NodeKind::ExternStatic(extern_static) => {
        self.visit_extern_static(extern_static, node);
      }
      ast::NodeKind::Function(function) => {
        self.visit_function(function, node);
        self.visit_prototype(&function.prototype, node);
        self.visit_block_expr(&function.body, node);
      }
      ast::NodeKind::IfExpr(if_expr) => {
        self.visit_if_expr(if_expr, node);
        self.traverse(&if_expr.condition);
        self.traverse(&if_expr.then_expr);

        for alternative_branches in &if_expr.alternative_branches {
          self.traverse(&alternative_branches.0);
          self.traverse(&alternative_branches.1);
        }

        if let Some(else_expr) = &if_expr.else_expr {
          self.traverse(&else_expr);
        }
      }
      ast::NodeKind::IndexingExpr(indexing_expr) => {
        self.traverse(&indexing_expr.index_expr);
      }
      ast::NodeKind::Literal(literal) => {
        self.visit_literal(literal, node);
      }
      ast::NodeKind::MemberAccess(member_expr) => {
        self.visit_member_access(member_expr, node);
        self.traverse(&member_expr.base_expr);
      }
      ast::NodeKind::Parameter(parameter) => {
        self.visit_parameter(parameter, node);
      }
      ast::NodeKind::ParenthesesExpr(parentheses_expr) => {
        self.visit_parentheses_expr(parentheses_expr, node);
        self.traverse(&parentheses_expr.0);
      }
      ast::NodeKind::Prototype(prototype) => {
        self.visit_prototype(prototype, node);

        for parameter in &prototype.parameters {
          self.visit_parameter(parameter, node);
        }
      }
      ast::NodeKind::ReturnStmt(return_expr) => {
        self.visit_return_stmt(return_expr, node);

        if let Some(return_value) = &return_expr.value {
          self.traverse(&return_value);
        }
      }
      ast::NodeKind::IntrinsicCall(intrinsic_call) => {
        self.visit_intrinsic_call(intrinsic_call, node);

        for argument in &intrinsic_call.arguments {
          self.traverse(&argument);
        }
      }
      ast::NodeKind::Reference(reference) => {
        self.visit_reference(reference, node);
      }
      ast::NodeKind::StructImpl(struct_impl) => {
        self.enter_struct_impl(struct_impl, node);

        for static_method in &struct_impl.static_methods {
          self.visit_function(static_method, node);
        }

        for member_method in &struct_impl.member_methods {
          self.visit_function(member_method, node);
        }

        self.exit_struct_impl(struct_impl, node);
      }
      ast::NodeKind::UnaryExpr(unary_expr) => {
        self.visit_unary_expr(unary_expr, node);
        self.traverse(&unary_expr.expr);
      }
      ast::NodeKind::UnsafeExpr(unsafe_expr) => {
        self.enter_unsafe_expr(unsafe_expr, node);
        self.traverse(&unsafe_expr.0);
        self.exit_unsafe_expr(unsafe_expr, node);
      }
      ast::NodeKind::StaticArrayValue(static_array_value) => {
        self.visit_static_array_value(static_array_value, node);

        for element in &static_array_value.elements {
          self.traverse(&element);
        }
      }
      ast::NodeKind::StructType(struct_type) => {
        self.visit_struct_type(struct_type, node);
      }
      ast::NodeKind::StructValue(struct_value) => {
        self.visit_struct_value(struct_value, node);

        for field in &struct_value.fields {
          self.traverse(&field);
        }
      }
      ast::NodeKind::Pattern(pattern) => {
        self.visit_pattern(pattern, node);
      }
      ast::NodeKind::Using(using) => {
        self.visit_using(using, node);
      }
      ast::NodeKind::TypeAlias(type_alias) => {
        self.visit_type_alias(type_alias, node);
      }
      ast::NodeKind::Trait(trait_) => {
        self.visit_trait(trait_, node);
      }
      ast::NodeKind::SizeofIntrinsic(sizeof_intrinsic) => {
        self.visit_sizeof_intrinsic(sizeof_intrinsic, node);
      }
      ast::NodeKind::Range(range) => {
        self.visit_range(range, node);
        self.traverse(&range.start);
        self.traverse(&range.end);
      }
    };
  }

  fn visit_literal(&mut self, _literal: &ast::Literal, _node: &ast::Node) {
    //
  }

  fn visit_extern_function(&mut self, _extern_fn: &ast::ExternFunction, _node: &ast::Node) {
    //
  }

  fn visit_prototype(&mut self, _prototype: &ast::Prototype, _node: &ast::Node) {
    //
  }

  fn visit_extern_static(&mut self, _extern_static: &ast::ExternStatic, _node: &ast::Node) {
    //
  }

  fn visit_inline_expr_stmt(&mut self, _inline_expr: &ast::InlineExprStmt, _node: &ast::Node) {
    //
  }

  fn visit_block_expr(&mut self, _block: &ast::BlockExpr, _node: &ast::Node) {
    //
  }

  fn visit_closure(&mut self, _closure: &ast::Closure, _node: &ast::Node) {
    //
  }

  fn visit_enum(&mut self, _enum: &ast::Enum, _node: &ast::Node) {
    //
  }

  fn visit_parameter(&mut self, _parameter: &ast::Parameter, _node: &ast::Node) {
    //
  }

  fn visit_intrinsic_call(&mut self, _intrinsic_call: &ast::IntrinsicCall, _node: &ast::Node) {
    //
  }

  fn visit_reference(&mut self, _reference: &ast::Reference, _node: &ast::Node) {
    //
  }

  fn enter_struct_impl(&mut self, _struct_impl: &ast::StructImpl, _node: &ast::Node) {
    //
  }

  fn exit_struct_impl(&mut self, _struct_impl: &ast::StructImpl, _node: &ast::Node) {
    //
  }

  fn visit_unary_expr(&mut self, _unary_expr: &ast::UnaryExpr, _node: &ast::Node) {
    //
  }

  fn visit_struct_type(&mut self, _struct_type: &ast::StructType, _node: &ast::Node) {
    //
  }

  fn visit_pattern(&mut self, _pattern: &ast::Pattern, _node: &ast::Node) {
    //
  }

  fn visit_using(&mut self, _using: &ast::Using, _node: &ast::Node) {
    //
  }

  fn visit_type_alias(&mut self, _type_alias: &ast::TypeAlias, _node: &ast::Node) {
    //
  }

  fn visit_trait(&mut self, _trait: &ast::Trait, _node: &ast::Node) {
    //
  }

  fn visit_sizeof_intrinsic(
    &mut self,
    _sizeof_intrinsic: &ast::SizeofIntrinsic,
    _node: &ast::Node,
  ) {
    //
  }

  fn visit_range(&mut self, _range: &ast::Range, _node: &ast::Node) {
    //
  }

  fn visit_struct_value(&mut self, _struct_value: &ast::StructValue, _node: &ast::Node) {
    //
  }

  fn visit_static_array_value(&mut self, _array_value: &ast::StaticArrayValue, _node: &ast::Node) {
    //
  }

  fn enter_unsafe_expr(&mut self, _unsafe_expr: &ast::UnsafeExpr, _node: &ast::Node) {
    //
  }

  fn exit_unsafe_expr(&mut self, _unsafe_expr: &ast::UnsafeExpr, _node: &ast::Node) {
    //
  }

  fn visit_return_stmt(&mut self, _return_stmt: &ast::ReturnStmt, _node: &ast::Node) {
    //
  }

  fn visit_parentheses_expr(
    &mut self,
    _parentheses_expr: &ast::ParenthesesExpr,
    _node: &ast::Node,
  ) {
    //
  }

  fn visit_member_access(&mut self, _member_access: &ast::MemberAccess, _node: &ast::Node) {
    //
  }

  fn visit_binary_expr(&mut self, _binary_expr: &ast::BinaryExpr, _node: &ast::Node) {
    //
  }

  fn visit_binding_stmt(&mut self, _binding_stmt: &ast::BindingStmt, _node: &ast::Node) {
    //
  }

  fn visit_call_expr(&mut self, _call_expr: &ast::CallExpr, _node: &ast::Node) {
    //
  }

  fn visit_function(&mut self, _function: &ast::Function, _node: &ast::Node) {
    //
  }

  fn visit_if_expr(&mut self, _if_expr: &ast::IfExpr, _node: &ast::Node) {
    //
  }

  fn visit_indexing_expr(&mut self, _indexing_expr: &ast::IndexingExpr, _node: &ast::Node) {
    //
  }
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
