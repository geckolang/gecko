use crate::ast;

pub trait AnalysisVisitor {
  fn traverse(&mut self, node: &ast::NodeKind) {
    match &node {
      ast::NodeKind::InlineExprStmt(inline_expr_stmt) => {
        self.visit_inline_expr_stmt(inline_expr_stmt);
        self.traverse(&inline_expr_stmt.expr.kind);
      }
      ast::NodeKind::BinaryExpr(binary_expr) => {
        self.visit_binary_expr(binary_expr);
        self.traverse(&binary_expr.left.kind);
        self.traverse(&binary_expr.right.kind);
      }
      ast::NodeKind::BindingStmt(binding_stmt) => {
        self.visit_binding_stmt(binding_stmt);
        self.traverse(&binding_stmt.value.kind);
      }
      ast::NodeKind::BlockExpr(block_expr) => {
        self.visit_block_expr(block_expr);

        for statement in &block_expr.statements {
          self.traverse(&statement.kind);
        }
      }
      ast::NodeKind::CallExpr(call_expr) => {
        self.visit_call_expr(call_expr);
        self.traverse(&call_expr.callee_expr.kind);

        for arg in &call_expr.arguments {
          self.traverse(&arg.kind);
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
        self.visit_function(function);
        self.visit_prototype(&function.prototype);
        self.visit_block_expr(&function.body);
      }
      ast::NodeKind::IfExpr(if_expr) => {
        self.visit_if_expr(if_expr);
        self.traverse(&if_expr.condition.kind);
        self.traverse(&if_expr.then_expr.kind);

        for alternative_branches in &if_expr.alternative_branches {
          self.traverse(&alternative_branches.0.kind);
          self.traverse(&alternative_branches.1.kind);
        }

        if let Some(else_expr) = &if_expr.else_expr {
          self.traverse(&else_expr.kind);
        }
      }
      ast::NodeKind::IndexingExpr(indexing_expr) => {
        self.traverse(&indexing_expr.index_expr.kind);
      }
      ast::NodeKind::Literal(literal) => {
        self.visit_literal(literal);
      }
      ast::NodeKind::MemberAccess(member_expr) => {
        self.visit_member_access(member_expr);
        self.traverse(&member_expr.base_expr.kind);
      }
      ast::NodeKind::Parameter(parameter) => {
        self.visit_parameter(parameter);
      }
      ast::NodeKind::ParenthesesExpr(parentheses_expr) => {
        self.visit_parentheses_expr(parentheses_expr);
        self.traverse(&parentheses_expr.0.kind);
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
          self.traverse(&return_value.kind);
        }
      }
      ast::NodeKind::IntrinsicCall(intrinsic_call) => {
        self.visit_intrinsic_call(intrinsic_call);

        for argument in &intrinsic_call.arguments {
          self.traverse(&argument.kind);
        }
      }
      ast::NodeKind::Reference(reference) => {
        self.visit_reference(reference);
      }
      ast::NodeKind::StructImpl(struct_impl) => {
        self.enter_struct_impl(struct_impl);

        for static_method in &struct_impl.static_methods {
          self.visit_function(static_method);
        }

        for member_method in &struct_impl.member_methods {
          self.visit_function(member_method);
        }

        self.exit_struct_impl(struct_impl);
      }
      ast::NodeKind::UnaryExpr(unary_expr) => {
        self.visit_unary_expr(unary_expr);
        self.traverse(&unary_expr.expr.kind);
      }
      ast::NodeKind::UnsafeExpr(unsafe_expr) => {
        self.enter_unsafe_expr(unsafe_expr);
        self.traverse(&unsafe_expr.0.kind);
        self.exit_unsafe_expr(unsafe_expr);
      }
      ast::NodeKind::StaticArrayValue(static_array_value) => {
        self.visit_static_array_value(static_array_value);

        for element in &static_array_value.elements {
          self.traverse(&element.kind);
        }
      }
      ast::NodeKind::StructType(struct_type) => {
        self.visit_struct_type(struct_type);
      }
      ast::NodeKind::StructValue(struct_value) => {
        self.visit_struct_value(struct_value);

        for field in &struct_value.fields {
          self.traverse(&field.kind);
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
        self.traverse(&range.start.kind);
        self.traverse(&range.end.kind);
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

  fn enter_struct_impl(&mut self, _node: &ast::StructImpl) {
    //
  }

  fn exit_struct_impl(&mut self, _node: &ast::StructImpl) {
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

  fn enter_unsafe_expr(&mut self, _node: &ast::UnsafeExpr) {
    //
  }

  fn exit_unsafe_expr(&mut self, _node: &ast::UnsafeExpr) {
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

  fn visit_function(&mut self, _node: &ast::Function) {
    //
  }

  fn visit_if_expr(&mut self, _node: &ast::IfExpr) {
    //
  }

  fn visit_indexing_expr(&mut self, _node: &ast::IndexingExpr) {
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
  fn visit_literal(&mut self, node: &ast::Literal) {
    for visitor in &mut self.visitors {
      visitor.visit_literal(node);
    }
  }

  fn visit_extern_function(&mut self, node: &ast::ExternFunction) {
    for visitor in &mut self.visitors {
      visitor.visit_extern_function(node);
    }
  }

  fn visit_prototype(&mut self, node: &ast::Prototype) {
    for visitor in &mut self.visitors {
      visitor.visit_prototype(node);
    }
  }

  fn visit_extern_static(&mut self, node: &ast::ExternStatic) {
    for visitor in &mut self.visitors {
      visitor.visit_extern_static(node);
    }
  }

  fn visit_inline_expr_stmt(&mut self, node: &ast::InlineExprStmt) {
    for visitor in &mut self.visitors {
      visitor.visit_inline_expr_stmt(node);
    }
  }

  fn visit_block_expr(&mut self, node: &ast::BlockExpr) {
    for visitor in &mut self.visitors {
      visitor.visit_block_expr(node);
    }
  }

  fn visit_closure(&mut self, node: &ast::Closure) {
    for visitor in &mut self.visitors {
      visitor.visit_closure(node);
    }
  }

  fn visit_enum(&mut self, node: &ast::Enum) {
    for visitor in &mut self.visitors {
      visitor.visit_enum(node);
    }
  }

  fn visit_parameter(&mut self, node: &ast::Parameter) {
    for visitor in &mut self.visitors {
      visitor.visit_parameter(node);
    }
  }

  fn visit_intrinsic_call(&mut self, node: &ast::IntrinsicCall) {
    for visitor in &mut self.visitors {
      visitor.visit_intrinsic_call(node);
    }
  }

  fn visit_reference(&mut self, node: &ast::Reference) {
    for visitor in &mut self.visitors {
      visitor.visit_reference(node);
    }
  }

  fn enter_struct_impl(&mut self, node: &ast::StructImpl) {
    for visitor in &mut self.visitors {
      visitor.enter_struct_impl(node);
    }
  }

  fn exit_struct_impl(&mut self, node: &ast::StructImpl) {
    for visitor in &mut self.visitors {
      visitor.exit_struct_impl(node);
    }
  }

  fn visit_unary_expr(&mut self, node: &ast::UnaryExpr) {
    for visitor in &mut self.visitors {
      visitor.visit_unary_expr(node);
    }
  }

  fn visit_struct_type(&mut self, node: &ast::StructType) {
    for visitor in &mut self.visitors {
      visitor.visit_struct_type(node);
    }
  }

  fn visit_pattern(&mut self, node: &ast::Pattern) {
    for visitor in &mut self.visitors {
      visitor.visit_pattern(node);
    }
  }

  fn visit_using(&mut self, node: &ast::Using) {
    for visitor in &mut self.visitors {
      visitor.visit_using(node);
    }
  }

  fn visit_type_alias(&mut self, node: &ast::TypeAlias) {
    for visitor in &mut self.visitors {
      visitor.visit_type_alias(node);
    }
  }

  fn visit_trait(&mut self, node: &ast::Trait) {
    for visitor in &mut self.visitors {
      visitor.visit_trait(node);
    }
  }

  fn visit_sizeof_intrinsic(&mut self, node: &ast::SizeofIntrinsic) {
    for visitor in &mut self.visitors {
      visitor.visit_sizeof_intrinsic(node);
    }
  }

  fn visit_range(&mut self, node: &ast::Range) {
    for visitor in &mut self.visitors {
      visitor.visit_range(node);
    }
  }

  fn visit_struct_value(&mut self, node: &ast::StructValue) {
    for visitor in &mut self.visitors {
      visitor.visit_struct_value(node);
    }
  }

  fn visit_static_array_value(&mut self, node: &ast::StaticArrayValue) {
    for visitor in &mut self.visitors {
      visitor.visit_static_array_value(node);
    }
  }
}
