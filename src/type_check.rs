use crate::{ast, diagnostic, int_kind};

pub type TypeCheckResult = Option<Vec<diagnostic::Diagnostic>>;

fn find_blocks_of<'a>(statement: &'a ast::AnyStmtNode<'a>) -> Option<Vec<&'a ast::Block<'a>>> {
  // FIXME: This should be done recursively.
  Some(match statement {
    ast::AnyStmtNode::BlockStmt(block_stmt) => vec![&block_stmt.block],
    _ => return None,
  })
}

fn find_kind_of(expr: &ast::ExprHolder<'_>) -> Option<ast::KindHolder> {
  Some(match expr {
    ast::ExprHolder::BoolLiteral(_) => ast::KindHolder::BoolKind(int_kind::BoolKind),
    ast::ExprHolder::IntLiteral(int_literal) => ast::KindHolder::IntKind(int_literal.kind),
    ast::ExprHolder::CallExpr(call_expr) => {
      assert!(call_expr.callee_stub.value.is_some());

      let callee = call_expr.callee_stub.value.as_ref().unwrap();
      let callee_prototype = callee.get_prototype();

      if let Some(return_kind_group) = &callee_prototype.return_kind_group {
        // TODO: Cloning return kind.
        return_kind_group.kind.clone()
      } else {
        return None;
      }
    }
  })
}

pub fn type_check_module(_module: &mut ast::Module<'_>) -> TypeCheckResult {
  // let mut _diagnostics = Vec::new();

  todo!();

  // FIXME:
  // module.walk_children(&mut |child: &mut dyn Node| {
  //   if let Some(child_diagnostics) = child.type_check() {
  //     diagnostics.extend(child_diagnostics);
  //   }
  // });

  // TODO: Consider just returning a vector (or better yet, a Result<(), Vec<Diagnostic>>).
  // FIXME:
  // if diagnostics.is_empty() {
  //   None
  // } else {
  //   Some(diagnostics)
  // }
}

pub fn type_check_function<'a>(function: &ast::Function<'a>) -> TypeCheckResult {
  // FIXME: Need proper implementation of walking the tree for return values.
  let mut block_queue = vec![&function.body];
  let mut values_returned = Vec::new();

  while let Some(block) = block_queue.pop() {
    if let Some(return_stmt) = block.find_terminator() {
      if let Some(return_value) = &return_stmt.value {
        values_returned.push(return_value);
      }
    }

    for statement in &block.statements {
      // TODO: What about recursive/child statements? Is this handled already?
      if let Some(child_blocks) = find_blocks_of(&statement) {
        block_queue.extend(child_blocks);
      }
    }
  }

  let mut diagnostics = Vec::new();

  if let Some(return_kind_group) = &function.prototype.return_kind_group {
    for return_value in values_returned {
      let return_value_kind = find_kind_of(return_value);

      // FIXME: Kind group is being ignored.
      if return_value_kind.is_none() || return_value_kind.unwrap() != return_kind_group.kind {
        diagnostics.push(diagnostic::Diagnostic {
          message: format!(
            // FIXME: Using temporary display value, also dumping objects.
            "function return value type mismatch; expected `{:?}`, but got `{}`",
            return_kind_group.kind, "temp"
          ),
          severity: diagnostic::Severity::Error,
        });
      }
    }
  } else if !values_returned.is_empty() {
    diagnostics.push(diagnostic::Diagnostic {
      message: format!(
        "function `{}` may not return a value because its signature does not specify a return type",
        function.prototype.name
      ),
      severity: diagnostic::Severity::Error,
    });
  }

  if diagnostics.is_empty() {
    None
  } else {
    Some(diagnostics)
  }
}

pub fn type_check_let_stmt(_let_stmt: &ast::LetStmt<'_>) -> TypeCheckResult {
  // let mut diagnostics = Vec::new();

  // if let Some(kind) = &let_stmt.kind {
  //   if let Some(kind_group) = kind.get_kind_group() {}
  // }

  None
}
