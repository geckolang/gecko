use crate::diagnostic;

struct _BorrowCheckContext {
  diagnostic_builder: diagnostic::DiagnosticBuilder,
  depth: usize,
}

impl _BorrowCheckContext {
  fn _new() -> Self {
    Self {
      diagnostic_builder: diagnostic::DiagnosticBuilder::new(),
      depth: 0,
    }
  }

  // TODO: If we can't save a reference or some kind of identifier to a let-statement
  // ... (to define its lifetime), consider adding an `Option<LifeTimeStruct>` field
  // ... to the let-statement, just for this purpose.
  // fn declare(&self, let_stmt: &ast::NodeKind::LetStmt) {
  //   // TODO:
  // }

  // fn lifetime_check(&mut self, root_block: &ast::Block) {
  //   // Reset the depth counter per-block.
  //   self.depth = 0;

  //   // TODO: Figure out how to determine depth via a linear manner? If not possible, opt for a different design/approach.

  //   let block_queue = vec![root_block];

  //   while let Some(block) = block_queue.pop() {
  //     match statement {
  //       ast::Node::LetStmt(let_stmt) => self.declare(let_stmt),
  //       // TODO: Continue implementation.
  //       _ => continue,
  //     };
  //   }
  // }
}

// trait LifetimeCheck {
//   fn lifetime(&self) {
//     // TODO: Isn't this actually recursive for default implementations? Investigate.
//     crate::dispatch!(self, BorrowCheck::lifetime);
//   }

//   fn lifetime_check(&self) {
//     crate::dispatch!(self, BorrowCheck::borrow_check);
//   }
// }

// impl BorrowCheck for ast::AssignStmt {
//   fn lifetime_check(&self) {}
// }
