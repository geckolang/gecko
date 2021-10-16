use crate::{int_kind, node, pass, pass::Pass, prototype, void_kind};
use inkwell::types::AnyType;

struct LlvmLoweringPass<'a> {
  llvm_context: &'a inkwell::context::Context,
  llvm_type_map: std::collections::HashMap<node::AnyKindNode<'a>, inkwell::types::AnyTypeEnum<'a>>,
}

impl<'a> LlvmLoweringPass<'a> {
  pub fn new(llvm_context: &'a inkwell::context::Context) -> Self {
    Self {
      llvm_context,
      llvm_type_map: std::collections::HashMap::new(),
    }
  }

  /// Visit the node and return its resulting LLVM type, or if it
  /// was already previously visited, simply retrieve and return
  /// the result from the LLVM types map.
  ///
  /// Returns `None` if visiting the node did not insert a result
  /// into the LLVM types map.
  fn visit_or_retrieve_type(
    &mut self,
    node: &'a node::AnyKindNode,
  ) -> Option<&'a inkwell::types::AnyTypeEnum> {
    if !self.llvm_type_map.contains_key(node) {
      match node {
        node::AnyKindNode::IntKind(_) => self.visit_int_kind(node.into_int_kind().unwrap()),
        node::AnyKindNode::VoidKind(_) => self.visit_void_kind(node.into_void_kind().unwrap()),
      };

      if !self.llvm_type_map.contains_key(node) {
        return None;
      }
    }

    self.llvm_type_map.get(node)
  }
}

impl<'a> pass::Pass<'a> for LlvmLoweringPass<'a> {
  fn visit_prototype(&mut self, _prototype: &prototype::Prototype) {
    // TODO
    // inkwell::values::GenericValue
  }

  fn visit_int_kind(&mut self, int_kind: &'a int_kind::IntKind) {
    self.llvm_type_map.insert(
      node::AnyKindNode::IntKind(int_kind),
      match int_kind.size {
        int_kind::IntSize::Signed8 => self.llvm_context.i8_type().as_any_type_enum(),
        int_kind::IntSize::Signed16 => self.llvm_context.i16_type().as_any_type_enum(),
        int_kind::IntSize::Signed32 => self.llvm_context.i32_type().as_any_type_enum(),
        int_kind::IntSize::Signed64 => self.llvm_context.i64_type().as_any_type_enum(),
        int_kind::IntSize::Signed128 => self.llvm_context.i128_type().as_any_type_enum(),
      },
    );
  }

  fn visit_void_kind(&mut self, void_kind: &'a void_kind::VoidKind) {
    self.llvm_type_map.insert(
      node::AnyKindNode::VoidKind(void_kind),
      self.llvm_context.void_type().as_any_type_enum(),
    );
  }
}

#[cfg(test)]
mod tests {
  use super::*;

  #[test]
  fn llvm_lowering_pass_proper_initial_values() {
    let llvm_context = inkwell::context::Context::create();
    let llvm_lowering_pass = LlvmLoweringPass::new(&llvm_context);

    assert_eq!(true, llvm_lowering_pass.llvm_type_map.is_empty());
  }

  #[test]
  fn llvm_lowering_pass_visit_or_retrieve_type() {
    let llvm_context = inkwell::context::Context::create();
    let mut llvm_lowering_pass = LlvmLoweringPass::new(&llvm_context);

    let int_kind = int_kind::IntKind {
      size: int_kind::IntSize::Signed32,
    };

    let int_kind_box = node::AnyKindNode::IntKind(&int_kind);

    assert_eq!(
      true,
      llvm_lowering_pass
        .visit_or_retrieve_type(&int_kind_box)
        .is_some()
    );

    // TODO:
    // assert_eq!(1, llvm_lowering_pass.llvm_type_map.len());
  }

  #[test]
  fn llvm_lowering_pass_visit_void_kind() {
    let llvm_context = inkwell::context::Context::create();
    let mut llvm_lowering_pass = LlvmLoweringPass::new(&llvm_context);

    llvm_lowering_pass.visit_void_kind(&void_kind::VoidKind {});
    assert_eq!(llvm_lowering_pass.llvm_type_map.len(), 1);
  }

  #[test]
  fn llvm_lowering_pass_visit_int_kind() {
    let llvm_context = inkwell::context::Context::create();
    let mut llvm_lowering_pass = LlvmLoweringPass::new(&llvm_context);

    llvm_lowering_pass.visit_int_kind(&int_kind::IntKind {
      size: int_kind::IntSize::Signed32,
    });

    assert_eq!(llvm_lowering_pass.llvm_type_map.len(), 1);
  }
}
