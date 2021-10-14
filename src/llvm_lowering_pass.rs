use crate::{int_kind, node, pass, pass::Pass, prototype, void_kind};
use inkwell::types::AnyType;

struct LlvmLoweringPass<'a> {
  llvm_context: inkwell::context::Context,
  llvm_type_map: std::collections::HashMap<node::AnyKindNode<'a>, inkwell::types::AnyTypeEnum<'a>>,
}

impl<'a> LlvmLoweringPass<'a> {
  pub fn new() -> Self {
    Self {
      llvm_context: inkwell::context::Context::create(),
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
    &'a mut self,
    node: &'a node::AnyKindNode,
  ) -> Option<&'a inkwell::types::AnyTypeEnum> {
    if !self.llvm_type_map.contains_key(node) {
      match node {
        node::AnyKindNode::IntKind(_) => self.visit_int_kind(node.into_int_kind().unwrap()),
        node::AnyKindNode::VoidKind(_) => self.visit_void_kind(node.into_void_kind().unwrap()),
      };

      // TODO:
      // self.visit();

      if !self.llvm_type_map.contains_key(node) {
        return None;
      }
    }

    Some(self.llvm_type_map.get(node).unwrap())
  }
}

impl<'a> pass::Pass<'a> for LlvmLoweringPass<'a> {
  fn visit_prototype(&mut self, _prototype: &prototype::Prototype) {
    // TODO
    // inkwell::values::GenericValue
  }

  /// Panics if the int kind has an unknown name.
  fn visit_int_kind(&'a mut self, int_kind: &'a int_kind::IntKind) {
    // TODO: Use diagnostics.
    if int_kind.name.is_empty() {
      panic!("kind's name is empty");
    }

    let llvm_int_type: Option<inkwell::types::IntType<'_>> = match int_kind.name.as_str() {
      "i32" => Some(self.llvm_context.i32_type()),
      "i64" => Some(self.llvm_context.i64_type()),
      _ => panic!("unknown int kind name"),
    };

    // v: llvm_int_type.unwrap().as_any_type_enum()

    self.llvm_type_map.insert(
      node::AnyKindNode::IntKind(int_kind),
      llvm_int_type.unwrap().as_any_type_enum(),
    );
  }

  fn visit_void_kind(&mut self, void_kind: &void_kind::VoidKind) {
    let llvm_void_type = self.llvm_context.void_type();

    // TODO: Implement.
  }
}

#[cfg(test)]
mod tests {
  use super::*;

  #[test]
  fn llvm_lowering_pass_proper_initial_values() {
    let llvm_lowering_pass = LlvmLoweringPass::new();

    assert_eq!(true, llvm_lowering_pass.llvm_type_map.is_empty());
  }

  #[test]
  fn llvm_lowering_pass_visit_int_kind() {
    let mut llvm_lowering_pass = LlvmLoweringPass::new();

    llvm_lowering_pass.visit_int_kind(&int_kind::IntKind {
      name: "i32".to_string(),
    });

    assert_eq!(1, 1);
  }

  #[test]
  fn llvm_lowering_pass_visit_or_retrieve_type() {
    let mut llvm_lowering_pass = LlvmLoweringPass::new();

    let int_kind = int_kind::IntKind {
      name: "i32".to_string(),
    };

    let int_kind_box = node::AnyKindNode::IntKind(&int_kind);

    assert_eq!(
      true,
      llvm_lowering_pass
        .visit_or_retrieve_type(&int_kind_box)
        .is_some()
    );

    assert_eq!(1, llvm_lowering_pass.llvm_type_map.len());
  }
}
