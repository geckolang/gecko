use crate::{int_kind, node::*, void_kind, prototype, pass, pass::Pass, node};
use inkwell::types::AnyType;

struct LlvmLoweringPass {
  llvm_context: inkwell::context::Context,
  llvm_types_map: std::collections::HashMap<node::AnyKindNode, i32>,
}

impl LlvmLoweringPass {
  pub fn new() -> Self {
    Self {
      llvm_context: inkwell::context::Context::create(),
      llvm_types_map: std::collections::HashMap::new(),
    }
  }

  fn visit_or_retrieve_type(&self, node: &node::AnyKindNode) -> i32 {
    if !self.llvm_types_map.contains_key(node) {
      match node {
        node::AnyKindNode::IntKind(_) => self.visit(node.into_int_kind().unwrap()),
        node::AnyKindNode::VoidKind(_) => self.visit(node.into_void_kind().unwrap()),
      };

      // TODO:
      // self.visit();

      if !self.llvm_types_map.contains_key(node) {
        panic!("visiting node did not yield a type on the llvm types map");
      }
    }

    // self.llvm_types_map.get(node).unwrap()
    3
  }
}

impl pass::Pass for LlvmLoweringPass {
  fn visit_prototype(&self, _prototype: &prototype::Prototype) {
    // TODO
    // inkwell::values::GenericValue
  }

  fn visit_int_kind(&self, int_kind: &int_kind::IntKind) {
    // TODO: Use diagnostics.
    if int_kind.name.is_empty() {
      panic!("kind's name is empty");
    }

    let mut llvm_type_map = std::collections::HashMap::new();

    let llvm_int_type: Option<inkwell::types::IntType<'_>> = match int_kind.name.as_str() {
      "i32" => Some(self.llvm_context.i32_type()),
      "i64" => Some(self.llvm_context.i64_type()),
      _ => None,
    };

    llvm_type_map.insert(int_kind, llvm_int_type.unwrap().as_any_type_enum());
    println!("{:?}", llvm_type_map);

    // TODO: Implement.
  }

  fn visit_void_kind(&self, void_kind: &void_kind::VoidKind) {
    let llvm_void_type = self.llvm_context.void_type();

    // TODO: Implement.
  }
}

#[cfg(test)]
mod tests {
  use super::*;

  #[test]
  fn llvm_lowering_pass_visit_int_kind() {
    let llvm_lowering_pass = LlvmLoweringPass::new();

    llvm_lowering_pass.visit_int_kind(&int_kind::IntKind{
      name: "i32".to_string()
    });

    assert_eq!(1, 1);
  }

  #[test]
  fn llvm_lowering_pass_visit_or_retrieve_type() {
    let mut llvm_lowering_pass = LlvmLoweringPass::new();

    llvm_lowering_pass.llvm_types_map.insert(node::AnyKindNode::IntKind(int_kind::IntKind{
      name: "i32".to_string()
    }), 123);

    assert_eq!(false, llvm_lowering_pass.llvm_types_map.is_empty());
  }
}
