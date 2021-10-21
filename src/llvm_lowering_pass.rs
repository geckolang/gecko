use crate::{diagnostic, function, int_kind, node, pass, pass::Pass, prototype, void_kind};
use inkwell::types::{AnyType, AnyTypeEnum, FloatType};

macro_rules! assert {
  ($condition:expr) => {
    if !$condition {
      return Err(diagnostic::Diagnostic {
        message: String::from("internal assertion failed"),
        severity: diagnostic::DiagnosticSeverity::Internal,
      });
    }
  };
}

struct LlvmLoweringPass<'a> {
  llvm_context: &'a inkwell::context::Context,
  llvm_module: &'a inkwell::module::Module<'a>,
  llvm_type_map: std::collections::HashMap<node::AnyKindNode, inkwell::types::AnyTypeEnum<'a>>,
}

impl<'a> LlvmLoweringPass<'a> {
  pub fn new(
    llvm_context: &'a inkwell::context::Context,
    llvm_module: &'a inkwell::module::Module<'a>,
  ) -> Self {
    Self {
      llvm_context,
      llvm_module,
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
    node: node::AnyKindNode,
  ) -> Option<&inkwell::types::AnyTypeEnum> {
    if !self.llvm_type_map.contains_key(&node) {
      match node {
        node::AnyKindNode::IntKind(value) => self.visit_int_kind(&value),
        node::AnyKindNode::VoidKind(value) => self.visit_void_kind(&value),
      };
    }

    self.llvm_type_map.get(&node)
  }
}

impl<'a> pass::Pass for LlvmLoweringPass<'a> {
  fn visit_prototype(&mut self, _prototype: &prototype::Prototype) -> pass::PassResult {
    // TODO
    // inkwell::values::GenericValue
    Ok(())
  }

  fn visit_int_kind(&mut self, int_kind: &int_kind::IntKind) -> pass::PassResult {
    self.llvm_type_map.insert(
      node::AnyKindNode::IntKind(*int_kind),
      match int_kind.size {
        int_kind::IntSize::Signed8 => self.llvm_context.i8_type().as_any_type_enum(),
        int_kind::IntSize::Signed16 => self.llvm_context.i16_type().as_any_type_enum(),
        int_kind::IntSize::Signed32 => self.llvm_context.i32_type().as_any_type_enum(),
        int_kind::IntSize::Signed64 => self.llvm_context.i64_type().as_any_type_enum(),
        int_kind::IntSize::Signed128 => self.llvm_context.i128_type().as_any_type_enum(),
      },
    );

    Ok(())
  }

  fn visit_void_kind(&mut self, void_kind: &void_kind::VoidKind) -> pass::PassResult {
    self.llvm_type_map.insert(
      node::AnyKindNode::VoidKind(*void_kind),
      self.llvm_context.void_type().as_any_type_enum(),
    );

    Ok(())
  }

  fn visit_function(&mut self, function: &function::Function) -> pass::PassResult {
    // TODO:
    let llvm_return_type = self.visit_or_retrieve_type(function.prototype.return_kind);

    assert!(llvm_return_type.is_some());

    let llvm_function_type = match llvm_return_type.unwrap() {
      inkwell::types::AnyTypeEnum::FloatType(float_type) => {
        float_type.fn_type(&[], function.prototype.is_variadic)
      }
      _ => {
        return Err(diagnostic::Diagnostic {
          message: String::from("tmp"),
          severity: diagnostic::DiagnosticSeverity::Error,
        })
      }
    };

    self.llvm_module.add_function(
      function.prototype.name.as_str(),
      llvm_function_type,
      Some(inkwell::module::Linkage::Private),
    );

    Ok(())
  }
}

#[cfg(test)]
mod tests {
  use super::*;

  fn create_llvm_lowering_pass(llvm_context: &inkwell::context::Context) -> LlvmLoweringPass {
    LlvmLoweringPass::new(llvm_context, &llvm_context.create_module("test"))
  }

  #[test]
  fn llvm_lowering_pass_proper_initial_values() {
    let mut llvm_lowering_pass = create_llvm_lowering_pass(&inkwell::context::Context::create());

    assert_eq!(true, llvm_lowering_pass.llvm_type_map.is_empty());
  }

  #[test]
  fn llvm_lowering_pass_visit_or_retrieve_type() {
    let mut llvm_lowering_pass = create_llvm_lowering_pass(&inkwell::context::Context::create());

    let int_kind_box = node::AnyKindNode::IntKind(int_kind::IntKind {
      size: int_kind::IntSize::Signed32,
    });

    assert_eq!(
      true,
      llvm_lowering_pass
        .visit_or_retrieve_type(int_kind_box)
        .is_some()
    );

    assert_eq!(1, llvm_lowering_pass.llvm_type_map.len());
  }

  #[test]
  fn llvm_lowering_pass_visit_void_kind() {
    let mut llvm_lowering_pass = create_llvm_lowering_pass(&inkwell::context::Context::create());

    llvm_lowering_pass.visit_void_kind(&void_kind::VoidKind {});
    assert_eq!(llvm_lowering_pass.llvm_type_map.len(), 1);
  }

  #[test]
  fn llvm_lowering_pass_visit_int_kind() {
    let mut llvm_lowering_pass = create_llvm_lowering_pass(&inkwell::context::Context::create());

    llvm_lowering_pass.visit_int_kind(&int_kind::IntKind {
      size: int_kind::IntSize::Signed32,
    });

    assert_eq!(llvm_lowering_pass.llvm_type_map.len(), 1);
  }
}
