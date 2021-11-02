use crate::{
  block, diagnostic, external, function, int_kind, namespace, node, pass, pass::Pass, prototype,
  void_kind,
};
use inkwell::types::AnyType;

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

pub struct LlvmLoweringPass<'a> {
  llvm_context: &'a inkwell::context::Context,
  pub llvm_module: inkwell::module::Module<'a>,
  llvm_type_map: std::collections::HashMap<node::AnyKindNode, inkwell::types::AnyTypeEnum<'a>>,
  llvm_value_map:
    std::collections::HashMap<node::AnyLiteralNode, inkwell::values::BasicValueEnum<'a>>,
  llvm_function_buffer: Option<inkwell::values::FunctionValue<'a>>,
  llvm_basic_block_buffer: Option<inkwell::basic_block::BasicBlock<'a>>,
  llvm_builder_buffer: inkwell::builder::Builder<'a>,
}

impl<'a> LlvmLoweringPass<'a> {
  // TODO: `llvm_module` is being moved.
  pub fn new(
    llvm_context: &'a inkwell::context::Context,
    llvm_module: inkwell::module::Module<'a>,
  ) -> Self {
    Self {
      llvm_context,
      llvm_module,
      llvm_type_map: std::collections::HashMap::new(),
      llvm_value_map: std::collections::HashMap::new(),
      llvm_function_buffer: None,
      llvm_basic_block_buffer: None,
      llvm_builder_buffer: llvm_context.create_builder(),
    }
  }

  // TODO: Support for parameters.
  fn get_function_type_from(
    llvm_return_type: &inkwell::types::AnyTypeEnum<'a>,
    is_variadic: bool,
  ) -> Result<inkwell::types::FunctionType<'a>, diagnostic::Diagnostic> {
    Ok(match llvm_return_type {
      inkwell::types::AnyTypeEnum::IntType(int_type) => int_type.fn_type(&[], is_variadic),
      inkwell::types::AnyTypeEnum::FloatType(float_type) => float_type.fn_type(&[], is_variadic),
      inkwell::types::AnyTypeEnum::VoidType(void_type) => void_type.fn_type(&[], is_variadic),
      _ => {
        // TODO: Better implementation.
        return Err(diagnostic::Diagnostic {
          message: String::from("unexpected point reached"),
          severity: diagnostic::DiagnosticSeverity::Internal,
        });
      }
    })
  }

  // TODO: Consider generalizing into a single function.
  /// Visit the node and return its resulting LLVM type, or if it
  /// was already previously visited, simply retrieve and return
  /// the result from the LLVM types map.
  ///
  /// Returns [`None`] if visiting the node did not insert a result
  /// into the LLVM types map.
  fn visit_or_retrieve_type(
    &mut self,
    node: &node::AnyKindNode,
  ) -> Result<Option<&inkwell::types::AnyTypeEnum<'a>>, diagnostic::Diagnostic> {
    if !self.llvm_type_map.contains_key(node) {
      match node {
        node::AnyKindNode::IntKind(value) => self.visit_int_kind(&value)?,
        node::AnyKindNode::VoidKind(value) => self.visit_void_kind(&value)?,
      };
    }

    Ok(self.llvm_type_map.get(&node))
  }

  /// Visit the node and return its resulting LLVM value, or if it
  /// was already previously visited, simply retrieve and return
  /// the result from the LLVM values map.
  ///
  /// Returns [`None`] if visiting the node did not insert a result
  /// into the LLVM values map.
  fn visit_or_retrieve_value(
    &mut self,
    node: &node::AnyLiteralNode,
  ) -> Result<Option<&inkwell::values::BasicValueEnum<'a>>, diagnostic::Diagnostic> {
    if !self.llvm_value_map.contains_key(node) {
      match node {
        node::AnyLiteralNode::BoolLiteral(value) => self.visit_bool_literal(&value)?,
      };
    }

    Ok(self.llvm_value_map.get(&node))
  }
}

impl<'a> pass::Pass<'a> for LlvmLoweringPass<'a> {
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
    let llvm_return_type = self.visit_or_retrieve_type(&function.prototype.return_kind)?;

    assert!(llvm_return_type.is_some());

    let llvm_function_type = LlvmLoweringPass::get_function_type_from(
      &llvm_return_type.unwrap(),
      function.prototype.is_variadic,
    )?;

    self.llvm_function_buffer = Some(self.llvm_module.add_function(
      function.prototype.name.as_str(),
      llvm_function_type,
      Some(match function.is_public {
        true => inkwell::module::Linkage::External,
        false => inkwell::module::Linkage::Private,
      }),
    ));

    let empty_body_block = block::Block {
      statements: vec![block::AnyStatementNode::ReturnStmt(block::ReturnStmt {
        value: None,
      })],
    };

    // If the body block contains no instructions, force
    // a return void instruction.
    self.visit_block(if function.body.statements.is_empty() {
      &empty_body_block
    } else {
      &function.body
    })
  }

  fn visit_namespace(&mut self, namespace: &namespace::Namespace) -> pass::PassResult {
    for top_level_node in namespace.symbol_table.values() {
      match top_level_node {
        namespace::TopLevelNode::Function(function) => self.visit_function(function)?,
        namespace::TopLevelNode::External(external) => self.visit_external(external)?,
      };
    }

    Ok(())
  }

  fn visit_external(&mut self, external: &external::External) -> pass::PassResult {
    let llvm_function_type = LlvmLoweringPass::get_function_type_from(
      self
        .visit_or_retrieve_type(&external.prototype.return_kind)?
        .unwrap(),
      external.prototype.is_variadic,
    );

    // TODO: Are externs always 'External' linkage?
    self.llvm_module.add_function(
      external.prototype.name.as_str(),
      llvm_function_type?,
      Some(inkwell::module::Linkage::External),
    );

    Ok(())
  }

  fn visit_block(&mut self, block: &block::Block) -> pass::PassResult {
    assert!(self.llvm_function_buffer.is_some());

    self.llvm_basic_block_buffer = Some(
      self
        .llvm_context
        .append_basic_block(self.llvm_function_buffer.unwrap(), ""),
    );

    self
      .llvm_builder_buffer
      .position_at_end(self.llvm_basic_block_buffer.unwrap());

    for statement in &block.statements {
      match statement {
        block::AnyStatementNode::ReturnStmt(return_stmt) => self.visit_return_stmt(&return_stmt)?,
      };
    }

    Ok(())
  }

  fn visit_return_stmt(&mut self, return_stmt: &block::ReturnStmt) -> pass::PassResult {
    assert!(self.llvm_basic_block_buffer.is_some());

    let value = if return_stmt.value.is_some() {
      self.visit_or_retrieve_value(&return_stmt.value.as_ref().unwrap())?
    } else {
      None
    };

    self
      .llvm_builder_buffer
      .build_return(Some(&value.unwrap().into_int_value()));

    Ok(())
  }

  fn visit_bool_literal(&mut self, bool_literal: &node::BoolLiteral) -> pass::PassResult {
    self.llvm_value_map.insert(
      node::AnyLiteralNode::BoolLiteral(*bool_literal),
      inkwell::values::BasicValueEnum::IntValue(
        self
          .llvm_context
          .bool_type()
          .const_int(bool_literal.value as u64, false),
      ),
    );

    Ok(())
  }
}

#[cfg(test)]
mod tests {
  use super::*;

  #[test]
  fn llvm_lowering_pass_proper_initial_values() {
    let llvm_context = inkwell::context::Context::create();
    let llvm_module: inkwell::module::Module = llvm_context.create_module("test");

    assert_eq!(
      true,
      LlvmLoweringPass::new(&llvm_context, llvm_module)
        .llvm_type_map
        .is_empty()
    );
  }

  #[test]
  fn llvm_lowering_pass_visit_or_retrieve_type() {
    let llvm_context = inkwell::context::Context::create();
    let llvm_module = llvm_context.create_module("test");
    let mut llvm_lowering_pass = LlvmLoweringPass::new(&llvm_context, llvm_module);

    let int_kind_box = node::AnyKindNode::IntKind(int_kind::IntKind {
      size: int_kind::IntSize::Signed32,
    });

    let visit_or_retrieve_result = llvm_lowering_pass.visit_or_retrieve_type(&int_kind_box);

    assert_eq!(true, visit_or_retrieve_result.is_ok());
    assert_eq!(true, visit_or_retrieve_result.ok().is_some());
    assert_eq!(1, llvm_lowering_pass.llvm_type_map.len());
  }

  #[test]
  fn llvm_lowering_pass_visit_void_kind() {
    let llvm_context = inkwell::context::Context::create();
    let llvm_module = llvm_context.create_module("test");
    let mut llvm_lowering_pass = LlvmLoweringPass::new(&llvm_context, llvm_module);

    let visit_void_kind_result = llvm_lowering_pass.visit_void_kind(&void_kind::VoidKind {});

    assert_eq!(true, visit_void_kind_result.is_ok());
    assert_eq!(llvm_lowering_pass.llvm_type_map.len(), 1);
  }

  #[test]
  fn llvm_lowering_pass_visit_int_kind() {
    let llvm_context = inkwell::context::Context::create();
    let llvm_module = llvm_context.create_module("test");
    let mut llvm_lowering_pass = LlvmLoweringPass::new(&llvm_context, llvm_module);

    let visit_int_kind_result = llvm_lowering_pass.visit_int_kind(&int_kind::IntKind {
      size: int_kind::IntSize::Signed32,
    });

    assert_eq!(true, visit_int_kind_result.is_ok());
    assert_eq!(llvm_lowering_pass.llvm_type_map.len(), 1);
  }
}
