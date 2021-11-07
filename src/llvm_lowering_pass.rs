use crate::{diagnostic, int_kind, node, pass, pass::Pass, void_kind};
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
  // TODO: Consider making Option.
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

  fn get_function_type_from(
    parameters: &[inkwell::types::BasicMetadataTypeEnum<'a>],
    llvm_return_type: &inkwell::types::AnyTypeEnum<'a>,
    is_variadic: bool,
  ) -> Result<inkwell::types::FunctionType<'a>, diagnostic::Diagnostic> {
    Ok(match llvm_return_type {
      inkwell::types::AnyTypeEnum::IntType(int_type) => int_type.fn_type(parameters, is_variadic),
      inkwell::types::AnyTypeEnum::FloatType(float_type) => {
        float_type.fn_type(parameters, is_variadic)
      }
      inkwell::types::AnyTypeEnum::VoidType(void_type) => {
        void_type.fn_type(parameters, is_variadic)
      }
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
        node::AnyKindNode::BoolKind(value) => self.visit_bool_kind(&value)?,
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
        node::AnyLiteralNode::IntLiteral(value) => self.visit_int_literal(&value)?,
      };
    }

    Ok(self.llvm_value_map.get(&node))
  }
}

impl<'a> pass::Pass for LlvmLoweringPass<'a> {
  fn visit_int_kind(&mut self, int_kind: &int_kind::IntKind) -> pass::PassResult {
    self.llvm_type_map.insert(
      node::AnyKindNode::IntKind(*int_kind),
      match int_kind.size {
        int_kind::IntSize::Bit8 => self.llvm_context.i8_type().as_any_type_enum(),
        int_kind::IntSize::Bit16 => self.llvm_context.i16_type().as_any_type_enum(),
        int_kind::IntSize::Bit32 => self.llvm_context.i32_type().as_any_type_enum(),
        int_kind::IntSize::Bit64 => self.llvm_context.i64_type().as_any_type_enum(),
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

  fn visit_bool_kind(&mut self, bool_kind: &int_kind::BoolKind) -> pass::PassResult {
    self.llvm_type_map.insert(
      node::AnyKindNode::BoolKind(*bool_kind),
      self.llvm_context.bool_type().as_any_type_enum(),
    );

    Ok(())
  }

  fn visit_function(&mut self, function: &node::Function) -> pass::PassResult {
    // TODO Simplify process of lowering parameters for externals as well.
    let mut parameters = vec![];

    // TODO: Further on, need to make use of the parameter's name somehow (maybe during lookups?).
    for parameter in &function.prototype.parameters {
      let llvm_parameter_result = self.visit_or_retrieve_type(&parameter.1.kind)?;

      assert!(llvm_parameter_result.is_some());

      parameters.push(match llvm_parameter_result.unwrap() {
        // TODO: Add other types as they become available.
        inkwell::types::AnyTypeEnum::IntType(int_type) => {
          inkwell::types::BasicMetadataTypeEnum::IntType(*int_type)
        }
        inkwell::types::AnyTypeEnum::VoidType(_) => {
          return Err(diagnostic::Diagnostic {
            message: format!("type of parameter `{}` cannot be void", parameter.0),
            severity: diagnostic::DiagnosticSeverity::Internal,
          })
        }
        _ => {
          return Err(diagnostic::Diagnostic {
            message: format!("unsupported parameter type for `{}`", parameter.0),
            severity: diagnostic::DiagnosticSeverity::Internal,
          })
        }
      });
    }

    let llvm_return_type =
      self.visit_or_retrieve_type(&function.prototype.return_kind_group.kind)?;

    assert!(llvm_return_type.is_some());

    // TODO:
    // match llvm_lowering_pass
    //   .visit_or_retrieve_type(&parameter.1.kind)
    //   .unwrap()
    //   .unwrap()
    // {
    //   inkwell::types::AnyTypeEnum::IntType(int_type) => {
    //     inkwell::types::BasicMetadataTypeEnum::IntType(*int_type)
    //   }
    //   _ => panic!("test"),
    // }

    let llvm_function_type = Self::get_function_type_from(
      parameters.as_slice(),
      &llvm_return_type.unwrap(),
      function.prototype.is_variadic,
    )?;

    self.llvm_function_buffer = Some(self.llvm_module.add_function(
      function.prototype.name.as_str(),
      llvm_function_type,
      Some(if function.is_public {
        inkwell::module::Linkage::External
      } else {
        inkwell::module::Linkage::Private
      }),
    ));

    let empty_body_block = node::Block {
      statements: vec![node::AnyStmtNode::ReturnStmt(node::ReturnStmt {
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

  fn visit_package(&mut self, package: &node::Package) -> pass::PassResult {
    // Reset all buffers when visiting a new package.
    self.llvm_builder_buffer.clear_insertion_position();
    self.llvm_basic_block_buffer = None;
    self.llvm_function_buffer = None;

    for top_level_node in package.symbol_table.values() {
      match top_level_node {
        node::TopLevelNode::Function(function) => self.visit_function(function)?,
        node::TopLevelNode::External(external) => self.visit_external(external)?,
      };
    }

    Ok(())
  }

  fn visit_external(&mut self, external: &node::External) -> pass::PassResult {
    // TODO: Support for parameters.
    let llvm_function_type = Self::get_function_type_from(
      &[],
      self
        .visit_or_retrieve_type(&external.prototype.return_kind_group.kind)?
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

  fn visit_block(&mut self, block: &node::Block) -> pass::PassResult {
    assert!(self.llvm_function_buffer.is_some());

    self.llvm_basic_block_buffer = Some(
      self
        .llvm_context
        // TODO: Name basic block?
        .append_basic_block(self.llvm_function_buffer.unwrap(), ""),
    );

    self
      .llvm_builder_buffer
      .position_at_end(self.llvm_basic_block_buffer.unwrap());

    for statement in &block.statements {
      match statement {
        node::AnyStmtNode::ReturnStmt(return_stmt) => self.visit_return_stmt(&return_stmt)?,
        // TODO: Consider relocating as an associated function for Pass?
        node::AnyStmtNode::ExprWrapperStmt(expr) => match expr {
          node::AnyExprNode::CallExpr(call_expr) => self.visit_call_expr(call_expr)?,
          node::AnyExprNode::LiteralWrapperExpr(_) => {}
        },
        node::AnyStmtNode::LetStmt(let_stmt) => self.visit_let_stmt(&let_stmt)?,
      };
    }

    Ok(())
  }

  fn visit_return_stmt(&mut self, return_stmt: &node::ReturnStmt) -> pass::PassResult {
    assert!(self.llvm_basic_block_buffer.is_some());

    // FIXME: This is a temporary fix.

    // let basic_value_enum_result = match return_stmt.value {
    //   // TODO: Fix unsafe unwrap (not checking for None).
    //   Some(value) => self.visit_or_retrieve_value(&value)?.unwrap(),
    //   _ => panic!("NO!"),
    // };

    if return_stmt.value.is_some() {
      match return_stmt.value.unwrap() {
        node::AnyLiteralNode::BoolLiteral(value) => self.visit_bool_literal(&value)?,
        node::AnyLiteralNode::IntLiteral(value) => self.visit_int_literal(&value)?,
      };
    }

    self
      .llvm_builder_buffer
      .build_return(if return_stmt.value.is_some() {
        Some(
          self
            .llvm_value_map
            .get(&return_stmt.value.unwrap())
            .unwrap(),
        )
      } else {
        None
      });

    Ok(())
  }

  fn visit_let_stmt(&mut self, let_stmt: &node::LetStmt) -> pass::PassResult {
    use inkwell::types::BasicType;

    assert!(self.llvm_basic_block_buffer.is_some());

    let llvm_any_type = self.visit_or_retrieve_type(&let_stmt.kind_group.kind)?;

    assert!(llvm_any_type.is_some());

    let llvm_type = match llvm_any_type.unwrap() {
      inkwell::types::AnyTypeEnum::IntType(int_type) => int_type.as_basic_type_enum(),
      _ => {
        return Err(diagnostic::Diagnostic {
          message: format!("illegal declaration type: `{:?}`", llvm_any_type),
          severity: diagnostic::DiagnosticSeverity::Error,
        })
      }
    };

    let llvm_alloca_inst_ptr = self
      .llvm_builder_buffer
      .build_alloca(llvm_type, let_stmt.name.as_str());

    // TODO: Finish implementing.
    // let llvm_value = self.visit_or_retrieve_value(&let_stmt.value)?;

    // assert!(llvm_value.is_some());

    // self
    //   .llvm_builder_buffer
    //   .build_store(llvm_alloca_inst_ptr, llvm_value);

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

  fn visit_int_literal(&mut self, int_literal: &node::IntLiteral) -> pass::PassResult {
    let llvm_type = self
      .visit_or_retrieve_type(&node::AnyKindNode::IntKind(int_literal.kind))?
      .unwrap();

    let llvm_value = match llvm_type {
      inkwell::types::AnyTypeEnum::IntType(int_type) => {
        int_type.const_int(int_literal.value as u64, false)
      }
      _ => {
        return Err(diagnostic::Diagnostic {
          // TODO: Better error message?
          message: String::from("expected integer type"),
          severity: diagnostic::DiagnosticSeverity::Internal,
        });
      }
    };

    self.llvm_value_map.insert(
      node::AnyLiteralNode::IntLiteral(*int_literal),
      inkwell::values::BasicValueEnum::IntValue(llvm_value),
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
      size: int_kind::IntSize::Bit32,
      is_signed: true,
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
      size: int_kind::IntSize::Bit32,
      is_signed: true,
    });

    assert_eq!(true, visit_int_kind_result.is_ok());
    assert_eq!(llvm_lowering_pass.llvm_type_map.len(), 1);
  }

  #[test]
  fn visit_function() {
    let llvm_context = inkwell::context::Context::create();
    let llvm_module = llvm_context.create_module("test");
    let mut llvm_lowering_pass = LlvmLoweringPass::new(&llvm_context, llvm_module);

    let visit_function_result = llvm_lowering_pass.visit_function(&node::Function {
      is_public: false,
      prototype: node::Prototype {
        name: String::from("foo"),
        return_kind_group: node::KindGroup {
          kind: node::AnyKindNode::VoidKind(void_kind::VoidKind {}),
          is_reference: false,
          is_mutable: false,
        },
        parameters: vec![],
        is_variadic: false,
      },
      body: node::Block { statements: vec![] },
    });

    assert_eq!(true, visit_function_result.is_ok());
    assert_eq!(true, llvm_lowering_pass.llvm_function_buffer.is_some());
  }
}
