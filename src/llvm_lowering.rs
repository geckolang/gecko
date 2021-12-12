use crate::{ast, context, diagnostic};
use inkwell::{types::AnyType, values::BasicValue};

const ENTRY_POINT_NAME: &str = "main";

trait Lower {
  fn lower<'ctx>(
    &self,
    generator: LlvmGenerator<'ctx>,
    context: &mut context::Context,
  ) -> inkwell::values::BasicValueEnum<'ctx>;
}

impl Lower for ast::Prototype {
  fn lower<'ctx>(
    &self,
    generator: LlvmGenerator<'ctx>,
    context: &mut context::Context,
  ) -> inkwell::values::BasicValueEnum<'ctx> {
    // TODO: Simplify process of lowering parameters for externals as well.
    let mut parameters = vec![];

    parameters.reserve(self.parameters.len());

    // TODO: Further on, need to make use of the parameter's name somehow (maybe during lookups?).
    for (parameter_name, parameter_kind_group) in &self.parameters {
      let llvm_parameter =
        lower_or_retrieve_type!(self, &NodeKindKey::from(&parameter_kind_group.kind))?;

      parameters.push(match llvm_parameter {
        // TODO: Add other types as they become available.
        inkwell::types::AnyTypeEnum::IntType(int_type) => {
          inkwell::types::BasicMetadataTypeEnum::IntType(*int_type)
        }
        inkwell::types::AnyTypeEnum::VoidType(_) => {
          todo!();
          // return Err(diagnostic::Diagnostic {
          //   message: format!("type of parameter `{}` cannot be void", parameter_name),
          //   severity: diagnostic::Severity::Internal,
          // })
        }
        _ => {
          todo!();
          // return Err(diagnostic::Diagnostic {
          //   message: format!("unsupported parameter type for `{}`", parameter_name),
          //   severity: diagnostic::Severity::Internal,
          // })
        }
      });
    }

    // TODO: Creating regardless.
    let llvm_void_type = generator.llvm_context.void_type().as_any_type_enum();

    let llvm_function_type = Self::get_function_type_from(
      parameters.as_slice(),
      if let Some(return_kind_group) = &prototype.return_kind_group {
        lower_or_retrieve_type!(self, &NodeKindKey::from(&return_kind_group.kind))?
      } else {
        &llvm_void_type
      },
      self.is_variadic,
    )?;

    Ok(llvm_function_type)
  }
}

impl Lower for ast::Literal {
  fn lower<'ctx>(
    &self,
    generator: LlvmGenerator<'ctx>,
    _: &mut context::Context,
  ) -> inkwell::values::BasicValueEnum<'ctx> {
    match self {
      ast::Literal::Integer(value, integer_kind) => {
        let llvm_type = match integer_kind {
          ast::IntegerKind::I8 => self.llvm_context.i8_type(),
          ast::IntegerKind::I16 => self.llvm_context.i16_type(),
          ast::IntegerKind::I32 => self.llvm_context.i32_type(),
          ast::IntegerKind::I64 => self.llvm_context.i64_type(),
          ast::IntegerKind::U8 => self.llvm_context.u8_type(),
          ast::IntegerKind::U16 => self.llvm_context.u16_type(),
          ast::IntegerKind::U32 => self.llvm_context.u32_type(),
          ast::IntegerKind::U64 => self.llvm_context.u64_type(),
          // TODO: Handle `Usize` and `Isize`.
          _ => todo!(),
        };

        let llvm_int_value = match llvm_type {
          inkwell::types::AnyTypeEnum::IntType(int_type) => int_type.const_int(value as u64, false),
          _ => {
            return Err(diagnostic::Diagnostic {
              // TODO: Better error message?
              message: "expected integer type".to_string(),
              severity: diagnostic::Severity::Internal,
            });
          }
        };
      }
      ast::Literal::Char(character) => generator
        .llvm_context
        .i8_type()
        .const_int(*character as u64, false)
        .as_basic_value_enum(),
      // TODO: Process all literals.
      ast::Literal::Bool(value) => generator
        .llvm_context
        .bool_type()
        .const_int(value as u64, false),
      _ => todo!(),
    }
  }
}

impl Lower for ast::Function<'_> {
  fn lower<'ctx>(
    &self,
    generator: LlvmGenerator<'ctx>,
    context: &mut context::Context,
  ) -> inkwell::values::BasicValueEnum<'ctx> {
    // TODO: At this point is should be clear by default (unless specific methods where called). Maybe put note about this.
    self.llvm_basic_block_fallthrough_stack.clear();

    crate::pass_assert!(self.module_buffer.is_some());

    let llvm_function_type =
      lower_or_retrieve_type!(self, &NodeKindKey::Prototype(&self.prototype))?;

    crate::pass_assert!(llvm_function_type.is_function_type());

    let llvm_function_name = if self.prototype.name == ENTRY_POINT_NAME {
      self.prototype.name.clone()
    } else {
      mangle_name(&self.module_buffer.unwrap().name, &self.prototype.name)
    };

    let llvm_function = self.llvm_module.add_function(
      llvm_function_name.as_str(),
      llvm_function_type.into_function_type(),
      Some(if self.prototype.name == ENTRY_POINT_NAME {
        inkwell::module::Linkage::External
      } else {
        inkwell::module::Linkage::Private
      }),
    );

    // TODO: Find a way to use only one loop to process both local parameters and LLVM's names.
    for (i, ref mut llvm_parameter) in llvm_function.get_param_iter().enumerate() {
      let (parameter_name, _) = &self.prototype.parameters[i];

      llvm_parameter.set_name(parameter_name.as_str());
    }

    // TODO: Buffer may be overwritten (example: visiting a call expression's callee), use a map instead.
    self.llvm_function_like_buffer = Some(llvm_function);
    self.lower_block(&self.body)?;

    // TODO: Ensure this works as expected (tests + debugging).
    // TODO: Cloning of `get_basic_blocks()` may occur twice.
    // TODO: Add handling in the case of being out-of-sync.
    // Handle fallthrough-eligible blocks.
    for (index, llvm_block) in self.llvm_basic_block_fallthrough_stack.iter().enumerate() {
      // Ignore basic blocks that already have terminator, as they don't require fallthrough.
      if llvm_block.get_terminator().is_some() {
        continue;
      }

      // Check if there is more than one fallthrough-eligible block, and this isn't the root block.
      if index > 0 {
        let llvm_temporary_builder = self.llvm_context.create_builder();

        // NOTE: It's okay to dereference, as the real LLVM basic block value is behind a reference (not owned by this value).
        llvm_temporary_builder.position_at_end(*llvm_block);

        llvm_temporary_builder
          .build_unconditional_branch(self.llvm_basic_block_fallthrough_stack[index - 1]);
      }
    }

    self.llvm_basic_block_fallthrough_stack.clear();
    crate::pass_assert!(self.llvm_function_like_buffer.unwrap().verify(false));

    Ok(llvm_function)
  }
}

impl Lower for ast::External {
  fn lower<'ctx>(
    &self,
    generator: LlvmGenerator<'ctx>,
    context: &mut context::Context,
  ) -> inkwell::values::BasicValueEnum<'ctx> {
    let llvm_function_type =
      lower_or_retrieve_type!(self, &NodeKindKey::Prototype(&self.prototype))?;

    crate::pass_assert!(llvm_function_type.is_function_type());

    let llvm_external_function = generator.llvm_module.add_function(
      self.prototype.name.as_str(),
      llvm_function_type.into_function_type(),
      Some(inkwell::module::Linkage::External),
    );

    crate::pass_assert!(llvm_external_function.verify(false));

    // TODO: Are externs always 'External' linkage?
    self.llvm_function_like_buffer = Some(llvm_external_function);

    Ok(llvm_external_function)
  }
}

impl Lower for ast::Block<'_> {
  fn lower<'ctx>(
    &self,
    generator: LlvmGenerator<'ctx>,
    context: &mut context::Context,
  ) -> inkwell::values::BasicValueEnum<'ctx> {
    crate::pass_assert!(self.llvm_function_like_buffer.is_some());

    let llvm_basic_block = generator.llvm_context.append_basic_block(
      self.llvm_function_like_buffer.unwrap(),
      self.llvm_name.as_str(),
    );

    self.llvm_builder_buffer.position_at_end(llvm_basic_block);

    for statement in &self.statements {
      // TODO: Get `&dyn Node` then call `accept()` on it. Only if this occurs more than once (would need to implement `From<>`).
      match statement {
        ast::Node::ReturnStmt(return_stmt) => {
          self.lower_return_stmt(&return_stmt)?;
        }
        // TODO: Consider relocating as an associated function for `Pass`? Does this occur more than once?
        ast::Node::ExprWrapperStmt(expr) => {
          match expr {
            // TODO: Implement missing cases.
            ast::ExprHolder::CallExpr(call_expr) => self.lower_call_expr(call_expr)?,
            _ => todo!(),
          };
        }
        ast::Node::LetStmt(let_stmt) => {
          self.lower_let_stmt(&let_stmt)?;
        }
        ast::Node::IfStmt(if_stmt) => {
          self.lower_if_stmt(&if_stmt)?;
        }
        ast::Node::WhileStmt(while_stmt) => {
          self.lower_while_stmt(&while_stmt)?;
        }
        ast::Node::BlockStmt(block_stmt) => {
          self.lower_block_stmt(&block_stmt)?;
        }
        ast::Node::BreakStmt(break_stmt) => {
          self.lower_break_stmt(&break_stmt)?;
        }
        // TODO: Throw I.C.E.?
        _ => unreachable!(),
      };
    }

    Ok(llvm_basic_block)
  }
}

impl Lower for ast::ReturnStmt<'_> {
  fn lower<'ctx>(
    &self,
    generator: LlvmGenerator<'ctx>,
    context: &mut context::Context,
  ) -> inkwell::values::BasicValueEnum<'ctx> {
    crate::pass_assert!(self.llvm_builder_buffer.get_insert_block().is_some());

    if self.value.is_some() {
      lower_or_retrieve_value!(
        self,
        &NodeValueKey::from(ast::ExprTransport::from(
          return_stmt.value.as_ref().unwrap(),
        ))
      )?;
    }

    let llvm_return_inst = self
      .llvm_builder_buffer
      .build_return(if self.value.is_some() {
        Some(
          self
            .llvm_value_map
            .get(&NodeValueKey::from(ast::ExprTransport::from(
              self.value.as_ref().unwrap(),
            )))
            .unwrap(),
        )
      } else {
        None
      });

    Ok(llvm_return_inst)
  }
}

impl Lower for ast::LetStmt<'_> {
  fn lower<'ctx>(
    &self,
    generator: LlvmGenerator<'ctx>,
    context: &mut context::Context,
  ) -> inkwell::values::BasicValueEnum<'ctx> {
    use inkwell::types::BasicType;

    crate::pass_assert!(self.llvm_builder_buffer.get_insert_block().is_some());

    let llvm_any_type =
      lower_or_retrieve_type!(self, &NodeKindKey::from(&let_stmt.kind_group.kind))?;

    let llvm_type = match llvm_any_type {
      // TODO: Support other LLVM types.
      // NOTE: This covers boolean types as well (`i1`).
      inkwell::types::AnyTypeEnum::IntType(int_type) => int_type.as_basic_type_enum(),
      _ => {
        return Err(diagnostic::Diagnostic {
          message: format!("illegal declaration type: `{:?}`", llvm_any_type),
          severity: diagnostic::Severity::Error,
        })
      }
    };

    // TODO: Finish implementing.
    let llvm_alloca_inst_ptr = self
      .llvm_builder_buffer
      .build_alloca(llvm_type, self.name.as_str());

    let llvm_value = lower_or_retrieve_value!(self, &NodeValueKey::from(&let_stmt.value))?;

    // FIXME: Consider adding a method to TAKE the values from `llvm_value_map` and `llvm_type_map`, as errors occur when dealing with reference values and types instead of OWNED ones.

    self
      .llvm_builder_buffer
      // TODO: Calling `to_owned()` clones the value. This is not ideal, and could cause problems? Or, SHOULD we be cloning values?
      .build_store(llvm_alloca_inst_ptr, llvm_value.to_owned());

    // TODO: No insertion into the value map?

    Ok(llvm_alloca_inst_ptr)
  }
}

impl Lower for ast::CallExpr<'_> {
  fn lower<'ctx>(
    &self,
    generator: LlvmGenerator<'ctx>,
    context: &mut context::Context,
  ) -> inkwell::values::BasicValueEnum<'ctx> {
    crate::pass_assert!(self.llvm_builder_buffer.get_insert_block().is_some());
    crate::pass_assert!(self.callee.value.is_some());

    match self.callee.value.as_ref().unwrap() {
      // TODO: Need to stop callee from lowering more than once. Also, watch out for buffers being overwritten.
      ast::CalleeTransport::Function(function) => self.lower_function(function)?,
      ast::CalleeTransport::External(external) => self.lower_external(external)?,
    };

    let mut arguments = Vec::new();

    arguments.reserve(self.arguments.len());

    for argument in &self.arguments {
      // TODO: Cloning argument.
      let llvm_value = lower_or_retrieve_value!(self, &NodeValueKey::from(argument.clone()))?;

      arguments.push(match llvm_value {
        // TODO: Add support for missing basic values.
        inkwell::values::BasicValueEnum::IntValue(int_value) => {
          inkwell::values::BasicMetadataValueEnum::IntValue(*int_value)
        }
        _ => {
          return Err(diagnostic::Diagnostic {
            message: format!("unexpected argument type `{:?}`", argument),
            severity: diagnostic::Severity::Internal,
          })
        }
      });
    }

    let llvm_call_value = self.llvm_builder_buffer.build_call(
      self.llvm_function_like_buffer.unwrap(),
      arguments.as_slice(),
      "call_result",
    );

    let llvm_call_basic_value_result = llvm_call_value.try_as_basic_value();

    crate::pass_assert!(llvm_call_basic_value_result.is_left());

    let llvm_call_basic_value = llvm_call_basic_value_result.left().unwrap();

    self
      .llvm_value_map
      .insert(NodeValueKey::CallExpr(self), llvm_call_basic_value);

    // FIXME: Awaiting checks?
    Ok(llvm_call_value)
  }
}

impl Lower for ast::WhileStmt<'_> {
  fn lower<'ctx>(
    &self,
    generator: LlvmGenerator<'ctx>,
    context: &mut context::Context,
  ) -> inkwell::values::BasicValueEnum<'ctx> {
    // FIXME: The problem is that fallthrough only occurs once, it does not propagate. (Along with the possible empty block problem).

    crate::pass_assert!(self.llvm_builder_buffer.get_insert_block().is_some());

    let llvm_parent_block = self.llvm_builder_buffer.get_insert_block().unwrap();

    let llvm_condition_basic_value =
      lower_or_retrieve_value!(self, &NodeValueKey::from(&self.condition))?;

    let llvm_condition_int_value = llvm_condition_basic_value.into_int_value();

    // TODO: What if even tho. we set the buffer to the after block, there isn't any instructions lowered? This would leave the block without even a `ret void` (which is required).
    let llvm_after_block = self
      .llvm_context
      .append_basic_block(self.llvm_function_like_buffer.unwrap(), "while_after");

    self
      .llvm_basic_block_fallthrough_stack
      .push(llvm_after_block);

    // TODO: Cloning basic block.
    let llvm_then_block = lower_or_retrieve_block!(self, &self.body)?.to_owned();
    let llvm_parent_builder = self.llvm_context.create_builder();

    llvm_parent_builder.position_at_end(llvm_parent_block);

    llvm_parent_builder.build_conditional_branch(
      llvm_condition_int_value,
      llvm_then_block,
      llvm_after_block,
    );

    // TODO: Assert condition is both an int value and that it has 1 bit width.

    self.llvm_builder_buffer.position_at_end(llvm_after_block);

    if llvm_then_block.get_terminator().is_none() {
      let llvm_temporary_builder = self.llvm_context.create_builder();

      llvm_temporary_builder.position_at_end(llvm_then_block);

      llvm_temporary_builder.build_conditional_branch(
        llvm_condition_int_value,
        llvm_then_block,
        llvm_after_block,
      );
    }

    Ok(())
  }
}

impl Lower for ast::IfStmt<'_> {
  fn lower<'ctx>(
    &self,
    generator: LlvmGenerator<'ctx>,
    context: &mut context::Context,
  ) -> inkwell::values::BasicValueEnum<'ctx> {
    crate::pass_assert!(self.llvm_function_like_buffer.is_some());
    crate::pass_assert!(self.llvm_builder_buffer.get_insert_block().is_some());

    let llvm_function = self.llvm_function_like_buffer.unwrap();

    // TODO: Verify builder is in the correct/expected block.
    // TODO: What if the buffer was intended to be for an external?

    let llvm_parent_block = self.llvm_builder_buffer.get_insert_block().unwrap();

    let llvm_after_block = self
      .llvm_context
      .append_basic_block(llvm_function, "if_after");

    self
      .llvm_basic_block_fallthrough_stack
      .push(llvm_after_block);

    let llvm_then_block = lower_or_retrieve_block!(self, &self.then_block)?;

    // Position the builder on the `after` block, for the next statement(s) (if any).
    // It is important to do this after visiting the `then` block.
    self.llvm_builder_buffer.position_at_end(llvm_after_block);

    // TODO: Does it matter if the condition is visited before the `then` block? (Remember that the code is not being executed).
    let llvm_condition_basic_value =
      lower_or_retrieve_value!(self, &NodeValueKey::from(&self.condition))?;

    crate::pass_assert!(llvm_condition_basic_value.is_int_value());

    let llvm_condition_int_value = llvm_condition_basic_value.into_int_value();

    crate::pass_assert!(llvm_condition_int_value.get_type().get_bit_width() == 1);

    let llvm_temporary_builder = self.llvm_context.create_builder();

    llvm_temporary_builder.position_at_end(llvm_parent_block);

    llvm_temporary_builder.build_conditional_branch(
      llvm_condition_int_value,
      // TODO: Cloning LLVM basic block.
      llvm_then_block.to_owned(),
      llvm_after_block,
    );

    // If there is no terminator instruction on the `then` LLVM basic block,
    // build a link to continue the code after the if statement.
    if llvm_then_block.get_terminator().is_none() {
      // TODO: Cloning LLVM basic block.
      llvm_temporary_builder.position_at_end(llvm_then_block.to_owned());
      llvm_temporary_builder.build_unconditional_branch(llvm_after_block);
    }

    // TODO: At this point not all instructions have been lowered (only up to the `if` statement itself), which means that a terminator can exist afterwards, but that case is being ignored here.
    // If the `after` block has no terminator instruction, there might be a
    // possibility for fallthrough. If after visiting the `then` block, the
    // length of the LLVM basic block stack is larger than the cached length,
    // then there is a fallthrough.
    if llvm_after_block.get_terminator().is_none() {
      llvm_temporary_builder.position_at_end(llvm_after_block);
    }

    // FIXME: Complete implementation.
    todo!();
  }
}

impl Lower for ast::BlockStmt<'_> {
  fn lower<'ctx>(
    &self,
    generator: LlvmGenerator<'ctx>,
    context: &mut context::Context,
  ) -> inkwell::values::BasicValueEnum<'ctx> {
    crate::pass_assert!(self.llvm_function_like_buffer.is_some());
    crate::pass_assert!(self.llvm_builder_buffer.get_insert_block().is_some());

    let llvm_parent_block = self.llvm_builder_buffer.get_insert_block().unwrap();
    let llvm_temporary_builder = self.llvm_context.create_builder();

    llvm_temporary_builder.position_at_end(llvm_parent_block);

    let llvm_after_block = self
      .llvm_context
      .append_basic_block(self.llvm_function_like_buffer.unwrap(), "block_stmt_after");

    // Push the `after` block before visiting the statement's body.
    self
      .llvm_basic_block_fallthrough_stack
      .push(llvm_after_block);

    // TODO: Cloning LLVM basic block.
    let llvm_new_block = lower_or_retrieve_block!(self, &self.block)?.to_owned();

    if llvm_parent_block.get_terminator().is_none() {
      llvm_temporary_builder.build_unconditional_branch(llvm_new_block);
    }

    llvm_temporary_builder.position_at_end(llvm_new_block);

    if llvm_new_block.get_terminator().is_none() {
      llvm_temporary_builder.build_unconditional_branch(llvm_after_block);
    }

    self.llvm_builder_buffer.position_at_end(llvm_after_block);

    Ok(())
  }
}

impl Lower for ast::BreakStmt {
  fn lower<'ctx>(
    &self,
    generator: LlvmGenerator<'ctx>,
    context: &mut context::Context,
  ) -> inkwell::values::BasicValueEnum<'ctx> {
    // TODO: Must ensure that the current block is a loop block.
    todo!();
  }
}

impl Lower for ast::Module {
  fn lower<'ctx>(
    &self,
    generator: LlvmGenerator<'ctx>,
    context: &mut context::Context,
  ) -> inkwell::values::BasicValueEnum<'ctx> {
    // Reset all buffers when visiting a new module.
    self.llvm_builder_buffer.clear_insertion_position();
    self.llvm_function_like_buffer = None;
    self.module_buffer = Some(module);

    for top_level_node in module.symbol_table.values() {
      match top_level_node {
        ast::TopLevelNodeHolder::Function(function) => self.lower_function(function)?,
        ast::TopLevelNodeHolder::External(external) => self.lower_external(external)?,
      };
    }

    Ok(())
  }
}

struct LlvmGenerator<'ctx> {
  llvm_context: &'ctx inkwell::context::Context,
  llvm_module: inkwell::module::Module<'ctx>,
  definitions:
    std::collections::HashMap<context::DefinitionKey, inkwell::values::BasicValueEnum<'ctx>>,
}

impl<'ctx> LlvmGenerator<'ctx> {
  fn new(
    llvm_context: &'ctx inkwell::context::Context,
    llvm_module: inkwell::module::Module<'ctx>,
  ) -> Self {
    Self {
      llvm_context,
      llvm_module,
      definitions: std::collections::HashMap::new(),
    }
  }
}

fn mangle_name(scope_name: &String, name: &String) -> String {
  format!(".{}.{}", scope_name, name)
}

// #[cfg(test)]
// mod tests {
//   use super::*;

//   #[test]
//   fn proper_initial_values<'a>() {
//     let llvm_context = inkwell::context::Context::create();
//     let llvm_module: inkwell::module::Module<'_> = llvm_context.create_module("test");

//     assert_eq!(
//       true,
//       LlvmLowering::new(&llvm_context, &llvm_module)
//         .llvm_type_map
//         .is_empty()
//     );
//   }

//   // TODO:
//   // #[test]
//   // fn visit_or_retrieve_type() {
//   //   let llvm_context = inkwell::context::Context::create();
//   //   let llvm_module = llvm_context.create_module("test");
//   //   let mut llvm_lowering_pass = LlvmLoweringPass::new(&llvm_context, &llvm_module);

//   //   let int_kind = node::KindTransport::IntKind(&int_kind::IntKind {
//   //     size: int_kind::IntSize::Bit32,
//   //     is_signed: true,
//   //   });

//   //   let visit_or_retrieve_result = visit_or_retrieve_type!(llvm_lowering_pass, &int_kind);

//   //   assert_eq!(true, visit_or_retrieve_result.is_ok());
//   //   assert_eq!(true, visit_or_retrieve_result.ok().is_some());
//   //   assert_eq!(1, llvm_lowering_pass.llvm_type_map.len());
//   // }

//   #[test]
//   fn visit_int_kind() {
//     let llvm_context = inkwell::context::Context::create();
//     let llvm_module = llvm_context.create_module("test");
//     let mut llvm_lowering_pass = LlvmLowering::new(&llvm_context, &llvm_module);

//     let visit_int_kind_result = llvm_lowering_pass.lower_int_kind(&int_kind::IntKind {
//       size: int_kind::IntSize::Size32,
//       is_signed: true,
//     });

//     assert_eq!(true, visit_int_kind_result.is_ok());
//     assert_eq!(llvm_lowering_pass.llvm_type_map.len(), 1);
//   }

//   #[test]
//   fn visit_function() {
//     let llvm_context = inkwell::context::Context::create();
//     let llvm_module = llvm_context.create_module("test");
//     let mut llvm_lowering_pass = LlvmLowering::new(&llvm_context, &llvm_module);
//     let module = ast::Module::new("test");

//     llvm_lowering_pass.module_buffer = Some(&module);

//     let function = ast::Function {
//       prototype: ast::Prototype {
//         name: "foo".to_string(),
//         return_kind_group: None,
//         parameters: vec![],
//         is_variadic: false,
//       },
//       body: ast::Block {
//         llvm_name: "entry".to_string(),

//         statements: vec![ast::AnyStmtNode::ReturnStmt(ast::ReturnStmt {
//           value: None,
//         })],
//       },
//     };

//     let visit_function_result = llvm_lowering_pass.lower_function(&function);

//     assert_eq!(true, visit_function_result.is_ok());
//     assert_eq!(true, llvm_lowering_pass.llvm_function_like_buffer.is_some());
//   }

//   // TODO: Add more tests: `visit_prototype()`, etc.
// }
