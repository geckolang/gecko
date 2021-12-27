use crate::{ast, context, dispatch};
use inkwell::values::BasicValue;
use std::convert::TryFrom;

pub trait Lower {
  fn lower<'a, 'ctx>(
    &self,
    generator: &mut LlvmGenerator<'a, 'ctx>,
    context: &mut context::Context,
  ) -> inkwell::values::BasicValueEnum<'ctx>;
}

impl Lower for ast::Node {
  fn lower<'a, 'ctx>(
    &self,
    generator: &mut LlvmGenerator<'a, 'ctx>,
    context: &mut context::Context,
  ) -> inkwell::values::BasicValueEnum<'ctx> {
    dispatch!(self, Lower::lower, generator, context)
  }
}

impl Lower for ast::VariableRef {
  fn lower<'a, 'ctx>(
    &self,
    generator: &mut LlvmGenerator<'a, 'ctx>,
    context: &mut context::Context,
  ) -> inkwell::values::BasicValueEnum<'ctx> {
    let llvm_variable = generator.memoize_or_retrieve(self.definition_key.unwrap(), context);

    generator
      .llvm_builder
      .build_load(llvm_variable.into_pointer_value(), "variable_ref")
  }
}

impl Lower for ast::WhileStmt {
  fn lower<'a, 'ctx>(
    &self,
    generator: &mut LlvmGenerator<'a, 'ctx>,
    context: &mut context::Context,
  ) -> inkwell::values::BasicValueEnum<'ctx> {
    // NOTE: At this point, the condition should be verified to be a boolean by the type-checker.
    let llvm_condition = self.condition.lower(generator, context).into_int_value();
    let llvm_current_function = generator.llvm_function_buffer.unwrap();

    let llvm_then_block = generator
      .llvm_context
      .append_basic_block(llvm_current_function, "while_then");

    let llvm_after_block = generator
      .llvm_context
      .append_basic_block(llvm_current_function, "while_after");

    generator.llvm_builder.build_conditional_branch(
      llvm_condition,
      llvm_then_block,
      llvm_after_block,
    );

    generator.llvm_builder.position_at_end(llvm_then_block);
    self.body.lower(generator, context);

    // Fallthrough or loop if applicable.
    if llvm_then_block.get_terminator().is_none() {
      generator.llvm_builder.position_at_end(llvm_then_block);

      generator.llvm_builder.build_conditional_branch(
        llvm_condition,
        llvm_then_block,
        llvm_after_block,
      );
    }

    generator.llvm_builder.position_at_end(llvm_after_block);

    generator.make_unit_value()
  }
}

impl Lower for ast::IfStmt {
  fn lower<'a, 'ctx>(
    &self,
    generator: &mut LlvmGenerator<'a, 'ctx>,
    context: &mut context::Context,
  ) -> inkwell::values::BasicValueEnum<'ctx> {
    // TODO: Add logic for the `else` branch.

    let llvm_condition = self.condition.lower(generator, context);
    let llvm_current_function = generator.llvm_function_buffer.unwrap();

    let llvm_then_block = generator
      .llvm_context
      .append_basic_block(llvm_current_function, "if_then");

    let llvm_after_block = generator
      .llvm_context
      .append_basic_block(llvm_current_function, "if_after");

    // NOTE: At this point, the condition must be verified to be a boolean by the type-checker.
    generator.llvm_builder.build_conditional_branch(
      llvm_condition.into_int_value(),
      llvm_then_block,
      llvm_after_block,
    );

    generator.llvm_builder.position_at_end(llvm_then_block);
    self.then_block.lower(generator, context);

    // Fallthrough if applicable.
    if llvm_then_block.get_terminator().is_none() {
      generator
        .llvm_builder
        .build_unconditional_branch(llvm_after_block);
    }

    generator.llvm_builder.position_at_end(llvm_after_block);

    generator.make_unit_value()
  }
}

impl Lower for ast::Literal {
  fn lower<'a, 'ctx>(
    &self,
    generator: &mut LlvmGenerator<'a, 'ctx>,
    _: &mut context::Context,
  ) -> inkwell::values::BasicValueEnum<'ctx> {
    match self {
      ast::Literal::Int(value, integer_kind) => {
        let llvm_int_type = generator
          .llvm_context
          .custom_width_int_type(match integer_kind {
            ast::IntSize::I8 | ast::IntSize::U8 => 8,
            ast::IntSize::I16 | ast::IntSize::U16 => 16,
            ast::IntSize::I32 | ast::IntSize::U32 => 32,
            ast::IntSize::I64 | ast::IntSize::U64 => 64,
            ast::IntSize::Isize | ast::IntSize::Usize => 128,
          });

        llvm_int_type
          .const_int(
            // TODO: Is this cloning?
            *value,
            match integer_kind {
              ast::IntSize::I8 => true,
              _ => false,
            },
          )
          .as_basic_value_enum()
      }
      ast::Literal::Char(value) => generator
        .llvm_context
        .i8_type()
        // TODO: Is this cloning?
        .const_int(*value as u64, false)
        .as_basic_value_enum(),
      // TODO: Process all literals.
      ast::Literal::Bool(value) => generator
        .llvm_context
        .bool_type()
        // TODO: Is this cloning?
        .const_int(*value as u64, false)
        .as_basic_value_enum(),
      ast::Literal::String(value) => generator
        .llvm_builder
        .build_global_string_ptr(value.as_str(), "string_literal")
        .as_basic_value_enum(),
    }
  }
}

impl Lower for ast::Function {
  fn lower<'a, 'ctx>(
    &self,
    generator: &mut LlvmGenerator<'a, 'ctx>,
    context: &mut context::Context,
  ) -> inkwell::values::BasicValueEnum<'ctx> {
    let llvm_function_type = generator
      .lower_type(&self.prototype)
      .into_pointer_type()
      .get_element_type();

    assert!(llvm_function_type.is_function_type());

    let is_main = self.name == "main";

    let llvm_function_name = if is_main {
      // TODO: Name being cloned. Is this okay?
      self.name.to_owned()
    } else {
      mangle_name(&generator.module_name, &self.name)
    };

    // FIXME: Function is emitted twice when being called. This is because first the function is lowered when it is invoked, then again during reaching its declaration. This is temporary fix.
    if let Some(llvm_existing_function) = generator
      .llvm_module
      .get_function(llvm_function_name.as_str())
    {
      return llvm_existing_function
        .as_global_value()
        .as_basic_value_enum();
    }

    let llvm_function = generator.llvm_module.add_function(
      llvm_function_name.as_str(),
      llvm_function_type.into_function_type(),
      Some(if is_main {
        inkwell::module::Linkage::External
      } else {
        inkwell::module::Linkage::Private
      }),
    );

    generator.llvm_function_buffer = Some(llvm_function);

    match &self.prototype {
      ast::Type::Prototype(parameters, _, _) => {
        // TODO: Find a way to use only one loop to process both local parameters and LLVM's names.
        for (i, ref mut llvm_parameter) in llvm_function.get_param_iter().enumerate() {
          // TODO: Ensure this access is safe and checked.
          let (parameter_name, _) = parameters.get(i).unwrap();

          llvm_parameter.set_name(parameter_name.as_str());
        }
      }
      _ => unreachable!(),
    };

    let llvm_entry_block = generator
      .llvm_context
      .append_basic_block(llvm_function, "fn_entry");

    generator.llvm_builder.position_at_end(llvm_entry_block);
    self.body.lower(generator, context);

    // Build return void instruction if the function doesn't return.
    if generator.get_current_block().get_terminator().is_none() {
      generator.llvm_builder.build_return(None);
    }

    assert!(llvm_function.verify(false));

    llvm_function.as_global_value().as_basic_value_enum()
  }
}

impl Lower for ast::Extern {
  fn lower<'a, 'ctx>(
    &self,
    generator: &mut LlvmGenerator<'a, 'ctx>,
    _: &mut context::Context,
  ) -> inkwell::values::BasicValueEnum<'ctx> {
    let llvm_function_type = generator
      .lower_type(&self.prototype)
      .into_pointer_type()
      .get_element_type();

    assert!(llvm_function_type.is_function_type());

    let llvm_external_function = generator.llvm_module.add_function(
      self.name.as_str(),
      llvm_function_type.into_function_type(),
      Some(inkwell::module::Linkage::External),
    );

    assert!(llvm_external_function.verify(false));

    llvm_external_function
      .as_global_value()
      .as_basic_value_enum()
  }
}

impl Lower for ast::Block {
  fn lower<'a, 'ctx>(
    &self,
    generator: &mut LlvmGenerator<'a, 'ctx>,
    context: &mut context::Context,
  ) -> inkwell::values::BasicValueEnum<'ctx> {
    for statement in &self.statements {
      statement.lower(generator, context);
    }

    generator.make_unit_value()
  }
}

impl Lower for ast::ReturnStmt {
  fn lower<'a, 'ctx>(
    &self,
    generator: &mut LlvmGenerator<'a, 'ctx>,
    context: &mut context::Context,
  ) -> inkwell::values::BasicValueEnum<'ctx> {
    let llvm_return_value = if let Some(return_value) = &self.value {
      Some(return_value.lower(generator, context))
    } else {
      None
    };

    generator.build_return(llvm_return_value);

    generator.make_unit_value()
  }
}

impl Lower for ast::LetStmt {
  fn lower<'a, 'ctx>(
    &self,
    generator: &mut LlvmGenerator<'a, 'ctx>,
    context: &mut context::Context,
  ) -> inkwell::values::BasicValueEnum<'ctx> {
    let llvm_type = generator.lower_type(&self.ty);

    // TODO: Finish implementing.
    let llvm_alloca_inst_ptr = generator
      .llvm_builder
      .build_alloca(llvm_type, self.name.as_str());

    let llvm_value = self.value.lower(generator, context);

    generator
      .llvm_builder
      .build_store(llvm_alloca_inst_ptr, llvm_value);

    llvm_alloca_inst_ptr.as_basic_value_enum()
  }
}

impl Lower for ast::FunctionCall {
  fn lower<'a, 'ctx>(
    &self,
    generator: &mut LlvmGenerator<'a, 'ctx>,
    context: &mut context::Context,
  ) -> inkwell::values::BasicValueEnum<'ctx> {
    let llvm_arguments = self
      .arguments
      .iter()
      .map(|argument| argument.lower(generator, context).into())
      .collect::<Vec<_>>();

    let llvm_target_function = generator
      .memoize_or_retrieve(self.callee_definition_key.unwrap(), context)
      .into_pointer_value();

    // TODO: Calling the current function for debugging.
    let llvm_call_value = generator.llvm_builder.build_call(
      inkwell::values::CallableValue::try_from(llvm_target_function).unwrap(),
      llvm_arguments.as_slice(),
      "fn_call_result",
    );

    let llvm_call_basic_value_result = llvm_call_value.try_as_basic_value();

    if llvm_call_basic_value_result.is_left() {
      llvm_call_basic_value_result.left().unwrap()
    } else {
      generator.make_unit_value()
    }
  }
}

impl Lower for ast::BreakStmt {
  fn lower<'a, 'ctx>(
    &self,
    _generator: &mut LlvmGenerator<'a, 'ctx>,
    _context: &mut context::Context,
  ) -> inkwell::values::BasicValueEnum<'ctx> {
    // TODO: Must ensure that the current block is a loop block. Additionally, will also need access to the next block in advance (maybe using a buffer?).
    todo!();
  }
}

impl Lower for ast::Definition {
  fn lower<'a, 'ctx>(
    &self,
    generator: &mut LlvmGenerator<'a, 'ctx>,
    context: &mut context::Context,
  ) -> inkwell::values::BasicValueEnum<'ctx> {
    if generator.definitions.contains_key(&self.key) {
      // NOTE: The underlying LLVM value is not actually cloned, but rather its reference.
      return generator.definitions.get(&self.key).unwrap().clone();
    }

    let llvm_value = self.node.borrow_mut().lower(generator, context);

    generator.definitions.insert(self.key, llvm_value);

    llvm_value
  }
}

impl Lower for ast::ExprWrapperStmt {
  fn lower<'a, 'ctx>(
    &self,
    generator: &mut LlvmGenerator<'a, 'ctx>,
    context: &mut context::Context,
  ) -> inkwell::values::BasicValueEnum<'ctx> {
    self.expr.lower(generator, context)
  }
}

pub struct LlvmGenerator<'a, 'ctx> {
  module_name: String,
  llvm_context: &'ctx inkwell::context::Context,
  llvm_module: &'a inkwell::module::Module<'ctx>,
  llvm_builder: inkwell::builder::Builder<'ctx>,
  llvm_function_buffer: Option<inkwell::values::FunctionValue<'ctx>>,
  definitions:
    std::collections::HashMap<context::DefinitionKey, inkwell::values::BasicValueEnum<'ctx>>,
}

impl<'a, 'ctx> LlvmGenerator<'a, 'ctx> {
  pub fn new(
    module_name: String,
    llvm_context: &'ctx inkwell::context::Context,
    llvm_module: &'a inkwell::module::Module<'ctx>,
  ) -> Self {
    Self {
      module_name,
      llvm_context,
      llvm_module,
      llvm_builder: llvm_context.create_builder(),
      llvm_function_buffer: None,
      definitions: std::collections::HashMap::new(),
    }
  }

  fn lower_type(&self, ty: &ast::Type) -> inkwell::types::BasicTypeEnum<'ctx> {
    use inkwell::types::BasicType;

    match ty {
      ast::Type::PrimitiveType(primitive_type) => match primitive_type {
        ast::PrimitiveType::Bool => self.llvm_context.bool_type().as_basic_type_enum(),
        ast::PrimitiveType::Int(size) => {
          // TODO: Should we handle unsigned integers here?

          let llvm_int_type = self.llvm_context.custom_width_int_type(match size {
            ast::IntSize::I8 | ast::IntSize::U8 => 8,
            ast::IntSize::I16 | ast::IntSize::U16 => 16,
            ast::IntSize::I32 | ast::IntSize::U32 => 32,
            ast::IntSize::I64 | ast::IntSize::U64 => 64,
            ast::IntSize::Isize | ast::IntSize::Usize => 128,
          });

          llvm_int_type.as_basic_type_enum()
        }
        ast::PrimitiveType::Char => self.llvm_context.i8_type().as_basic_type_enum(),
        ast::PrimitiveType::String => self
          .llvm_context
          .i8_type()
          .ptr_type(inkwell::AddressSpace::Generic)
          .as_basic_type_enum(),
      },
      ast::Type::Prototype(parameter_types, return_type_result, is_variadic) => {
        let llvm_parameter_types = parameter_types
          .iter()
          .map(|parameter_type| self.lower_type(&parameter_type.1).into())
          .collect::<Vec<_>>();

        // TODO: Simplify code (find common ground between `void` and `basic` types).
        if let Some(return_type) = return_type_result {
          self
            .lower_type(&return_type)
            // TODO: Is `is_variadic` being copied?
            .fn_type(llvm_parameter_types.as_slice(), *is_variadic)
            .ptr_type(inkwell::AddressSpace::Generic)
            .into()
        } else {
          self
            .llvm_context
            .void_type() // TODO: Is `is_variadic` being copied?
            .fn_type(llvm_parameter_types.as_slice(), *is_variadic)
            .ptr_type(inkwell::AddressSpace::Generic)
            .into()
        }
      }
    }
  }

  fn make_unit_value(&self) -> inkwell::values::BasicValueEnum<'ctx> {
    // TODO: In our case, this might not be ideal.
    self.llvm_context.bool_type().const_int(0, false).into()
  }

  fn get_current_block(&self) -> inkwell::basic_block::BasicBlock<'ctx> {
    self.llvm_builder.get_insert_block().unwrap()
  }

  fn build_return(&mut self, return_value: Option<inkwell::values::BasicValueEnum<'ctx>>) {
    // TODO: Consider mixing this with the function's terminator check, for void functions?
    // Only build a single return instruction per block.
    if self.get_current_block().get_terminator().is_some() {
      return;
    } else if let Some(return_value) = return_value {
      self.llvm_builder.build_return(Some(&return_value));
    } else {
      self.llvm_builder.build_return(None);
    }
  }

  fn memoize_or_retrieve(
    &mut self,
    definition_key: context::DefinitionKey,
    context: &mut context::Context,
  ) -> inkwell::values::BasicValueEnum<'ctx> {
    if !self.definitions.contains_key(&definition_key) {
      // TODO: Will there be a case where we'd need the buffers to change? If so, take in a flag parameter.
      let previous_function_buffer = self.llvm_function_buffer;
      let previous_block = self.get_current_block();

      // If the definition is not already memoized, memoize it.
      // This retrieval will panic in case of a logic error (internal error).
      let result = std::rc::Rc::clone(context.declarations.get_mut(&definition_key).unwrap())
        .borrow_mut()
        .lower(self, context);

      // Restore buffers after processing.
      self.llvm_builder.position_at_end(previous_block);
      self.llvm_function_buffer = previous_function_buffer;

      return result;
    }

    // NOTE: The LLVM value is not copied, but rather the reference to it.
    self.definitions.get(&definition_key).unwrap().clone()
  }
}

// TODO: Receive scope path as `std::path::PathBuf` instead?
fn mangle_name(scope_name: &String, name: &String) -> String {
  format!(".{}.{}", scope_name, name)
}
