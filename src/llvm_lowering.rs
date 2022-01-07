use crate::{ast, context, dispatch};
use inkwell::{types::BasicType, values::BasicValue};
use std::convert::TryFrom;

pub trait Lower {
  // TODO: Make return type `Option<inkwell::values::BasicValueEnum<'ctx>>`.
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

impl Lower for ast::Enum {
  fn lower<'a, 'ctx>(
    &self,
    generator: &mut LlvmGenerator<'a, 'ctx>,
    _context: &mut context::Context,
  ) -> inkwell::values::BasicValueEnum<'ctx> {
    for (index, name) in self.variants.iter().enumerate() {
      let llvm_variant_global = generator.llvm_module.add_global(
        generator.llvm_context.i32_type(),
        Some(inkwell::AddressSpace::Const),
        format!("{}.{}", self.name, name).as_str(),
      );

      llvm_variant_global.set_initializer(
        &generator
          .llvm_context
          .i32_type()
          .const_int(index as u64, false),
      );
    }

    generator.make_unit_value()
  }
}

impl Lower for ast::VariableAssignStmt {
  fn lower<'a, 'ctx>(
    &self,
    generator: &mut LlvmGenerator<'a, 'ctx>,
    context: &mut context::Context,
  ) -> inkwell::values::BasicValueEnum<'ctx> {
    let llvm_value = self.value.lower(generator, context);
    let definition_key = self.definition_key.unwrap();

    // TODO: Here we should only be retrieving, no memoization should be done by this point (variables are declared top-down).
    let llvm_variable = generator.memoize_or_retrieve(definition_key, context);

    let llvm_new_variable = generator.llvm_builder.build_alloca(
      llvm_variable.get_type(),
      format!("{}.assign", self.name).as_str(),
    );

    // FIXME: This part is causing problems (segfault).
    let llvm_new_variable_ptr = generator
      .llvm_builder
      .build_load(
        llvm_new_variable,
        format!("{}.assign.ptr", self.name).as_str(),
      )
      .into_pointer_value();

    generator
      .llvm_builder
      .build_store(llvm_new_variable_ptr, llvm_value);

    // TODO: Shouldn't the declaration be replaced as well? Think this through.
    // Remove the original variable.
    generator.definitions.remove(&definition_key);

    // TODO: We're inserting a `load` instruction value here, yet on variable references we insert an `alloca` instruction. Research.
    // Replace it with the new one.
    generator
      .definitions
      .insert(definition_key, llvm_new_variable_ptr.as_basic_value_enum());

    generator.make_unit_value()
  }
}

impl Lower for ast::ContinueStmt {
  fn lower<'a, 'ctx>(
    &self,
    generator: &mut LlvmGenerator<'a, 'ctx>,
    _context: &mut context::Context,
  ) -> inkwell::values::BasicValueEnum<'ctx> {
    generator
      .llvm_builder
      .build_unconditional_branch(generator.get_current_block());

    generator.make_unit_value()
  }
}

impl Lower for ast::ArrayIndexing {
  fn lower<'a, 'ctx>(
    &self,
    generator: &mut LlvmGenerator<'a, 'ctx>,
    context: &mut context::Context,
  ) -> inkwell::values::BasicValueEnum<'ctx> {
    let _llvm_index = self.index.lower(generator, context).into_int_value();
    let _llvm_target_array = generator.memoize_or_retrieve(self.definition_key.unwrap(), context);

    // TODO: Implement.
    todo!();
    // generator
    //   .llvm_builder
    //   .build_extract_value(
    //     llvm_target_array.into_array_value(),
    //     llvm_index,
    //     "array_indexing",
    //   )
    //   .unwrap()
  }
}

impl Lower for ast::ArrayValue {
  fn lower<'a, 'ctx>(
    &self,
    generator: &mut LlvmGenerator<'a, 'ctx>,
    context: &mut context::Context,
  ) -> inkwell::values::BasicValueEnum<'ctx> {
    let mut llvm_values = Vec::new();

    llvm_values.reserve(self.elements.len());

    for element in &self.elements {
      llvm_values.push(element.lower(generator, context));
    }

    // TODO: Is this cast (usize -> u32) safe?
    let llvm_array_type = if llvm_values.is_empty() {
      generator.lower_type(self.explicit_type.as_ref().unwrap())
    } else {
      llvm_values.first().unwrap().get_type()
    }
    .array_type(llvm_values.len() as u32);

    let llvm_array_alloca = generator
      .llvm_builder
      .build_alloca(llvm_array_type, "array_value");

    let llvm_array_ref = generator
      .llvm_builder
      .build_load(llvm_array_alloca, "array_load")
      .into_array_value();

    // TODO: Double loop is redundant (adds complexity). With a single one should be fine. Re-implement.
    for (index, llvm_value) in llvm_values.iter().enumerate() {
      generator.llvm_builder.build_insert_value(
        llvm_array_ref,
        llvm_value.clone(),
        // TODO: Is this cast safe?
        index as u32,
        "array_init",
      );
    }

    llvm_array_ref.as_basic_value_enum()
  }
}

impl Lower for ast::Parameter {
  fn lower<'a, 'ctx>(
    &self,
    generator: &mut LlvmGenerator<'a, 'ctx>,
    _context: &mut context::Context,
  ) -> inkwell::values::BasicValueEnum<'ctx> {
    // TODO: In the future, consider having an `alloca` per parameter, for simpler tracking of them.
    generator
      .llvm_function_buffer
      .unwrap()
      .get_nth_param(self.2)
      .unwrap()
  }
}

impl Lower for ast::ArrayAssignStmt {
  fn lower<'a, 'ctx>(
    &self,
    generator: &mut LlvmGenerator<'a, 'ctx>,
    context: &mut context::Context,
  ) -> inkwell::values::BasicValueEnum<'ctx> {
    // FIXME: This only works when assigning full arrays (not elements on their indexes them).

    let llvm_array_variable = generator.memoize_or_retrieve(self.definition_key.unwrap(), context);
    let llvm_value = self.value.lower(generator, context);

    // TODO: What if the array variable is a not a pointer value? This must be enforced by the type-checker.
    generator
      .llvm_builder
      .build_store(llvm_array_variable.into_pointer_value(), llvm_value);

    generator.make_unit_value()
  }
}

impl Lower for ast::UnsafeBlock {
  fn lower<'a, 'ctx>(
    &self,
    generator: &mut LlvmGenerator<'a, 'ctx>,
    context: &mut context::Context,
  ) -> inkwell::values::BasicValueEnum<'ctx> {
    self.0.lower(generator, context)
  }
}

impl Lower for ast::BinaryExpr {
  fn lower<'a, 'ctx>(
    &self,
    generator: &mut LlvmGenerator<'a, 'ctx>,
    context: &mut context::Context,
  ) -> inkwell::values::BasicValueEnum<'ctx> {
    let llvm_left_value = self.left.lower(generator, context);
    let llvm_right_value = self.right.lower(generator, context);

    // NOTE: By this point, we assume that both values are of the same type.
    let is_int_values = llvm_left_value.is_int_value();

    // TODO: Make use of.
    // let is_signed = llvm_left_value.into_int_value().get_sign_extended_constant();

    match self.operator {
      ast::OperatorKind::Add if is_int_values => generator
        .llvm_builder
        .build_int_add(
          llvm_left_value.into_int_value(),
          llvm_right_value.into_int_value(),
          "int_add_op",
        )
        .as_basic_value_enum(),
      ast::OperatorKind::Add => generator
        .llvm_builder
        .build_float_add(
          llvm_left_value.into_float_value(),
          llvm_right_value.into_float_value(),
          "float_add_op",
        )
        .as_basic_value_enum(),
      ast::OperatorKind::Subtract if is_int_values => generator
        .llvm_builder
        .build_int_sub(
          llvm_left_value.into_int_value(),
          llvm_right_value.into_int_value(),
          "int_subtract_op",
        )
        .as_basic_value_enum(),
      ast::OperatorKind::Subtract => generator
        .llvm_builder
        .build_float_sub(
          llvm_left_value.into_float_value(),
          llvm_right_value.into_float_value(),
          "float_subtract_op",
        )
        .as_basic_value_enum(),
      ast::OperatorKind::Multiply if is_int_values => generator
        .llvm_builder
        .build_int_mul(
          llvm_left_value.into_int_value(),
          llvm_right_value.into_int_value(),
          "int_multiply_op",
        )
        .as_basic_value_enum(),
      ast::OperatorKind::Multiply => generator
        .llvm_builder
        .build_float_mul(
          llvm_left_value.into_float_value(),
          llvm_right_value.into_float_value(),
          "float_multiply_op",
        )
        .as_basic_value_enum(),
      // TODO: What if there's division by zero?
      // TODO: Support for unsgined division?
      ast::OperatorKind::Divide if is_int_values => generator
        .llvm_builder
        .build_int_signed_div(
          llvm_left_value.into_int_value(),
          llvm_right_value.into_int_value(),
          "int_divide_op",
        )
        .as_basic_value_enum(),
      ast::OperatorKind::Divide => generator
        .llvm_builder
        .build_float_div(
          llvm_left_value.into_float_value(),
          llvm_right_value.into_float_value(),
          "float_divide_op",
        )
        .as_basic_value_enum(),
      ast::OperatorKind::LessThan if is_int_values => generator
        .llvm_builder
        .build_int_compare(
          // TODO: Support for unsigned?
          inkwell::IntPredicate::SLT,
          llvm_left_value.into_int_value(),
          llvm_right_value.into_int_value(),
          "int_less_than_op",
        )
        .as_basic_value_enum(),
      ast::OperatorKind::LessThan => generator
        .llvm_builder
        .build_float_compare(
          inkwell::FloatPredicate::OLT,
          llvm_left_value.into_float_value(),
          llvm_right_value.into_float_value(),
          "float_less_than_op",
        )
        .as_basic_value_enum(),
      ast::OperatorKind::GreaterThan if is_int_values => generator
        .llvm_builder
        .build_int_compare(
          // TODO: Support for unsigned?
          inkwell::IntPredicate::SGT,
          llvm_left_value.into_int_value(),
          llvm_right_value.into_int_value(),
          "int_greater_than_op",
        )
        .as_basic_value_enum(),
      ast::OperatorKind::GreaterThan => generator
        .llvm_builder
        .build_float_compare(
          inkwell::FloatPredicate::OGT,
          llvm_left_value.into_float_value(),
          llvm_right_value.into_float_value(),
          "float_greater_than_op",
        )
        .as_basic_value_enum(),
      ast::OperatorKind::LessThanOrEqual if is_int_values => generator
        .llvm_builder
        .build_int_compare(
          inkwell::IntPredicate::SLE,
          llvm_left_value.into_int_value(),
          llvm_right_value.into_int_value(),
          "int_less_than_or_equal_op",
        )
        .as_basic_value_enum(),
      ast::OperatorKind::LessThanOrEqual => generator
        .llvm_builder
        .build_float_compare(
          inkwell::FloatPredicate::OLE,
          llvm_left_value.into_float_value(),
          llvm_right_value.into_float_value(),
          "float_less_than_or_equal_op",
        )
        .as_basic_value_enum(),
      ast::OperatorKind::GreaterThanOrEqual if is_int_values => generator
        .llvm_builder
        .build_int_compare(
          inkwell::IntPredicate::SGE,
          llvm_left_value.into_int_value(),
          llvm_right_value.into_int_value(),
          "int_greater_than_or_equal_op",
        )
        .as_basic_value_enum(),
      ast::OperatorKind::GreaterThanOrEqual => generator
        .llvm_builder
        .build_float_compare(
          inkwell::FloatPredicate::OGE,
          llvm_left_value.into_float_value(),
          llvm_right_value.into_float_value(),
          "float_greater_than_or_equal_op",
        )
        .as_basic_value_enum(),
      // TODO: Support for all operators.
      _ => todo!(),
    }
  }
}

impl Lower for ast::VariableRef {
  fn lower<'a, 'ctx>(
    &self,
    generator: &mut LlvmGenerator<'a, 'ctx>,
    context: &mut context::Context,
  ) -> inkwell::values::BasicValueEnum<'ctx> {
    let llvm_variable = generator.memoize_or_retrieve(self.definition_key.unwrap(), context);

    // TODO: This logic is here to make up for parameters not being pointer values. Is this okay?
    if llvm_variable.is_pointer_value() {
      generator.llvm_builder.build_load(
        llvm_variable.into_pointer_value(),
        format!("{}.load", self.name).as_str(),
      )
    } else {
      llvm_variable
    }
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
      .append_basic_block(llvm_current_function, "while.then");

    let llvm_after_block = generator
      .llvm_context
      .append_basic_block(llvm_current_function, "while.after");

    generator.llvm_builder.build_conditional_branch(
      llvm_condition,
      llvm_then_block,
      llvm_after_block,
    );

    generator.llvm_builder.position_at_end(llvm_then_block);
    generator.next_block = Some(llvm_after_block);
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
      .append_basic_block(llvm_current_function, "if.then");

    let llvm_after_block = generator
      .llvm_context
      .append_basic_block(llvm_current_function, "if.after");

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
            value.clone(),
            match integer_kind {
              ast::IntSize::I8
              | ast::IntSize::I16
              | ast::IntSize::I32
              | ast::IntSize::I64
              | ast::IntSize::Isize => true,
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
          // TODO: Is this cloning?
          // TODO: Simplify this process. Maybe via implementing a `From` trait?
          let parameter_node = &*parameters.get(i).unwrap().node.borrow();

          let parameter = match parameter_node {
            ast::Node::Parameter(parameter) => parameter,
            _ => unreachable!(),
          };

          parameter.lower(generator, context);

          // TODO: Ensure this access is safe and checked.
          let (parameter_name, _, _) = parameter;

          llvm_parameter.set_name(parameter_name.as_str());
        }
      }
      _ => unreachable!(),
    };

    let llvm_entry_block = generator
      .llvm_context
      .append_basic_block(llvm_function, "fn.entry");

    generator.llvm_builder.position_at_end(llvm_entry_block);
    self.body.lower(generator, context);

    // Build return void instruction if the function doesn't return.
    if generator.get_current_block().get_terminator().is_none() {
      generator.llvm_builder.build_return(None);
    }

    // Verify the LLVM function to be well-formed.
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

      // Do not continue lowering statements if the current block is terminated.
      if generator.get_current_block().get_terminator().is_some() {
        break;
      }
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

    let llvm_alloca_inst_ptr = generator
      .llvm_builder
      .build_alloca(llvm_type, self.name.as_str());

    let llvm_value = self.value.lower(generator, context);

    // TODO: Shouldn't there be a load instruction first?
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

    let llvm_call_value = generator.llvm_builder.build_call(
      inkwell::values::CallableValue::try_from(llvm_target_function).unwrap(),
      llvm_arguments.as_slice(),
      format!("{}.call", self.callee_name).as_str(),
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
    generator: &mut LlvmGenerator<'a, 'ctx>,
    _context: &mut context::Context,
  ) -> inkwell::values::BasicValueEnum<'ctx> {
    // TODO: What happens if there are nested loops? Will the buffer be overwritten?
    // NOTE: By this point, we assume that whether we're actually in a loop was handled by the type-checker.
    generator
      .llvm_builder
      .build_unconditional_branch(generator.next_block.unwrap());

    generator.make_unit_value()
  }
}

impl Lower for ast::Definition {
  fn lower<'a, 'ctx>(
    &self,
    generator: &mut LlvmGenerator<'a, 'ctx>,
    context: &mut context::Context,
  ) -> inkwell::values::BasicValueEnum<'ctx> {
    generator.memoize_or_retrieve(self.key, context)
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
  // TODO: Shouldn't this be a vector instead?
  definitions:
    std::collections::HashMap<context::DefinitionKey, inkwell::values::BasicValueEnum<'ctx>>,
  next_block: Option<inkwell::basic_block::BasicBlock<'ctx>>,
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
      next_block: None,
    }
  }

  fn lower_type(&self, ty: &ast::Type) -> inkwell::types::BasicTypeEnum<'ctx> {
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
      ast::Type::Array(_element_type, size) => self
        .lower_type(&_element_type)
        .array_type(size.clone())
        .as_basic_type_enum(),
      ast::Type::Prototype(parameter_types, return_type_result, is_variadic) => {
        let llvm_parameter_types = parameter_types
          .iter()
          .map(|parameter_type| {
            self
              .lower_type(
                // TODO: Simplify. Also, is this cloning?
                &match &*parameter_type.node.borrow() {
                  ast::Node::Parameter(parameter) => parameter,
                  _ => unreachable!(),
                }
                .1,
              )
              .into()
          })
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
            .void_type()
            // TODO: Is `is_variadic` being copied?
            .fn_type(llvm_parameter_types.as_slice(), *is_variadic)
            .ptr_type(inkwell::AddressSpace::Generic)
            .as_basic_type_enum()
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
      let previous_block = self.llvm_builder.get_insert_block();

      // If the definition is not already memoized, memoize it.
      // This retrieval will panic in case of a logic error (internal error).
      let result = std::rc::Rc::clone(context.declarations.get_mut(&definition_key).unwrap())
        .borrow_mut()
        .lower(self, context);

      // TODO: Should we be inserting the definition key here?
      self.definitions.insert(definition_key, result);

      // Restore buffers after processing.
      if let Some(previous_block) = previous_block {
        self.llvm_builder.position_at_end(previous_block);
      }

      self.llvm_function_buffer = previous_function_buffer;

      return result;
    }

    // FIXME: Is this actually the case?
    // NOTE: The LLVM value is not copied, but rather the reference to it.
    self.definitions.get(&definition_key).unwrap().clone()
  }
}

// TODO: Receive scope path as `std::path::PathBuf` instead?
fn mangle_name(scope_name: &String, name: &String) -> String {
  format!(".{}.{}", scope_name, name)
}
