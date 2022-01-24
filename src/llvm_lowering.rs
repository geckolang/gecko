use crate::{ast, cache, dispatch};
use inkwell::{types::BasicType, values::BasicValue};
use std::convert::TryFrom;

pub const MAIN_FUNCTION_NAME: &str = "main";

pub trait Lower {
  fn lower<'a, 'ctx>(
    &self,
    _generator: &mut LlvmGenerator<'a, 'ctx>,
    _cache: &mut cache::Cache,
  ) -> Option<inkwell::values::BasicValueEnum<'ctx>> {
    None
  }
}

impl Lower for ast::Node {
  fn lower<'a, 'ctx>(
    &self,
    generator: &mut LlvmGenerator<'a, 'ctx>,
    cache: &mut cache::Cache,
  ) -> Option<inkwell::values::BasicValueEnum<'ctx>> {
    dispatch!(self, Lower::lower, generator, cache)
  }
}

impl Lower for ast::StructValue {
  fn lower<'a, 'ctx>(
    &self,
    generator: &mut LlvmGenerator<'a, 'ctx>,
    cache: &mut cache::Cache,
  ) -> Option<inkwell::values::BasicValueEnum<'ctx>> {
    // TODO: If possible and practical, find a way to remove reliance on the generator's state.

    // FIXME: This is invalid. Struct value might be used in a different context (non-declaration).
    let struct_alloca_ptr = generator.let_stmt_allocation.unwrap().clone();

    // Populate struct fields.
    for (index, field) in self.fields.iter().enumerate() {
      let struct_field_gep = generator
        .llvm_builder
        // TODO: Is this conversion safe?
        .build_struct_gep(struct_alloca_ptr, index as u32, "struct.field.gep")
        .unwrap();

      let llvm_field_value = field.lower(generator, cache).unwrap();

      generator
        .llvm_builder
        // FIXME: For nested structs, they will return `None`.
        .build_store(struct_field_gep, llvm_field_value);
    }

    None
  }
}

impl Lower for ast::Prototype {
  //
}

impl Lower for ast::StructType {
  //
}

impl Lower for ast::Enum {
  fn lower<'a, 'ctx>(
    &self,
    generator: &mut LlvmGenerator<'a, 'ctx>,
    _cache: &mut cache::Cache,
  ) -> Option<inkwell::values::BasicValueEnum<'ctx>> {
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

    None
  }
}

impl Lower for ast::AssignStmt {
  fn lower<'a, 'ctx>(
    &self,
    generator: &mut LlvmGenerator<'a, 'ctx>,
    cache: &mut cache::Cache,
  ) -> Option<inkwell::values::BasicValueEnum<'ctx>> {
    let llvm_value = self.value.lower(generator, cache).unwrap();
    let llvm_target = self.assignee_expr.lower(generator, cache).unwrap();

    generator
      .llvm_builder
      .build_store(llvm_target.into_pointer_value(), llvm_value);

    None
  }
}

impl Lower for ast::ContinueStmt {
  fn lower<'a, 'ctx>(
    &self,
    generator: &mut LlvmGenerator<'a, 'ctx>,
    _cache: &mut cache::Cache,
  ) -> Option<inkwell::values::BasicValueEnum<'ctx>> {
    generator
      .llvm_builder
      .build_unconditional_branch(generator.get_current_block());

    None
  }
}

impl Lower for ast::ArrayIndexing {
  fn lower<'a, 'ctx>(
    &self,
    generator: &mut LlvmGenerator<'a, 'ctx>,
    cache: &mut cache::Cache,
  ) -> Option<inkwell::values::BasicValueEnum<'ctx>> {
    let llvm_index = self.index.lower(generator, cache).unwrap().into_int_value();

    let llvm_target_array = generator.memoize_or_retrieve(self.target_key.unwrap(), cache);

    // TODO: Need a way to handle possible segfaults (due to an index being out-of-bounds).
    unsafe {
      // TODO: Figure out why there's a zero index (may want to look on `https://www.llvm.org/docs/GetElementPtr.html#why-is-the-extra-0-index-required`).
      let first_index = generator.llvm_context.i32_type().const_int(0, false);

      let llvm_gep_ptr = generator.llvm_builder.build_in_bounds_gep(
        llvm_target_array.into_pointer_value(),
        &[first_index, llvm_index],
        "array.index.gep",
      );

      let llvm_load_ptr = generator
        .llvm_builder
        .build_load(llvm_gep_ptr, "array.index.ptr");

      Some(llvm_load_ptr)
    }
  }
}

impl Lower for ast::ArrayValue {
  fn lower<'a, 'ctx>(
    &self,
    generator: &mut LlvmGenerator<'a, 'ctx>,
    cache: &mut cache::Cache,
  ) -> Option<inkwell::values::BasicValueEnum<'ctx>> {
    let mut llvm_values = Vec::new();

    llvm_values.reserve(self.elements.len());

    for element in &self.elements {
      llvm_values.push(element.lower(generator, cache).unwrap());
    }

    // TODO: Is this cast (usize -> u32) safe?
    let llvm_array_type = if llvm_values.is_empty() {
      generator.lower_type(self.explicit_type.as_ref().unwrap(), cache)
    } else {
      llvm_values.first().unwrap().get_type()
    }
    .array_type(llvm_values.len() as u32);

    let llvm_array_alloca = generator
      .llvm_builder
      .build_alloca(llvm_array_type, "array.value");

    // TODO: Two loops in a single function is redundant (adds complexity). With a single one should be fine. Re-implement.
    for (index, llvm_value) in llvm_values.iter().enumerate() {
      let first_index = generator.llvm_context.i32_type().const_int(0, false);

      // TODO: Is this conversion safe?
      let llvm_index = generator
        .llvm_context
        .i32_type()
        .const_int(index as u64, false);

      // FIXME: There is no bounds checking guard being inserted (panic).
      unsafe {
        let llvm_gep = generator.llvm_builder.build_gep(
          llvm_array_alloca,
          &[first_index, llvm_index],
          "array.init",
        );

        generator
          .llvm_builder
          .build_store(llvm_gep, llvm_value.clone());
      }
    }

    // TODO: Might not need to load the array in order to initialize it, but to return it?
    let llvm_array_ptr = generator
      .llvm_builder
      .build_load(llvm_array_alloca, "array.load")
      .into_array_value();

    Some(llvm_array_ptr.as_basic_value_enum())
  }
}

impl Lower for ast::Parameter {
  fn lower<'a, 'ctx>(
    &self,
    generator: &mut LlvmGenerator<'a, 'ctx>,
    _cache: &mut cache::Cache,
  ) -> Option<inkwell::values::BasicValueEnum<'ctx>> {
    // TODO: In the future, consider having an `alloca` per parameter, for simpler tracking of them.
    Some(
      generator
        .llvm_function_buffer
        .unwrap()
        .get_nth_param(self.2)
        .unwrap(),
    )
  }
}

impl Lower for ast::UnsafeBlockStmt {
  fn lower<'a, 'ctx>(
    &self,
    generator: &mut LlvmGenerator<'a, 'ctx>,
    cache: &mut cache::Cache,
  ) -> Option<inkwell::values::BasicValueEnum<'ctx>> {
    self.0.lower(generator, cache);

    None
  }
}

impl Lower for ast::BinaryExpr {
  fn lower<'a, 'ctx>(
    &self,
    generator: &mut LlvmGenerator<'a, 'ctx>,
    cache: &mut cache::Cache,
  ) -> Option<inkwell::values::BasicValueEnum<'ctx>> {
    let llvm_left_value = self.left.lower(generator, cache).unwrap();
    let llvm_right_value = self.right.lower(generator, cache).unwrap();

    // NOTE: By this point, we assume that both values are of the same type.
    let is_int_values = llvm_left_value.is_int_value();

    // TODO: Make use of.
    // let is_signed = llvm_left_value.into_int_value().get_sign_extended_constant();

    Some(match self.operator {
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
      ast::OperatorKind::SubtractOrNegate if is_int_values => generator
        .llvm_builder
        .build_int_sub(
          llvm_left_value.into_int_value(),
          llvm_right_value.into_int_value(),
          "int_subtract_op",
        )
        .as_basic_value_enum(),
      ast::OperatorKind::SubtractOrNegate => generator
        .llvm_builder
        .build_float_sub(
          llvm_left_value.into_float_value(),
          llvm_right_value.into_float_value(),
          "float_subtract_op",
        )
        .as_basic_value_enum(),
      ast::OperatorKind::MultiplyOrDereference if is_int_values => generator
        .llvm_builder
        .build_int_mul(
          llvm_left_value.into_int_value(),
          llvm_right_value.into_int_value(),
          "int_multiply_op",
        )
        .as_basic_value_enum(),
      ast::OperatorKind::MultiplyOrDereference => generator
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
    })
  }
}

impl Lower for ast::VariableRef {
  fn lower<'a, 'ctx>(
    &self,
    generator: &mut LlvmGenerator<'a, 'ctx>,
    cache: &mut cache::Cache,
  ) -> Option<inkwell::values::BasicValueEnum<'ctx>> {
    Some(generator.memoize_or_retrieve(self.target_key.unwrap(), cache))

    // TODO: This removes ability to use pointers at all. For this reason, it was commented out.
    // TODO: This logic is here to make up for parameters not being pointer values. Is this okay?
    // if llvm_variable.is_pointer_value() {
    //   generator.llvm_builder.build_load(
    //     llvm_variable.into_pointer_value(),
    //     format!("{}.load", self.name).as_str(),
    //   )
    // } else {
    //   llvm_variable
    // }
  }
}

impl Lower for ast::WhileStmt {
  fn lower<'a, 'ctx>(
    &self,
    generator: &mut LlvmGenerator<'a, 'ctx>,
    cache: &mut cache::Cache,
  ) -> Option<inkwell::values::BasicValueEnum<'ctx>> {
    // NOTE: At this point, the condition should be verified to be a boolean by the type-checker.
    let llvm_condition = self
      .condition
      .lower(generator, cache)
      .unwrap()
      .into_int_value();

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
    self.body.lower(generator, cache);

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

    None
  }
}

impl Lower for ast::IfStmt {
  fn lower<'a, 'ctx>(
    &self,
    generator: &mut LlvmGenerator<'a, 'ctx>,
    cache: &mut cache::Cache,
  ) -> Option<inkwell::values::BasicValueEnum<'ctx>> {
    // TODO: Add logic for the `else` branch.

    let llvm_condition = self.condition.lower(generator, cache).unwrap();
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
    self.then_block.lower(generator, cache);

    // Fallthrough if applicable.
    if llvm_then_block.get_terminator().is_none() {
      generator
        .llvm_builder
        .build_unconditional_branch(llvm_after_block);
    }

    generator.llvm_builder.position_at_end(llvm_after_block);

    None
  }
}

impl Lower for ast::Literal {
  fn lower<'a, 'ctx>(
    &self,
    generator: &mut LlvmGenerator<'a, 'ctx>,
    _cache: &mut cache::Cache,
  ) -> Option<inkwell::values::BasicValueEnum<'ctx>> {
    Some(match self {
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
    })
  }
}

impl Lower for ast::Function {
  fn lower<'a, 'ctx>(
    &self,
    generator: &mut LlvmGenerator<'a, 'ctx>,
    cache: &mut cache::Cache,
  ) -> Option<inkwell::values::BasicValueEnum<'ctx>> {
    let llvm_function_type = generator.lower_prototype(&self.prototype, cache);

    // TODO: Investigate whether this is enough. What about nested pointer types (`**`, etc.)?
    if let Some(return_type) = llvm_function_type.get_return_type() {
      generator.return_expects_rvalue = !return_type.is_pointer_type();
    }

    let is_main = self.name == MAIN_FUNCTION_NAME;

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
      return Some(
        llvm_existing_function
          .as_global_value()
          .as_basic_value_enum(),
      );
    }

    let llvm_function = generator.llvm_module.add_function(
      llvm_function_name.as_str(),
      llvm_function_type,
      Some(if is_main {
        inkwell::module::Linkage::External
      } else {
        inkwell::module::Linkage::Private
      }),
    );

    generator.llvm_function_buffer = Some(llvm_function);

    // TODO: Use a zipper, along with a chain. Actually, is this necessary?
    for (i, ref mut llvm_parameter) in llvm_function.get_param_iter().enumerate() {
      // TODO: Ensure safe access.
      let parameter = &self.prototype.parameters[i];

      parameter.lower(generator, cache);
      llvm_parameter.set_name(parameter.0.as_str());
    }

    let llvm_entry_block = generator
      .llvm_context
      .append_basic_block(llvm_function, "fn.entry");

    generator.llvm_builder.position_at_end(llvm_entry_block);
    self.body.lower(generator, cache);

    // Build return void instruction if the function doesn't return.
    if generator.get_current_block().get_terminator().is_none() {
      generator.llvm_builder.build_return(None);
    }

    Some(llvm_function.as_global_value().as_basic_value_enum())
  }
}

impl Lower for ast::Extern {
  fn lower<'a, 'ctx>(
    &self,
    generator: &mut LlvmGenerator<'a, 'ctx>,
    cache: &mut cache::Cache,
  ) -> Option<inkwell::values::BasicValueEnum<'ctx>> {
    let llvm_function_type = generator.lower_prototype(&self.prototype, cache);

    let llvm_external_function = generator.llvm_module.add_function(
      self.name.as_str(),
      llvm_function_type,
      Some(inkwell::module::Linkage::External),
    );

    Some(
      llvm_external_function
        .as_global_value()
        .as_basic_value_enum(),
    )
  }
}

impl Lower for ast::Block {
  fn lower<'a, 'ctx>(
    &self,
    generator: &mut LlvmGenerator<'a, 'ctx>,
    cache: &mut cache::Cache,
  ) -> Option<inkwell::values::BasicValueEnum<'ctx>> {
    for statement in &self.statements {
      statement.lower(generator, cache);

      // Do not continue lowering statements if the current block is terminated.
      if generator.get_current_block().get_terminator().is_some() {
        break;
      }
    }

    None
  }
}

impl Lower for ast::ReturnStmt {
  fn lower<'a, 'ctx>(
    &self,
    generator: &mut LlvmGenerator<'a, 'ctx>,
    cache: &mut cache::Cache,
  ) -> Option<inkwell::values::BasicValueEnum<'ctx>> {
    let llvm_return_value = if let Some(return_value) = &self.value {
      let llvm_value = return_value.lower(generator, cache).unwrap();

      // TODO: Should this only apply for return statements? What about variable declarations or assignments?
      // Perform an implicit dereference if applicable.
      let llvm_final_value =
        if generator.return_expects_rvalue && llvm_value.get_type().is_pointer_type() {
          generator
            .llvm_builder
            // TODO: Is the value always going to be a pointer value?
            .build_load(llvm_value.into_pointer_value(), "implicit.dereference")
        } else {
          llvm_value
        };

      Some(llvm_final_value)
    } else {
      None
    };

    generator.build_return(llvm_return_value);

    None
  }
}

impl Lower for ast::UnaryExpr {
  fn lower<'a, 'ctx>(
    &self,
    generator: &mut LlvmGenerator<'a, 'ctx>,
    cache: &mut cache::Cache,
  ) -> Option<inkwell::values::BasicValueEnum<'ctx>> {
    let llvm_value = self.expr.lower(generator, cache).unwrap();

    Some(match self.operator {
      ast::OperatorKind::Not => {
        // NOTE: We expect the value to be a boolean. This should be enforced during type-checking.
        generator
          .llvm_builder
          .build_not(llvm_value.into_int_value(), "not")
          .as_basic_value_enum()
      }
      ast::OperatorKind::SubtractOrNegate => {
        // NOTE: We expect the value to be an integer or float. This should be enforced during type-checking.
        if llvm_value.is_int_value() {
          generator
            .llvm_builder
            .build_int_neg(llvm_value.into_int_value(), "negate")
            .as_basic_value_enum()
        } else {
          generator
            .llvm_builder
            .build_float_neg(llvm_value.into_float_value(), "negate")
            .as_basic_value_enum()
        }
      }
      ast::OperatorKind::AddressOf => {
        // FIXME: Also, have to verify lifetimes. This isn't nearly started.
        // FIXME: Cannot bind references to temporary values. Must only be to existing definitions. This should be enforced during type-checking.

        // TODO: Hot-fix for avoiding temporary values.
        assert!(llvm_value.is_pointer_value());

        llvm_value
      }
      // FIXME: Type-checker must verify this action, since it is assumed that the value is a pointer.
      ast::OperatorKind::MultiplyOrDereference => generator
        .llvm_builder
        .build_load(llvm_value.into_pointer_value(), "dereference"),
      _ => unreachable!(),
    })
  }
}

impl Lower for ast::LetStmt {
  fn lower<'a, 'ctx>(
    &self,
    generator: &mut LlvmGenerator<'a, 'ctx>,
    cache: &mut cache::Cache,
  ) -> Option<inkwell::values::BasicValueEnum<'ctx>> {
    let llvm_type = generator.lower_type(&self.ty, cache);

    let llvm_alloca_ptr = generator
      .llvm_builder
      .build_alloca(llvm_type, self.name.as_str());

    generator.let_stmt_allocation = Some(llvm_alloca_ptr);

    let llvm_value = self.value.lower(generator, cache);

    generator.let_stmt_allocation = None;

    // NOTE: Some values (such as `StructValue`) work on the allocated type, and do not produce any value.
    if let Some(llvm_value) = llvm_value {
      generator
        .llvm_builder
        .build_store(llvm_alloca_ptr, llvm_value);
    }

    Some(llvm_alloca_ptr.as_basic_value_enum())
  }
}

impl Lower for ast::FunctionCall {
  fn lower<'a, 'ctx>(
    &self,
    generator: &mut LlvmGenerator<'a, 'ctx>,
    cache: &mut cache::Cache,
  ) -> Option<inkwell::values::BasicValueEnum<'ctx>> {
    let llvm_arguments = self
      .arguments
      .iter()
      .map(|argument| argument.lower(generator, cache).unwrap().into())
      .collect::<Vec<_>>();

    let llvm_target_function = generator
      .memoize_or_retrieve(self.target_key.unwrap(), cache)
      .into_pointer_value();

    let llvm_call_value = generator.llvm_builder.build_call(
      inkwell::values::CallableValue::try_from(llvm_target_function).unwrap(),
      llvm_arguments.as_slice(),
      format!("{}.call", self.callee_id.to_string()).as_str(),
    );

    let llvm_call_basic_value_result = llvm_call_value.try_as_basic_value();

    if llvm_call_basic_value_result.is_left() {
      Some(llvm_call_basic_value_result.left().unwrap())
    } else {
      None
    }
  }
}

impl Lower for ast::BreakStmt {
  fn lower<'a, 'ctx>(
    &self,
    generator: &mut LlvmGenerator<'a, 'ctx>,
    _cache: &mut cache::Cache,
  ) -> Option<inkwell::values::BasicValueEnum<'ctx>> {
    // TODO: What happens if there are nested loops? Will the buffer be overwritten?
    // NOTE: By this point, we assume that whether we're actually in a loop was handled by the type-checker.
    generator
      .llvm_builder
      .build_unconditional_branch(generator.next_block.unwrap());

    None
  }
}

impl Lower for ast::Definition {
  fn lower<'a, 'ctx>(
    &self,
    generator: &mut LlvmGenerator<'a, 'ctx>,
    cache: &mut cache::Cache,
  ) -> Option<inkwell::values::BasicValueEnum<'ctx>> {
    // TODO: This might error for other globally-defined types.
    if !matches!(&*self.node.as_ref().borrow(), ast::Node::StructType(_)) {
      Some(generator.memoize_or_retrieve(self.definition_key, cache))
    } else {
      None
    }
  }
}

impl Lower for ast::ExprStmt {
  fn lower<'a, 'ctx>(
    &self,
    generator: &mut LlvmGenerator<'a, 'ctx>,
    cache: &mut cache::Cache,
  ) -> Option<inkwell::values::BasicValueEnum<'ctx>> {
    self.expr.lower(generator, cache)
  }
}

pub struct LlvmGenerator<'a, 'ctx> {
  module_name: String,
  llvm_context: &'ctx inkwell::context::Context,
  llvm_module: &'a inkwell::module::Module<'ctx>,
  llvm_builder: inkwell::builder::Builder<'ctx>,
  llvm_function_buffer: Option<inkwell::values::FunctionValue<'ctx>>,
  // TODO: Shouldn't this be a vector instead?
  llvm_cached_values:
    std::collections::HashMap<cache::DefinitionKey, inkwell::values::BasicValueEnum<'ctx>>,
  llvm_cached_types:
    std::collections::HashMap<cache::DefinitionKey, inkwell::types::BasicTypeEnum<'ctx>>,
  next_block: Option<inkwell::basic_block::BasicBlock<'ctx>>,
  return_expects_rvalue: bool,
  let_stmt_allocation: Option<inkwell::values::PointerValue<'ctx>>,
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
      llvm_cached_values: std::collections::HashMap::new(),
      llvm_cached_types: std::collections::HashMap::new(),
      next_block: None,
      return_expects_rvalue: false,
      let_stmt_allocation: None,
    }
  }

  // TODO: Ensure that this function is tail-recursive.
  fn lower_type(
    &mut self,
    ty: &ast::Type,
    cache: &cache::Cache,
  ) -> inkwell::types::BasicTypeEnum<'ctx> {
    match ty {
      ast::Type::Primitive(primitive_type) => match primitive_type {
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
      ast::Type::Array(element_type, size) => self
        .lower_type(&element_type, cache)
        .array_type(size.clone())
        .as_basic_type_enum(),
      ast::Type::Pointer(pointee_type) => self
        .lower_type(&pointee_type, cache)
        .ptr_type(inkwell::AddressSpace::Generic)
        .as_basic_type_enum(),
      // FIXME: Is this redundant (because of `UserDefined`)?
      ast::Type::Struct(struct_type) => self
        .lower_struct_type(struct_type, cache)
        .as_basic_type_enum(),
      ast::Type::UserDefined(user_defined_type) => {
        let target_key = user_defined_type.target_key.unwrap();

        // If the type has been cached previously, simply retrieve it.
        if let Some(llvm_cached_type) = self.llvm_cached_types.get(&target_key) {
          return llvm_cached_type.clone();
        }

        // Otherwise, lower and cache the target type.
        let cached_type_node = cache
          .declarations
          .get(&target_key)
          .unwrap()
          .as_ref()
          .borrow();

        let llvm_type = match &*cached_type_node {
          ast::Node::StructType(struct_type) => self.lower_struct_type(struct_type, cache),
          _ => unreachable!(),
        }
        .as_basic_type_enum();

        self.llvm_cached_types.insert(target_key, llvm_type.clone());

        llvm_type
      }
    }
  }

  fn lower_prototype(
    &mut self,
    prototype: &ast::Prototype,
    cache: &cache::Cache,
  ) -> inkwell::types::FunctionType<'ctx> {
    let llvm_parameter_types = prototype
      .parameters
      .iter()
      .map(|parameter| self.lower_type(&parameter.1, cache).into())
      .collect::<Vec<_>>();

    // TODO: Simplify code (find common ground between `void` and `basic` types).
    if let Some(return_type) = &prototype.return_type {
      self
        .lower_type(&return_type, cache)
        // TODO: Is `is_variadic` being copied?
        .fn_type(llvm_parameter_types.as_slice(), prototype.is_variadic)
        .ptr_type(inkwell::AddressSpace::Generic)
        .into()
    } else {
      self
        .llvm_context
        .void_type()
        // TODO: Is `is_variadic` being copied?
        .fn_type(llvm_parameter_types.as_slice(), prototype.is_variadic)
        .ptr_type(inkwell::AddressSpace::Generic)
        .as_basic_type_enum()
    }
    .into_pointer_type()
    .get_element_type()
    .into_function_type()
  }

  // TODO: Being repeated in `lower_type`.
  fn lower_struct_type(
    &mut self,
    struct_type: &ast::StructType,
    cache: &cache::Cache,
  ) -> inkwell::types::StructType<'ctx> {
    let llvm_field_types = struct_type
      .fields
      .iter()
      .map(|field| self.lower_type(&field.1, cache))
      .collect::<Vec<_>>();

    // TODO: Consider caching the struct type here?

    self
      .llvm_context
      .struct_type(llvm_field_types.as_slice(), false)
      .as_basic_type_enum()
      .into_struct_type()
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
    definition_key: cache::DefinitionKey,
    cache: &mut cache::Cache,
  ) -> inkwell::values::BasicValueEnum<'ctx> {
    if !self.llvm_cached_values.contains_key(&definition_key) {
      // TODO: Will there be a case where we'd need the buffers to change? If so, take in a flag parameter.
      let previous_function_buffer = self.llvm_function_buffer;
      let previous_block = self.llvm_builder.get_insert_block();

      // If the definition is not already memoized, memoize it.
      // This retrieval will panic in case of a logic error (internal error).
      let result = std::rc::Rc::clone(cache.declarations.get_mut(&definition_key).unwrap())
        .borrow_mut()
        .lower(self, cache)
        .unwrap();

      // TODO: Should we be inserting the definition key here?
      self.llvm_cached_values.insert(definition_key, result);

      // Restore buffers after processing.
      if let Some(previous_block) = previous_block {
        self.llvm_builder.position_at_end(previous_block);
      }

      self.llvm_function_buffer = previous_function_buffer;

      return result;
    }

    // NOTE: The LLVM value is not copied, but rather the reference to it.
    self
      .llvm_cached_values
      .get(&definition_key)
      .unwrap()
      .clone()
  }
}

// TODO: Receive scope path as `std::path::PathBuf` instead?
fn mangle_name(scope_name: &String, name: &String) -> String {
  format!(".{}.{}", scope_name, name)
}
