use crate::{ast, cache, dispatch};
use inkwell::{types::BasicType, values::BasicValue};
use std::convert::TryFrom;

pub const MAIN_FUNCTION_NAME: &str = "main";

pub trait Lower {
  fn lower<'a, 'ctx>(
    &self,
    _generator: &mut LlvmGenerator<'a, 'ctx>,
    _cache: &cache::Cache,
  ) -> Option<inkwell::values::BasicValueEnum<'ctx>> {
    None
  }
}

impl Lower for ast::Node {
  fn lower<'a, 'ctx>(
    &self,
    generator: &mut LlvmGenerator<'a, 'ctx>,
    cache: &cache::Cache,
  ) -> Option<inkwell::values::BasicValueEnum<'ctx>> {
    dispatch!(self, Lower::lower, generator, cache)
  }
}

impl Lower for ast::Pattern {
  //
}

impl Lower for ast::IntrinsicCall {
  fn lower<'a, 'ctx>(
    &self,
    generator: &mut LlvmGenerator<'a, 'ctx>,
    cache: &cache::Cache,
  ) -> Option<inkwell::values::BasicValueEnum<'ctx>> {
    match self.kind {
      ast::IntrinsicKind::Panic => {
        let llvm_panic_function = generator.get_or_insert_panic_function();

        // FIXME: Forcing to expect a first argument.
        // FIXME: How would we prefix it with `panic:`? Maybe will have to use `printf` instead, as the print function.
        let llvm_message_value = self
          .arguments
          .first()
          .unwrap()
          .lower(generator, cache)
          .unwrap();

        // TODO: How about we merge this into a `panic` function?
        let print_function = generator.get_or_insert_print_function();

        generator
          .llvm_builder
          .build_call(print_function, &[llvm_message_value.into()], "");

        generator
          .llvm_builder
          .build_call(llvm_panic_function, &[], "intrinsic.call");

        None
      }
    }
  }
}

impl Lower for ast::ExternStatic {
  fn lower<'a, 'ctx>(
    &self,
    generator: &mut LlvmGenerator<'a, 'ctx>,
    cache: &cache::Cache,
  ) -> Option<inkwell::values::BasicValueEnum<'ctx>> {
    let llvm_global_value =
      generator
        .llvm_module
        .add_global(generator.lower_type(&self.1, cache), None, self.0.as_str());

    Some(llvm_global_value.as_basic_value_enum())
  }
}

impl Lower for ast::StructValue {
  fn lower<'a, 'ctx>(
    &self,
    generator: &mut LlvmGenerator<'a, 'ctx>,
    cache: &cache::Cache,
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
    _cache: &cache::Cache,
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
    cache: &cache::Cache,
  ) -> Option<inkwell::values::BasicValueEnum<'ctx>> {
    let llvm_value = self.value.lower(generator, cache).unwrap();

    let llvm_target = generator
      .lower_without_access(&self.assignee_expr, cache)
      .unwrap();

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
    _cache: &cache::Cache,
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
    cache: &cache::Cache,
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

      generator.build_panic_assertion(
        inkwell::IntPredicate::ULT,
        (
          llvm_index,
          // FIXME: Is this the correct way to get the size of the array? Probably not.
          generator.llvm_context.i32_type().const_int(3, false),
        ),
        // TODO: Temporary, the `panic` prefix should be added automatically.
        "panic: array index out of bounds".to_string(),
      );

      // TODO: Should we actually be de-referencing the pointer here?
      Some(generator.access(llvm_gep_ptr))
    }
  }
}

impl Lower for ast::ArrayValue {
  fn lower<'a, 'ctx>(
    &self,
    generator: &mut LlvmGenerator<'a, 'ctx>,
    cache: &cache::Cache,
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

    let llvm_array_ptr = generator
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
          llvm_array_ptr,
          &[first_index, llvm_index],
          "array.init",
        );

        generator
          .llvm_builder
          .build_store(llvm_gep, llvm_value.clone());
      }
    }

    // TODO: Might not need to load the array in order to initialize it, but to return it?
    let llvm_array_value = generator
      .access(llvm_array_ptr)
      .into_array_value()
      .as_basic_value_enum();

    Some(llvm_array_value)
  }
}

impl Lower for ast::Parameter {
  fn lower<'a, 'ctx>(
    &self,
    generator: &mut LlvmGenerator<'a, 'ctx>,
    _cache: &cache::Cache,
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
    cache: &cache::Cache,
  ) -> Option<inkwell::values::BasicValueEnum<'ctx>> {
    self.0.lower(generator, cache);

    None
  }
}

impl Lower for ast::BinaryExpr {
  fn lower<'a, 'ctx>(
    &self,
    generator: &mut LlvmGenerator<'a, 'ctx>,
    cache: &cache::Cache,
  ) -> Option<inkwell::values::BasicValueEnum<'ctx>> {
    let llvm_left_value = self.left.lower(generator, cache).unwrap();
    let llvm_right_value = self.right.lower(generator, cache).unwrap();

    // NOTE: By this point, we assume that both values are of the same type.
    let is_int_values = llvm_left_value.is_int_value();

    // TODO: Make use of.
    // let is_signed = llvm_left_value.into_int_value().get_sign_extended_constant();

    let llvm_operation = match self.operator {
      ast::OperatorKind::Add if is_int_values => generator
        .llvm_builder
        .build_int_add(
          llvm_left_value.into_int_value(),
          llvm_right_value.into_int_value(),
          "int.add_op",
        )
        .as_basic_value_enum(),
      ast::OperatorKind::Add => generator
        .llvm_builder
        .build_float_add(
          llvm_left_value.into_float_value(),
          llvm_right_value.into_float_value(),
          "float.add_op",
        )
        .as_basic_value_enum(),
      ast::OperatorKind::SubtractOrNegate if is_int_values => generator
        .llvm_builder
        .build_int_sub(
          llvm_left_value.into_int_value(),
          llvm_right_value.into_int_value(),
          "int.subtract_op",
        )
        .as_basic_value_enum(),
      ast::OperatorKind::SubtractOrNegate => generator
        .llvm_builder
        .build_float_sub(
          llvm_left_value.into_float_value(),
          llvm_right_value.into_float_value(),
          "float.subtract_op",
        )
        .as_basic_value_enum(),
      ast::OperatorKind::MultiplyOrDereference if is_int_values => generator
        .llvm_builder
        .build_int_mul(
          llvm_left_value.into_int_value(),
          llvm_right_value.into_int_value(),
          "int.multiply_op",
        )
        .as_basic_value_enum(),
      ast::OperatorKind::MultiplyOrDereference => generator
        .llvm_builder
        .build_float_mul(
          llvm_left_value.into_float_value(),
          llvm_right_value.into_float_value(),
          "float.multiply_op",
        )
        .as_basic_value_enum(),
      // TODO: What if there's division by zero?
      // TODO: Support for unsgined division?
      ast::OperatorKind::Divide if is_int_values => generator
        .llvm_builder
        .build_int_signed_div(
          llvm_left_value.into_int_value(),
          llvm_right_value.into_int_value(),
          "int.divide_op",
        )
        .as_basic_value_enum(),
      ast::OperatorKind::Divide => generator
        .llvm_builder
        .build_float_div(
          llvm_left_value.into_float_value(),
          llvm_right_value.into_float_value(),
          "float.divide_op",
        )
        .as_basic_value_enum(),
      ast::OperatorKind::LessThan if is_int_values => generator
        .llvm_builder
        .build_int_compare(
          // TODO: Support for unsigned?
          inkwell::IntPredicate::SLT,
          llvm_left_value.into_int_value(),
          llvm_right_value.into_int_value(),
          "int.slt_op",
        )
        .as_basic_value_enum(),
      ast::OperatorKind::LessThan => generator
        .llvm_builder
        .build_float_compare(
          inkwell::FloatPredicate::OLT,
          llvm_left_value.into_float_value(),
          llvm_right_value.into_float_value(),
          "float.slt_op",
        )
        .as_basic_value_enum(),
      ast::OperatorKind::GreaterThan if is_int_values => generator
        .llvm_builder
        .build_int_compare(
          // TODO: Support for unsigned?
          inkwell::IntPredicate::SGT,
          llvm_left_value.into_int_value(),
          llvm_right_value.into_int_value(),
          "int.sgt_op",
        )
        .as_basic_value_enum(),
      ast::OperatorKind::GreaterThan => generator
        .llvm_builder
        .build_float_compare(
          inkwell::FloatPredicate::OGT,
          llvm_left_value.into_float_value(),
          llvm_right_value.into_float_value(),
          "float.gt_op",
        )
        .as_basic_value_enum(),
      ast::OperatorKind::LessThanOrEqual if is_int_values => generator
        .llvm_builder
        .build_int_compare(
          inkwell::IntPredicate::SLE,
          llvm_left_value.into_int_value(),
          llvm_right_value.into_int_value(),
          "int.sltoe_op",
        )
        .as_basic_value_enum(),
      ast::OperatorKind::LessThanOrEqual => generator
        .llvm_builder
        .build_float_compare(
          inkwell::FloatPredicate::OLE,
          llvm_left_value.into_float_value(),
          llvm_right_value.into_float_value(),
          "float.ltoe_op",
        )
        .as_basic_value_enum(),
      ast::OperatorKind::GreaterThanOrEqual if is_int_values => generator
        .llvm_builder
        .build_int_compare(
          inkwell::IntPredicate::SGE,
          llvm_left_value.into_int_value(),
          llvm_right_value.into_int_value(),
          "int.sgtoe_op",
        )
        .as_basic_value_enum(),
      ast::OperatorKind::GreaterThanOrEqual => generator
        .llvm_builder
        .build_float_compare(
          inkwell::FloatPredicate::OGE,
          llvm_left_value.into_float_value(),
          llvm_right_value.into_float_value(),
          "float.gtoe_op",
        )
        .as_basic_value_enum(),
      ast::OperatorKind::Equality if is_int_values => generator
        .llvm_builder
        .build_int_compare(
          inkwell::IntPredicate::EQ,
          llvm_left_value.into_int_value(),
          llvm_right_value.into_int_value(),
          "int.eq_op",
        )
        .as_basic_value_enum(),
      ast::OperatorKind::Equality => generator
        .llvm_builder
        .build_float_compare(
          inkwell::FloatPredicate::OEQ,
          llvm_left_value.into_float_value(),
          llvm_right_value.into_float_value(),
          "float.eq_op",
        )
        .as_basic_value_enum(),
      ast::OperatorKind::And => generator
        .llvm_builder
        .build_and(
          llvm_left_value.into_int_value(),
          llvm_right_value.into_int_value(),
          "and_op",
        )
        .as_basic_value_enum(),
      ast::OperatorKind::Or => generator
        .llvm_builder
        .build_or(
          llvm_left_value.into_int_value(),
          llvm_right_value.into_int_value(),
          "or_op",
        )
        .as_basic_value_enum(),
      // TODO: Support for when comparing equality of pointers/references.
      // TODO: Support for all operators.
      _ => todo!(),
    };

    // TODO: Simplify this to obtain the operator/predicate, then lower separately? Maybe not possible.
    Some(llvm_operation)
  }
}

impl Lower for ast::VariableOrMemberRef {
  fn lower<'a, 'ctx>(
    &self,
    generator: &mut LlvmGenerator<'a, 'ctx>,
    cache: &cache::Cache,
  ) -> Option<inkwell::values::BasicValueEnum<'ctx>> {
    // FIXME: Verify that the logic for automatic access is correct in this function.

    // FIXME: This may not be working, because the `memoize_or_retrieve` function directly lowers, regardless of expected access or not.
    let llvm_value = generator.memoize_or_retrieve(self.0.target_key.unwrap(), cache);

    Some(if generator.expecting_access {
      generator.attempt_access(llvm_value)
    } else {
      llvm_value
    })
  }
}

impl Lower for ast::LoopStmt {
  fn lower<'a, 'ctx>(
    &self,
    generator: &mut LlvmGenerator<'a, 'ctx>,
    cache: &cache::Cache,
  ) -> Option<inkwell::values::BasicValueEnum<'ctx>> {
    // FIXME: The condition needs to be re-lowered per iteration.
    // NOTE: At this point, the condition should be verified to be a boolean by the type-checker.
    let llvm_condition = if let Some(condition) = &self.condition {
      condition.lower(generator, cache).unwrap().into_int_value()
    } else {
      generator.llvm_context.bool_type().const_int(1, false)
    };

    let llvm_current_function = generator.llvm_function_buffer.unwrap();

    let llvm_then_block = generator
      .llvm_context
      .append_basic_block(llvm_current_function, "loop.then");

    let llvm_after_block = generator
      .llvm_context
      .append_basic_block(llvm_current_function, "loop.after");

    // Build the initial conditional jump to start the loop.
    generator.llvm_builder.build_conditional_branch(
      llvm_condition,
      llvm_then_block,
      llvm_after_block,
    );

    generator.llvm_builder.position_at_end(llvm_then_block);
    generator.next_block = Some(llvm_after_block);
    self.body.lower(generator, cache);

    // Fallthrough or loop if applicable.
    if generator.get_current_block().get_terminator().is_none() {
      let llvm_condition_iter = if let Some(condition) = &self.condition {
        condition.lower(generator, cache).unwrap().into_int_value()
      } else {
        generator.llvm_context.bool_type().const_int(1, false)
      };

      // FIXME: Ensure this logic is correct (investigate).
      generator.llvm_builder.build_conditional_branch(
        llvm_condition_iter,
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
    cache: &cache::Cache,
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
    _cache: &cache::Cache,
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
      ast::Literal::Nullptr => generator
        .llvm_context
        // FIXME: The type should be correct. Otherwise, we'll get a type mismatch error when compiling the LLVM IR.
        .i8_type()
        .ptr_type(inkwell::AddressSpace::Generic)
        .const_null()
        .as_basic_value_enum(),
    })
  }
}

impl Lower for ast::Function {
  fn lower<'a, 'ctx>(
    &self,
    generator: &mut LlvmGenerator<'a, 'ctx>,
    cache: &cache::Cache,
  ) -> Option<inkwell::values::BasicValueEnum<'ctx>> {
    let llvm_function_type = generator.lower_prototype(&self.prototype, cache);

    // TODO: Investigate whether this is enough. What about nested pointer types (`**`, etc.)?
    if let Some(return_type) = llvm_function_type.get_return_type() {
      generator.return_expects_access = !return_type.is_pointer_type();
    }

    let is_main = self.name == MAIN_FUNCTION_NAME;

    let llvm_function_name = if is_main {
      // TODO: Name being cloned. Is this okay?
      self.name.to_owned()
    } else {
      generator.mangle_name(&self.name)
    };

    assert!(generator
      .llvm_module
      .get_function(llvm_function_name.as_str())
      .is_none());

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

    // Manually cache the function now to allow for recursive function calls.
    generator.llvm_cached_values.insert(
      generator.pending_function_definition_key.unwrap(),
      llvm_function.as_global_value().as_basic_value_enum(),
    );

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

impl Lower for ast::ExternFunction {
  fn lower<'a, 'ctx>(
    &self,
    generator: &mut LlvmGenerator<'a, 'ctx>,
    cache: &cache::Cache,
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
    cache: &cache::Cache,
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
    cache: &cache::Cache,
  ) -> Option<inkwell::values::BasicValueEnum<'ctx>> {
    let llvm_return_value = if let Some(return_value) = &self.value {
      Some(return_value.lower(generator, cache).unwrap())
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
    cache: &cache::Cache,
  ) -> Option<inkwell::values::BasicValueEnum<'ctx>> {
    Some(match self.operator {
      ast::OperatorKind::Not => {
        let llvm_value = self.expr.lower(generator, cache).unwrap();

        // NOTE: We expect the value to be a boolean. This should be enforced during type-checking.
        generator
          .llvm_builder
          .build_not(llvm_value.into_int_value(), "not_op")
          .as_basic_value_enum()
      }
      ast::OperatorKind::SubtractOrNegate => {
        let llvm_value = self.expr.lower(generator, cache).unwrap();

        // NOTE: We expect the value to be an integer or float. This should be enforced during type-checking.
        if llvm_value.is_int_value() {
          generator
            .llvm_builder
            .build_int_neg(llvm_value.into_int_value(), "int.negate_op")
            .as_basic_value_enum()
        } else {
          generator
            .llvm_builder
            .build_float_neg(llvm_value.into_float_value(), "float.negate_op")
            .as_basic_value_enum()
        }
      }
      ast::OperatorKind::AddressOf => {
        // FIXME: Also, have to verify lifetimes. This isn't nearly started.
        // FIXME: Cannot bind references to temporary values. Must only be to existing definitions. This should be enforced during type-checking.

        generator.lower_without_access(&self.expr, cache).unwrap()
      }
      ast::OperatorKind::MultiplyOrDereference => {
        let llvm_value = self.expr.lower(generator, cache).unwrap();

        generator.access(llvm_value.into_pointer_value())
      }
      _ => unreachable!(),
    })
  }
}

impl Lower for ast::LetStmt {
  fn lower<'a, 'ctx>(
    &self,
    generator: &mut LlvmGenerator<'a, 'ctx>,
    cache: &cache::Cache,
  ) -> Option<inkwell::values::BasicValueEnum<'ctx>> {
    let llvm_type = generator.lower_type(&self.ty, cache);

    let llvm_alloca_ptr = generator
      .llvm_builder
      .build_alloca(llvm_type, format!("var.{}", self.name).as_str());

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
    cache: &cache::Cache,
  ) -> Option<inkwell::values::BasicValueEnum<'ctx>> {
    let llvm_arguments = self
      .arguments
      .iter()
      .map(|argument| argument.lower(generator, cache).unwrap().into())
      .collect::<Vec<_>>();

    let llvm_target_function = generator
      .memoize_or_retrieve(self.callee_pattern.target_key.unwrap(), cache)
      .into_pointer_value();

    let llvm_call_value = generator.llvm_builder.build_call(
      inkwell::values::CallableValue::try_from(llvm_target_function).unwrap(),
      llvm_arguments.as_slice(),
      format!("call.{}", self.callee_pattern.to_string()).as_str(),
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
    _cache: &cache::Cache,
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
    cache: &cache::Cache,
  ) -> Option<inkwell::values::BasicValueEnum<'ctx>> {
    let node = self.node_ref_cell.borrow();

    // Set the pending function definition key to cache the function early.
    // This eliminates problems with multi-borrows that may occur when lowering
    // recursive functions.
    if matches!(&*node, ast::Node::Function(_)) {
      generator.pending_function_definition_key = Some(self.definition_key);
    }

    // TODO: This might error for other globally-defined types.
    if !matches!(&*node, ast::Node::StructType(_)) {
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
    cache: &cache::Cache,
  ) -> Option<inkwell::values::BasicValueEnum<'ctx>> {
    self.expr.lower(generator, cache)
  }
}

pub struct LlvmGenerator<'a, 'ctx> {
  pub module_name: String,
  llvm_context: &'ctx inkwell::context::Context,
  llvm_module: &'a inkwell::module::Module<'ctx>,
  llvm_builder: inkwell::builder::Builder<'ctx>,
  llvm_function_buffer: Option<inkwell::values::FunctionValue<'ctx>>,
  // TODO: Shouldn't this be a vector instead?
  llvm_cached_values:
    std::collections::HashMap<cache::DefinitionKey, inkwell::values::BasicValueEnum<'ctx>>,
  llvm_cached_types:
    std::collections::HashMap<cache::DefinitionKey, inkwell::types::BasicTypeEnum<'ctx>>,
  /// The next fall-through block (if any).
  next_block: Option<inkwell::basic_block::BasicBlock<'ctx>>,
  return_expects_access: bool,
  let_stmt_allocation: Option<inkwell::values::PointerValue<'ctx>>,
  // TODO: Consider merging implicit dereference flags.
  /// Whether the construct is expecting an implicit `load` instruction.
  ///
  /// Will always be `true` by default, unless a construct requires otherwise.
  expecting_access: bool,
  /// The definition key of the function currently being emitted (if any).
  /// Used to be able to handle recursive calls.
  pending_function_definition_key: Option<cache::DefinitionKey>,
  panic_function_cache: Option<inkwell::values::FunctionValue<'ctx>>,
  print_function_cache: Option<inkwell::values::FunctionValue<'ctx>>,
  mangle_counter: usize,
}

impl<'a, 'ctx> LlvmGenerator<'a, 'ctx> {
  pub fn new(
    llvm_context: &'ctx inkwell::context::Context,
    llvm_module: &'a inkwell::module::Module<'ctx>,
  ) -> Self {
    Self {
      module_name: "unnamed".to_string(),
      llvm_context,
      llvm_module,
      llvm_builder: llvm_context.create_builder(),
      llvm_function_buffer: None,
      llvm_cached_values: std::collections::HashMap::new(),
      llvm_cached_types: std::collections::HashMap::new(),
      next_block: None,
      return_expects_access: false,
      let_stmt_allocation: None,
      expecting_access: true,
      pending_function_definition_key: None,
      panic_function_cache: None,
      print_function_cache: None,
      mangle_counter: 0,
    }
  }

  /// Mangle a name with an unique counter to avoid name collisions.
  fn mangle_name(&mut self, name: &String) -> String {
    // TODO: Consider using the `definition_key` instead?
    // NOTE: The current module name isn't used because it's not guaranteed to be the
    // active module when the definition is memoized (for example, inter-module function
    // calls).
    let mangled_name = format!(".fn{}.{}", self.mangle_counter, name);

    self.mangle_counter += 1;

    mangled_name
  }

  fn get_or_insert_panic_function(&mut self) -> inkwell::values::FunctionValue<'ctx> {
    if let Some(cached_panic_function) = self.panic_function_cache {
      return cached_panic_function;
    }

    // TODO: What if the `abort` function was already defined by the user?
    let libc_abort_function = self.llvm_module.add_function(
      "abort",
      self.llvm_context.void_type().fn_type(&[], false),
      Some(inkwell::module::Linkage::External),
    );

    let llvm_function_type = self
      .llvm_context
      .void_type()
      // TODO: Accept a message to be displayed first (string).
      .fn_type(&[], false);

    let llvm_panic_function =
      self
        .llvm_module
        .add_function("intrinsic.panic", llvm_function_type, None);

    let llvm_entry_block = self
      .llvm_context
      .append_basic_block(llvm_panic_function, "entry");

    let llvm_builder = self.llvm_context.create_builder();

    llvm_builder.position_at_end(llvm_entry_block);
    llvm_builder.build_call(libc_abort_function, &[], "");
    llvm_builder.build_unreachable();
    self.panic_function_cache = Some(llvm_panic_function);

    llvm_panic_function
  }

  fn get_or_insert_print_function(&mut self) -> inkwell::values::FunctionValue<'ctx> {
    if let Some(cached_print_function) = self.print_function_cache {
      return cached_print_function;
    }

    let libc_puts_arguments = &[self
      .llvm_context
      .i8_type()
      .ptr_type(inkwell::AddressSpace::Generic)
      .into()];

    // TODO: What if the `abort` function was already defined by the user?
    let libc_puts_function = self.llvm_module.add_function(
      "puts",
      self
        .llvm_context
        .i32_type()
        .fn_type(libc_puts_arguments, false),
      Some(inkwell::module::Linkage::External),
    );

    libc_puts_function
  }

  fn build_panic_assertion(
    &mut self,
    predicate: inkwell::IntPredicate,
    llvm_values: (
      inkwell::values::IntValue<'ctx>,
      inkwell::values::IntValue<'ctx>,
    ),
    message: String,
  ) {
    let llvm_next_block = self
      .llvm_context
      .append_basic_block(self.llvm_function_buffer.unwrap(), "panic.next");

    let llvm_panic_block = self
      .llvm_context
      .append_basic_block(self.llvm_function_buffer.unwrap(), "panic");

    let llvm_builder = self.llvm_context.create_builder();

    llvm_builder.position_at_end(llvm_panic_block);

    let llvm_panic_message = self
      .llvm_builder
      .build_global_string_ptr(message.as_ref(), "panic.msg")
      .as_basic_value_enum();

    llvm_builder.build_call(
      self.get_or_insert_print_function(),
      &[llvm_panic_message.into()],
      "",
    );

    llvm_builder.build_call(self.get_or_insert_panic_function(), &[], "call.panic");
    llvm_builder.build_unreachable();

    let llvm_assertion_comparison = self.llvm_builder.build_int_compare(
      predicate,
      llvm_values.0,
      llvm_values.1,
      "panic.assertion",
    );

    self.llvm_builder.build_conditional_branch(
      llvm_assertion_comparison,
      llvm_next_block,
      llvm_panic_block,
    );

    self.llvm_builder.position_at_end(llvm_next_block);
  }

  fn lower_without_access(
    &mut self,
    node: &ast::Node,
    cache: &cache::Cache,
  ) -> Option<inkwell::values::BasicValueEnum<'ctx>> {
    let expecting_access_buffer = self.expecting_access;

    self.expecting_access = false;

    let llvm_value = node.lower(self, cache);

    self.expecting_access = expecting_access_buffer;

    llvm_value
  }

  /// Insert a `load` instruction for the given LLVM value.
  ///
  /// Equivalent to a de-reference of a pointer.
  fn access(
    &mut self,
    llvm_value: inkwell::values::PointerValue<'ctx>,
  ) -> inkwell::values::BasicValueEnum<'ctx> {
    self
      .llvm_builder
      .build_load(llvm_value, "access")
      .as_basic_value_enum()
  }

  fn attempt_access(
    &mut self,
    llvm_value: inkwell::values::BasicValueEnum<'ctx>,
  ) -> inkwell::values::BasicValueEnum<'ctx> {
    if llvm_value.is_pointer_value() {
      self.access(llvm_value.into_pointer_value())
    } else {
      llvm_value
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
        // NOTE: The null primitive type is never lowered, only the nullptr value.
        ast::PrimitiveType::Null => unreachable!(),
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
        let cached_type_node = cache.get(&target_key);

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

    let llvm_struct_type = self
      .llvm_context
      .opaque_struct_type(format!("struct.{}", struct_type.name).as_str());

    llvm_struct_type.set_body(llvm_field_types.as_slice(), false);

    llvm_struct_type
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

  /// Attempt to retrieve an existing definition, otherwise proceed to
  /// lowering it and memoizing it under the current module.
  fn memoize_or_retrieve(
    &mut self,
    definition_key: cache::DefinitionKey,
    cache: &cache::Cache,
  ) -> inkwell::values::BasicValueEnum<'ctx> {
    // If the definition has been cached previously, simply retrieve it.
    if self.llvm_cached_values.contains_key(&definition_key) {
      // NOTE: The underlying LLVM value is not copied, but rather the reference to it.
      return self
        .llvm_cached_values
        .get(&definition_key)
        .unwrap()
        .clone();
    }

    // TODO: Are we missing any buffers to be saved?
    // TODO: Will there be a case where we'd need the buffers to change? If so, take in a flag parameter.
    let previous_function_buffer = self.llvm_function_buffer;
    let previous_insert_block = self.llvm_builder.get_insert_block();

    let llvm_value = cache
      .declarations
      .get(&definition_key)
      .unwrap()
      .borrow()
      .lower(self, cache)
      .unwrap();

    self.llvm_cached_values.insert(definition_key, llvm_value);

    // TODO: What if the `previous_block` buffer was `None`?
    // Restore buffers after processing.
    if let Some(previous_block) = previous_insert_block {
      self.llvm_builder.position_at_end(previous_block);
    }

    self.llvm_function_buffer = previous_function_buffer;

    return llvm_value;
  }
}
