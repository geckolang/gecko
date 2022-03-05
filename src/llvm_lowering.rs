use crate::{
  ast, cache, dispatch,
  type_check::{TypeCheck, TypeCheckContext},
};

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
    dispatch!(&self.kind, Lower::lower, generator, cache)
  }
}

impl Lower for ast::Trait {
  //
}

impl Lower for ast::StructImpl {
  fn lower<'a, 'ctx>(
    &self,
    generator: &mut LlvmGenerator<'a, 'ctx>,
    cache: &cache::Cache,
  ) -> Option<inkwell::values::BasicValueEnum<'ctx>> {
    for method in &self.methods {
      method.lower(generator, cache).unwrap();
    }

    None
  }
}

impl Lower for ast::MemberAccess {
  fn lower<'a, 'ctx>(
    &self,
    generator: &mut LlvmGenerator<'a, 'ctx>,
    cache: &cache::Cache,
  ) -> Option<inkwell::values::BasicValueEnum<'ctx>> {
    let llvm_struct = self
      .base_expr
      .lower(generator, cache)
      .unwrap()
      .into_pointer_value();

    // TODO: Should we reposition this to the name resolution step (post-resolve), and include a `Option<u32>` index field in `MemberAccess`?
    let struct_type = match TypeCheckContext::infer_and_resolve_type(&self.base_expr, cache) {
      ast::Type::Struct(struct_type) => struct_type,
      // FIXME: What about `This` type? Parameter `this` is of `This` type.
      _ => unreachable!(),
    };

    // First, check if its a field.
    if let Some(field_index) = struct_type
      .fields
      .iter()
      .position(|x| x.0 == self.member_name)
    {
      return Some(
        generator
          .llvm_builder
          .build_struct_gep(llvm_struct, field_index as u32, "struct.member.gep")
          .unwrap()
          .as_basic_value_enum(),
      );
    }

    // Otherwise, it must be a method.
    let impl_method_info = cache
      .get_struct_impls(&struct_type.unique_id)
      .unwrap()
      .iter()
      .find(|x| x.1 == self.member_name)
      .unwrap();

    generator.memoize_or_retrieve_value(impl_method_info.0, cache, false)
  }
}

impl Lower for ast::Closure {
  fn lower<'a, 'ctx>(
    &self,
    generator: &mut LlvmGenerator<'a, 'ctx>,
    cache: &cache::Cache,
  ) -> Option<inkwell::values::BasicValueEnum<'ctx>> {
    let buffers = generator.copy_buffers();
    // let mut modified_prototype = self.prototype.clone();

    for (_index, _capture) in self.captures.iter().enumerate() {
      // let capture_node = cache.force_get(&capture.1.unwrap());
      // let capture_node_type = (&*capture_node).infer_type(cache);
      // let computed_parameter_index = self.prototype.parameters.len() as usize + index;

      // TODO: Is the parameter position correct?
      // FIXME: Re-implement, after parameters were made definitions.
      // modified_prototype.parameters.push((
      //   format!("capture.{}", capture.0),
      //   capture_node_type,
      //   computed_parameter_index as u32,
      // ))
    }

    // FIXME: Use modified prototype.
    let llvm_function_type = generator.lower_prototype(&self.prototype, cache);
    let llvm_function_name = generator.mangle_name(&String::from("closure"));

    assert!(generator
      .llvm_module
      .get_function(llvm_function_name.as_str())
      .is_none());

    let llvm_function = generator.llvm_module.add_function(
      llvm_function_name.as_str(),
      llvm_function_type,
      Some(inkwell::module::Linkage::Private),
    );

    // TODO: Use a zipper, along with a chain.
    // for (i, llvm_parameter) in llvm_function.get_param_iter().enumerate() {
    //   // TODO: Ensure safe access.
    //   // FIXME: The llvm function's parameter count is longer than that of the prototypes. This is because of inserted captures. Fix this bug.
    //   let parameter = &self.prototype.parameters[i];

    //   parameter.lower(generator, cache);
    //   llvm_parameter.set_name(format!("param.{}", parameter.0).as_str());
    // }

    generator.llvm_function_buffer = Some(llvm_function);

    let llvm_entry_block = generator
      .llvm_context
      .append_basic_block(llvm_function, "closure.entry");

    generator.llvm_builder.position_at_end(llvm_entry_block);

    let yielded_result = self.body.lower(generator, cache);

    generator.attempt_build_return(yielded_result);

    let result = llvm_function.as_global_value().as_basic_value_enum();

    generator.restore_buffers(buffers);

    Some(result)
  }
}

impl Lower for ast::TypeAlias {
  //
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
    let llvm_struct_type = generator.memoize_or_retrieve_type(self.target_id.unwrap(), cache);

    let llvm_struct_alloca = generator.llvm_builder.build_alloca(
      llvm_struct_type,
      format!("struct.{}.alloca", self.struct_name).as_str(),
    );

    // Populate struct fields.
    for (index, field) in self.fields.iter().enumerate() {
      let struct_field_gep = generator
        .llvm_builder
        // TODO: Is this conversion safe?
        // TODO: Better name.
        .build_struct_gep(llvm_struct_alloca, index as u32, "struct.alloca.field.gep")
        .unwrap();

      let llvm_field_value = generator.lower_with_access_rules(field, cache).unwrap();

      generator
        .llvm_builder
        // FIXME: For nested structs, they will return `None`.
        .build_store(struct_field_gep, llvm_field_value);
    }

    // FIXME: [!] Revise: Inconsistent. Deals with flag, also what about on the case of an assignment?
    // If the array value is being directly assigned to a variable,
    // return the alloca pointer. Otherwise, access the alloca.
    // Some(if generator.let_stmt_flag.is_some() {
    //   llvm_struct_alloca.as_basic_value_enum()
    // } else {
    //   generator.access(llvm_struct_alloca)
    // })

    Some(llvm_struct_alloca.as_basic_value_enum())
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
      let llvm_name = generator.mangle_name(&format!("enum.{}.{}", self.name, name));

      let llvm_variant_global = generator.llvm_module.add_global(
        generator.llvm_context.i32_type(),
        Some(inkwell::AddressSpace::Const),
        llvm_name.as_str(),
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
    let llvm_value = generator
      .lower_with_access_rules(self.value.as_ref(), cache)
      .unwrap();

    // NOTE: In the case that our target is a let-statement (through
    // a reference), memoization or retrieval will occur on the lowering
    // step of the reference.
    let llvm_assignee = self.assignee_expr.lower(generator, cache).unwrap();

    generator
      .llvm_builder
      .build_store(llvm_assignee.into_pointer_value(), llvm_value);

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
    let llvm_index = self
      .index_expr
      .lower(generator, cache)
      .unwrap()
      .into_int_value();

    // TODO: Here we opted not to forward buffers. Ensure this is correct.
    let llvm_target_array = generator
      .memoize_or_retrieve_value(self.target_id.unwrap(), cache, false)
      .unwrap();

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

// FIXME: [!] Bug: Parameters with the same name on other functions are being resolved elsewhere. This is likely because of the current design of relative scopes. Fix this issue. (May be related to when memoizing or retrieving other functions, then not virtualizing their environment).
impl Lower for ast::Parameter {
  fn lower<'a, 'ctx>(
    &self,
    generator: &mut LlvmGenerator<'a, 'ctx>,
    _cache: &cache::Cache,
  ) -> Option<inkwell::values::BasicValueEnum<'ctx>> {
    // TODO: In the future, consider having an `alloca` per parameter, for simpler tracking of them?

    Some(
      generator
        .llvm_function_buffer
        .unwrap()
        .get_nth_param(self.position)
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
    self.0.lower(generator, cache)
  }
}

impl Lower for ast::BinaryExpr {
  fn lower<'a, 'ctx>(
    &self,
    generator: &mut LlvmGenerator<'a, 'ctx>,
    cache: &cache::Cache,
  ) -> Option<inkwell::values::BasicValueEnum<'ctx>> {
    let mut llvm_left_value = self.left.lower(generator, cache).unwrap();
    let mut llvm_right_value = self.right.lower(generator, cache).unwrap();

    llvm_left_value = generator.attempt_access(llvm_left_value);
    llvm_right_value = generator.attempt_access(llvm_right_value);

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
      ast::OperatorKind::Nand => generator
        .llvm_builder
        .build_not(
          generator.llvm_builder.build_and(
            llvm_left_value.into_int_value(),
            llvm_right_value.into_int_value(),
            "",
          ),
          "nand_op",
        )
        .as_basic_value_enum(),
      ast::OperatorKind::Nor => generator
        .llvm_builder
        .build_not(
          generator.llvm_builder.build_or(
            llvm_left_value.into_int_value(),
            llvm_right_value.into_int_value(),
            "",
          ),
          "nor_op",
        )
        .as_basic_value_enum(),
      // FIXME: Add the `xor` operator.
      // TODO: Support for when comparing equality of pointers/references.
      // TODO: Support for all operators.
      _ => todo!(),
    };

    // TODO: Simplify this to obtain the operator/predicate, then lower separately? Maybe not possible.
    Some(llvm_operation)
  }
}

impl Lower for ast::Reference {
  fn lower<'a, 'ctx>(
    &self,
    generator: &mut LlvmGenerator<'a, 'ctx>,
    cache: &cache::Cache,
  ) -> Option<inkwell::values::BasicValueEnum<'ctx>> {
    // TODO: Here we opted not to forward buffers. Ensure this is correct.
    // FIXME: This may not be working, because the `memoize_or_retrieve` function directly lowers, regardless of expected access or not.

    // FIXME: [!!] Investigate+Revise: What about if there's a nested reference? Will it be intercepted by the flag? Perhaps we can capture the flag here immediately? But wouldn't this affect the order of which reference is actually not lowered, ex. first one in `foo.bar`, which is incorrect?
    let llvm_target = generator
      .memoize_or_retrieve_value(self.0.target_id.unwrap(), cache, false)
      .unwrap();

    // FIXME: [!] Investigate+Revise: Usage of a flag may cause trouble if the buffer is modified several times.
    // Perhaps since the llvm target's buffers aren't saved, it's fine?

    Some(llvm_target)
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
    generator.current_loop_block = Some(llvm_after_block);
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
    let llvm_condition = self.condition.lower(generator, cache).unwrap();
    let llvm_current_function = generator.llvm_function_buffer.unwrap();
    let ty = self.infer_type(cache);
    let yields_expression = !ty.is_unit();
    let mut llvm_if_value = None;

    // Allocate the resulting if-value early on, if applicable.
    if yields_expression {
      let llvm_if_value_type = generator.lower_type(&ty, cache);

      let llvm_if_value_alloca = generator
        .llvm_builder
        .build_alloca(llvm_if_value_type, "if.value");

      llvm_if_value = Some(llvm_if_value_alloca.as_basic_value_enum());
    }

    let llvm_then_block = generator
      .llvm_context
      .append_basic_block(llvm_current_function, "if.then");

    let llvm_after_block = generator
      .llvm_context
      .append_basic_block(llvm_current_function, "if.after");

    let mut llvm_else_block_result = None;
    let mut llvm_else_block_value = None;

    // TODO: Simplify (use a buffer for the next block onto build the cond. br. to).
    if let Some(else_block) = &self.else_block {
      let llvm_else_block = generator
        .llvm_context
        .append_basic_block(llvm_current_function, "if.else");

      llvm_else_block_result = Some(llvm_else_block);

      generator.llvm_builder.build_conditional_branch(
        llvm_condition.into_int_value(),
        llvm_then_block,
        llvm_else_block,
      );

      generator.llvm_builder.position_at_end(llvm_else_block);
      llvm_else_block_value = else_block.lower(generator, cache);

      // FIXME: Is this correct? Or should we be using the `else_block` directly here?
      // Fallthrough if applicable.
      if generator.get_current_block().get_terminator().is_none() {
        generator
          .llvm_builder
          .build_unconditional_branch(llvm_after_block);
      }
    } else {
      // NOTE: At this point, the condition must be verified to be a boolean by the type-checker.
      generator.llvm_builder.build_conditional_branch(
        llvm_condition.into_int_value(),
        llvm_then_block,
        llvm_after_block,
      );
    }

    generator.llvm_builder.position_at_end(llvm_then_block);

    let llvm_then_block_value = self.then_block.lower(generator, cache);

    // FIXME: Is this correct? Or should we be using `get_current_block()` here? Or maybe this is just a special case to not leave the `then` block without a terminator? Investigate.
    // Fallthrough if applicable.
    if generator.get_current_block().get_terminator().is_none() {
      generator
        .llvm_builder
        .build_unconditional_branch(llvm_after_block);
    }

    if yields_expression {
      // TODO: Is it guaranteed to have a first instruction? Think (at this point both block return a value, correct?).
      generator
        .llvm_builder
        .position_before(&llvm_then_block.get_last_instruction().unwrap());

      generator.llvm_builder.build_store(
        llvm_if_value.unwrap().into_pointer_value(),
        llvm_then_block_value.unwrap(),
      );

      // TODO: Is it guaranteed to have a first instruction? Think (at this point both block return a value, correct?).
      generator.llvm_builder.position_before(
        &llvm_else_block_result
          .unwrap()
          .get_last_instruction()
          .unwrap(),
      );

      generator.llvm_builder.build_store(
        llvm_if_value.unwrap().into_pointer_value(),
        llvm_else_block_value.unwrap(),
      );
    }

    generator.llvm_builder.position_at_end(llvm_after_block);

    // If an expression is to be yielded, it must be accessed. A pointer
    // shouldn't be yielded.
    if let Some(llvm_if_value) = llvm_if_value {
      Some(generator.access(llvm_if_value.into_pointer_value()))
    } else {
      None
    }
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
    let is_main = self.name == MAIN_FUNCTION_NAME;

    // TODO: Prepend `fn` to the name.
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

    // FIXME: Still getting stack-overflow errors when using recursive functions (specially multiple of them at the same time). Investigate whether that's caused here or elsewhere.
    // Manually cache the function now to allow for recursive function calls.
    generator.llvm_cached_values.insert(
      generator.pending_function_unique_id.unwrap(),
      llvm_function.as_global_value().as_basic_value_enum(),
    );

    let mut expected_param_count = self.prototype.parameters.len() as u32;

    if self.prototype.accepts_instance {
      expected_param_count += 1;
    }

    assert_eq!(llvm_function.count_params(), expected_param_count);

    // FIXME: [!] Revise: The parameter counts aren't always guaranteed
    // to be the same, given if the prototype accepts an instance. This zip
    // might cause unexpected problems.
    llvm_function
      .get_param_iter()
      .zip(self.prototype.parameters.iter())
      .for_each(|params| {
        params.1.lower(generator, cache);
        params
          .0
          .set_name(format!("param.{}", params.1.name).as_str());
      });

    let llvm_entry_block = generator
      .llvm_context
      .append_basic_block(llvm_function, "fn.entry");

    generator.llvm_builder.position_at_end(llvm_entry_block);

    let yielded_result = self.body.lower(generator, cache);

    generator.attempt_build_return(yielded_result);

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
    for (index, statement) in self.statements.iter().enumerate() {
      if index == self.statements.len() - 1 && self.yield_last_expr {
        return generator.lower_with_access_rules(statement, cache);
      }

      statement.lower(generator, cache);

      // Do not continue lowering statements if the current block was terminated.
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
    // TODO: Simplify.
    let llvm_return_value = if let Some(return_value) = &self.value {
      Some(
        generator
          .lower_with_access_rules(return_value, cache)
          .unwrap(),
      )
    } else {
      None
    };

    generator.attempt_build_return(llvm_return_value);

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
        let llvm_final_value = generator.attempt_access(llvm_value);

        // NOTE: We expect the value to be a boolean. This should be enforced during type-checking.
        generator
          .llvm_builder
          .build_not(llvm_final_value.into_int_value(), "not_op")
          .as_basic_value_enum()
      }
      ast::OperatorKind::SubtractOrNegate => {
        let llvm_value = self.expr.lower(generator, cache).unwrap();
        let llvm_final_value = generator.attempt_access(llvm_value);

        // NOTE: We expect the value to be an integer or float. This should be enforced during type-checking.
        if llvm_value.is_int_value() {
          generator
            .llvm_builder
            .build_int_neg(llvm_final_value.into_int_value(), "int.negate_op")
            .as_basic_value_enum()
        } else {
          generator
            .llvm_builder
            .build_float_neg(llvm_final_value.into_float_value(), "float.negate_op")
            .as_basic_value_enum()
        }
      }
      ast::OperatorKind::AddressOf => {
        // FIXME: Also, have to verify lifetimes. This isn't nearly started.
        // FIXME: Cannot bind references to temporary values. Must only be to existing definitions. This should be enforced during type-checking.
        // FIXME: The expression shouldn't be accessed in this case. Find out how to accomplish this.

        // TODO: For the time being, this isn't being returned. This should be the return value.
        self.expr.lower(generator, cache).unwrap();

        todo!()
      }
      ast::OperatorKind::MultiplyOrDereference => {
        let llvm_value = self.expr.lower(generator, cache).unwrap();

        generator.access(llvm_value.into_pointer_value())
      }
      ast::OperatorKind::Cast => {
        let llvm_value = self.expr.lower(generator, cache).unwrap();
        let llvm_final_value = generator.attempt_access(llvm_value);
        let llvm_to_type = generator.lower_type(self.cast_type.as_ref().unwrap(), cache);

        if !llvm_value.is_int_value() {
          // TODO: Implement for other cases.
          todo!();
        }

        generator.llvm_builder.build_cast(
          // FIXME: Different instruction opcodes depending on whether the target type is bigger or smaller (extend vs. truncate). As well as from different types of values, such as when going from int to float, etc.
          inkwell::values::InstructionOpcode::SExt,
          llvm_final_value,
          llvm_to_type,
          "cast_op",
        )
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
    // TODO: Do we need to resolve here?
    // Special cases. The allocation is done elsewhere.
    if matches!(
      TypeCheckContext::flatten_type(&self.ty),
      ast::Type::Callable(_) | ast::Type::Struct(_)
    ) {
      // TODO: Here create a definition for the closure, with the let statement as the name?

      return self.value.lower(generator, cache);
    }

    // FIXME: [!] Bug: Use type-caching. Declaring multiple variables with the same struct type will cause repeated type definitions.
    let llvm_type = generator.lower_type(&self.ty, cache);

    let llvm_alloca = generator
      .llvm_builder
      .build_alloca(llvm_type, format!("var.{}", self.name).as_str());

    let llvm_value = generator
      .lower_with_access_rules(self.value.as_ref(), cache)
      .unwrap();

    generator.llvm_builder.build_store(llvm_alloca, llvm_value);

    Some(llvm_alloca.as_basic_value_enum())
  }
}

impl Lower for ast::CallExpr {
  fn lower<'a, 'ctx>(
    &self,
    generator: &mut LlvmGenerator<'a, 'ctx>,
    cache: &cache::Cache,
  ) -> Option<inkwell::values::BasicValueEnum<'ctx>> {
    let mut llvm_arguments = self
      .arguments
      .iter()
      .map(|x| generator.lower_with_access_rules(x, cache).unwrap().into())
      .collect::<Vec<_>>();

    // Insert the instance pointer as the first argument, if applicable.
    if let ast::NodeKind::MemberAccess(member_access) = &self.callee_expr.kind {
      // FIXME: This will panic for zero-length vectors. Find another way to prepend elements.
      llvm_arguments.insert(
        0,
        member_access
          .base_expr
          .lower(generator, cache)
          .unwrap()
          .into(),
      );
    }

    // FIXME: It seems that this is causing stack-overflow because results aren't cached? What's going on? Or maybe it's the parser?
    // TODO: Here we opted not to forward buffers. Ensure this is correct.
    let llvm_target_callable = self
      .callee_expr
      .lower(generator, cache)
      .unwrap()
      .into_pointer_value();

    let llvm_call_value = generator.llvm_builder.build_call(
      inkwell::values::CallableValue::try_from(llvm_target_callable).unwrap(),
      llvm_arguments.as_slice(),
      "call",
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
      .build_unconditional_branch(generator.current_loop_block.unwrap());

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
    let node_kind = &(&*node).kind;

    // FIXME: What about closures?
    // Set the pending function definition key to cache the function early.
    // This eliminates problems with multi-borrows that may occur when lowering
    // recursive functions.
    if let ast::NodeKind::Function(_) = node_kind {
      generator.pending_function_unique_id = Some(self.unique_id);
    }

    // TODO: Is there a need to return a value for the main function? Even for `Definition`?
    generator.memoize_or_retrieve_value(self.unique_id, cache, true)
  }
}

impl Lower for ast::InlineExprStmt {
  fn lower<'a, 'ctx>(
    &self,
    generator: &mut LlvmGenerator<'a, 'ctx>,
    cache: &cache::Cache,
  ) -> Option<inkwell::values::BasicValueEnum<'ctx>> {
    self.expr.lower(generator, cache)
  }
}

pub struct LlvmGeneratorBuffers<'ctx> {
  current_loop_block: Option<inkwell::basic_block::BasicBlock<'ctx>>,
  llvm_current_block: Option<inkwell::basic_block::BasicBlock<'ctx>>,
  llvm_function_buffer: Option<inkwell::values::FunctionValue<'ctx>>,
}

pub struct LlvmGenerator<'a, 'ctx> {
  pub module_name: String,
  llvm_context: &'ctx inkwell::context::Context,
  llvm_module: &'a inkwell::module::Module<'ctx>,
  llvm_builder: inkwell::builder::Builder<'ctx>,
  llvm_function_buffer: Option<inkwell::values::FunctionValue<'ctx>>,
  // TODO: Shouldn't this be a vector instead?
  llvm_cached_values:
    std::collections::HashMap<cache::UniqueId, inkwell::values::BasicValueEnum<'ctx>>,
  llvm_cached_types:
    std::collections::HashMap<cache::UniqueId, inkwell::types::BasicTypeEnum<'ctx>>,
  /// The next fall-through block (if any).
  current_loop_block: Option<inkwell::basic_block::BasicBlock<'ctx>>,
  /// The definition key of the function currently being emitted (if any).
  /// Used to be able to handle recursive calls.
  pending_function_unique_id: Option<cache::UniqueId>,
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
      current_loop_block: None,
      pending_function_unique_id: None,
      panic_function_cache: None,
      print_function_cache: None,
      mangle_counter: 0,
    }
  }

  /// Lower a node while applying the access rules.
  fn lower_with_access_rules(
    &mut self,
    node: &ast::Node,
    cache: &cache::Cache,
  ) -> Option<inkwell::values::BasicValueEnum<'ctx>> {
    let llvm_value_result = node.lower(self, cache);
    let node_type = TypeCheckContext::flatten_type(&node.infer_type(cache));

    println!("node type: {:?}", node_type);

    // TODO: Is there a need to resolve the type?
    // FIXME: [!] Bug: This won't work for nested string values. Cannot compare node kind.
    // Special case for string literals. They shouldn't be
    // lowered, since they're natural pointers. And for structs,
    // they must be passed as pointer values.
    if (matches!(node_type, ast::Type::Primitive(ast::PrimitiveType::String))
      && !matches!(node.kind, ast::NodeKind::Reference(_)))
      || matches!(node_type, ast::Type::Struct(_))
    {
      return llvm_value_result;
    }

    if let Some(llvm_value) = llvm_value_result {
      // FIXME: Strings shouldn't be accessed (exception).
      Some(self.attempt_access(llvm_value))
    } else {
      None
    }
  }

  fn is_callable(llvm_value: inkwell::values::BasicValueEnum<'ctx>) -> bool {
    inkwell::values::CallableValue::try_from(llvm_value.into_pointer_value()).is_ok()
  }

  fn copy_buffers(&self) -> LlvmGeneratorBuffers<'ctx> {
    LlvmGeneratorBuffers {
      current_loop_block: self.current_loop_block,
      llvm_current_block: self.llvm_builder.get_insert_block(),
      llvm_function_buffer: self.llvm_function_buffer,
    }
  }

  fn restore_buffers(&mut self, buffers: LlvmGeneratorBuffers<'ctx>) {
    self.current_loop_block = buffers.current_loop_block;
    self.llvm_function_buffer = buffers.llvm_function_buffer;

    if let Some(llvm_current_block) = buffers.llvm_current_block {
      self.llvm_builder.position_at_end(llvm_current_block);
    }
  }

  /// Mangle a name with an unique counter to avoid name collisions.
  fn mangle_name(&mut self, name: &String) -> String {
    // TODO: Consider using the `unique_id` instead?
    // NOTE: The current module name isn't used because it's not guaranteed to be the
    // active module when the definition is memoized (for example, inter-module function
    // calls).
    let mangled_name = format!(".{}.{}", self.mangle_counter, name);

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

  /// Insert a `load` instruction for the given LLVM value.
  ///
  /// Equivalent to a de-reference of a pointer.
  fn access(
    &mut self,
    llvm_value: inkwell::values::PointerValue<'ctx>,
  ) -> inkwell::values::BasicValueEnum<'ctx> {
    // TODO: Do we need an assertion here that the `llvm_value` is an LLVM pointer value? Or does it always throw when its not?

    self
      .llvm_builder
      .build_load(llvm_value, "access")
      .as_basic_value_enum()
  }

  fn attempt_access(
    &mut self,
    llvm_value: inkwell::values::BasicValueEnum<'ctx>,
  ) -> inkwell::values::BasicValueEnum<'ctx> {
    if llvm_value.is_pointer_value() && !LlvmGenerator::is_callable(llvm_value) {
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
    // FIXME: [!] Bug: Use type-caching HERE (for structs, or any type that needs to be cached). Declaring multiple variables with the same struct type will cause repeated type definitions.

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
      ast::Type::Struct(struct_type) => {
        let llvm_field_types = struct_type
          .fields
          .iter()
          .map(|field| self.lower_type(&field.1, cache))
          .collect::<Vec<_>>();

        // TODO: Consider caching the struct type here?

        let name = self.mangle_name(&format!("struct.{}", struct_type.name));
        let llvm_struct_type = self.llvm_context.opaque_struct_type(name.as_str());

        llvm_struct_type.set_body(llvm_field_types.as_slice(), false);

        // All struct types should be pointer types.
        llvm_struct_type
          .ptr_type(inkwell::AddressSpace::Generic)
          .as_basic_type_enum()
      }
      // FIXME: Why not resolve the type if it is a stub type, then proceed to lower it?
      ast::Type::Stub(stub_type) => {
        self.memoize_or_retrieve_type(stub_type.target_id.unwrap(), cache)
      }
      ast::Type::Callable(callable_type) => self
        .lower_callable_type(callable_type, cache)
        .ptr_type(inkwell::AddressSpace::Generic)
        .as_basic_type_enum(),
      // TODO: Implement.
      ast::Type::Reference(_reference_type) => todo!(),
      // FIXME: Implement.
      ast::Type::This(this_type) => {
        self.memoize_or_retrieve_type(this_type.target_id.unwrap(), cache)
      }
      // FIXME: What about when a resolved function type is encountered? Wouldn't it need to be lowered here?
      // TODO: Consider lowering the unit type as void? Only in case we actually use this, otherwise no. (This also serves as a bug catcher).
      // NOTE: These types are never lowered.
      ast::Type::Unit | ast::Type::Error => unreachable!(),
    }
  }

  fn lower_callable_type(
    &mut self,
    callable_type: &ast::CallableType,
    cache: &cache::Cache,
  ) -> inkwell::types::FunctionType<'ctx> {
    let llvm_parameter_types = callable_type
      .parameter_types
      .iter()
      .map(|parameter_type| self.lower_type(&parameter_type, cache).into())
      .collect::<Vec<_>>();

    let llvm_return_type = self.lower_type(&callable_type.return_type, cache);

    llvm_return_type.fn_type(llvm_parameter_types.as_slice(), callable_type.is_variadic)
  }

  fn lower_prototype(
    &mut self,
    prototype: &ast::Prototype,
    cache: &cache::Cache,
  ) -> inkwell::types::FunctionType<'ctx> {
    let mut llvm_parameter_types = prototype
      .parameters
      .iter()
      .map(|x| self.lower_type(&x.infer_type(cache), cache).into())
      .collect::<Vec<_>>();

    if prototype.accepts_instance {
      let llvm_instance_type =
        self.memoize_or_retrieve_type(prototype.instance_type_id.unwrap(), cache);

      // FIXME: This will panic for zero-length vectors. Find another way to prepend elements.
      llvm_parameter_types.insert(
        0,
        llvm_instance_type
          .ptr_type(inkwell::AddressSpace::Generic)
          .into(),
      );
    }

    // TODO: Simplify code (find common ground between `void` and `basic` types).
    if !prototype.return_type.is_unit() {
      self
        .lower_type(&prototype.return_type, cache)
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

  fn memoize_or_retrieve_type(
    &mut self,
    unique_id: cache::UniqueId,
    cache: &cache::Cache,
  ) -> inkwell::types::BasicTypeEnum<'ctx> {
    if let Some(existing_definition) = self.llvm_cached_types.get(&unique_id) {
      return existing_definition.clone();
    }

    // TODO: Consider making a separate map for types in the cache.

    let node = cache.force_get(&unique_id);

    let ty = match &(&*node).kind {
      ast::NodeKind::StructType(struct_type) => struct_type,
      // TODO: Any more?
      _ => unreachable!(),
    };

    let llvm_type = self.lower_type(&ast::Type::Struct(ty.clone()), cache);

    self.llvm_cached_types.insert(unique_id, llvm_type);

    llvm_type
  }

  fn get_current_block(&self) -> inkwell::basic_block::BasicBlock<'ctx> {
    self.llvm_builder.get_insert_block().unwrap()
  }

  fn attempt_build_return(&mut self, return_value: Option<inkwell::values::BasicValueEnum<'ctx>>) {
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

  // TODO: Shouldn't this be used for any and all lowering? Possible problems with closures and/or structs if not?
  // TODO: Accept reference for key?
  /// Attempt to retrieve an existing definition, otherwise proceed to
  /// lowering it and memoizing it under the current module.
  ///
  /// If specified, any modified buffers during the process will be kept,
  /// otherwise they will all be restored.
  fn memoize_or_retrieve_value(
    &mut self,
    unique_id: cache::UniqueId,
    cache: &cache::Cache,
    forward_buffers: bool,
  ) -> Option<inkwell::values::BasicValueEnum<'ctx>> {
    // If the definition has been cached previously, simply retrieve it.
    if let Some(existing_definition) = self.llvm_cached_values.get(&unique_id) {
      // NOTE: The underlying LLVM value is not copied, but rather the reference to it.
      return Some(existing_definition.clone());
    }

    let buffers = self.copy_buffers();
    let llvm_value = cache.force_get(&unique_id).lower(self, cache);

    if let Some(llvm_value) = llvm_value {
      self.llvm_cached_values.insert(unique_id, llvm_value);
    }

    // Restore buffers after processing, if requested.
    if !forward_buffers {
      self.restore_buffers(buffers);
    }

    return llvm_value;
  }
}
