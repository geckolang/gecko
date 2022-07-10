use crate::{
  ast, cache, dispatch,
  type_system::{Check, TypeContext},
};

use inkwell::{types::BasicType, values::BasicValue};
use std::convert::TryFrom;

pub const MAIN_FUNCTION_NAME: &str = "main";

pub trait Lower {
  fn lower<'a, 'ctx>(
    &self,
    _generator: &mut LlvmGenerator<'a, 'ctx>,
    _cache: &cache::Cache,
    _access: bool,
  ) -> Option<inkwell::values::BasicValueEnum<'ctx>> {
    None
  }
}

impl Lower for ast::Node {
  fn lower<'a, 'ctx>(
    &self,
    generator: &mut LlvmGenerator<'a, 'ctx>,
    cache: &cache::Cache,
    access: bool,
  ) -> Option<inkwell::values::BasicValueEnum<'ctx>> {
    dispatch!(&self.kind, Lower::lower, generator, cache, access)
  }
}

impl Lower for ast::NodeKind {
  fn lower<'a, 'ctx>(
    &self,
    generator: &mut LlvmGenerator<'a, 'ctx>,
    cache: &cache::Cache,
    access: bool,
  ) -> Option<inkwell::values::BasicValueEnum<'ctx>> {
    dispatch!(&self, Lower::lower, generator, cache, access)
  }
}

impl Lower for ast::Range {
  // TODO: ?
}

impl Lower for ast::SizeofIntrinsic {
  fn lower<'a, 'ctx>(
    &self,
    generator: &mut LlvmGenerator<'a, 'ctx>,
    cache: &cache::Cache,
    _access: bool,
  ) -> Option<inkwell::values::BasicValueEnum<'ctx>> {
    let ty = generator.memoize_or_retrieve_type(&self.ty, cache);

    Some(ty.size_of().unwrap().as_basic_value_enum())
  }
}

impl Lower for ast::Using {
  //
}

impl Lower for ast::ParenthesesExpr {
  fn lower<'a, 'ctx>(
    &self,
    generator: &mut LlvmGenerator<'a, 'ctx>,
    cache: &cache::Cache,
    access: bool,
  ) -> Option<inkwell::values::BasicValueEnum<'ctx>> {
    // NOTE: The `access` flag is passed down, since this is a transient construct.
    self.0.lower(generator, cache, access)
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
    _access: bool,
  ) -> Option<inkwell::values::BasicValueEnum<'ctx>> {
    for method in &self.member_methods {
      method.lower(generator, cache, false).unwrap();
    }

    None
  }
}

impl Lower for ast::MemberAccess {
  fn lower<'a, 'ctx>(
    &self,
    generator: &mut LlvmGenerator<'a, 'ctx>,
    cache: &cache::Cache,
    access: bool,
  ) -> Option<inkwell::values::BasicValueEnum<'ctx>> {
    let llvm_struct = self
      .base_expr
      .lower(generator, cache, false)
      .unwrap()
      .into_pointer_value();

    // Flatten the type in case it is a `ThisType`.
    let llvm_struct_type = crate::force_match!(
      self.base_expr.kind.infer_flatten_type(cache),
      ast::Type::Struct
    );

    // TODO: Must disallow fields and methods with the same name on semantic check phase.
    // First, check if its a field.
    if let Some(field_index) = llvm_struct_type
      .fields
      .iter()
      .position(|field| field.0 == self.member_name)
    {
      let field_gep = generator
        .llvm_builder
        // REVIEW: Is this conversion safe?
        .build_struct_gep(llvm_struct, field_index as u32, "struct.member.gep")
        .unwrap();

      return Some(if access {
        generator.access(field_gep).as_basic_value_enum()
      } else {
        field_gep.as_basic_value_enum()
      });
    }

    // REVIEW: Is it safe to use the binding id of an inferred struct type?
    // Otherwise, it must be a method.
    let impl_method_info = cache
      .struct_impls
      .get(&llvm_struct_type.cache_id)
      .unwrap()
      .iter()
      .find(|x| x.1 == self.member_name)
      .unwrap();

    // REVIEW: Opted to not use access rules. Ensure this is correct.
    generator.memoize_or_retrieve_value(impl_method_info.0, cache, false, false)
  }
}

impl Lower for ast::Closure {
  fn lower<'a, 'ctx>(
    &self,
    generator: &mut LlvmGenerator<'a, 'ctx>,
    cache: &cache::Cache,
    _access: bool,
  ) -> Option<inkwell::values::BasicValueEnum<'ctx>> {
    // REVIEW: Closures don't have a unique id.
    // ... Don't we need to set the buffer unique id for closures as well?

    let buffers = generator.copy_buffers();
    // let mut modified_prototype = self.prototype.clone();

    for (_index, _capture) in self.captures.iter().enumerate() {
      // let capture_node = cache.force_get(&capture.1.unwrap());
      // let capture_node_type = (&*capture_node).infer_type(cache);
      // let computed_parameter_index = self.prototype.parameters.len() as usize + index;

      // REVIEW: Is the parameter position correct?
      // TODO: Re-implement, after parameters were made definitions.
      // modified_prototype.parameters.push((
      //   format!("capture.{}", capture.0),
      //   capture_node_type,
      //   computed_parameter_index as u32,
      // ))
    }

    // FIXME: Use the modified prototype.
    let llvm_function_type = generator.lower_prototype(
      &self.prototype,
      &TypeContext::infer_return_value_type(&self.body, cache),
      cache,
    );

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

    let yielded_result = self.body.lower(generator, cache, false);

    generator.attempt_build_return(yielded_result);

    // FIXME: Might be missing the same check for never type as function.

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
    _access: bool,
  ) -> Option<inkwell::values::BasicValueEnum<'ctx>> {
    // REVIEW: No need to use the `access` parameter?

    // TODO: Unused.
    let _llvm_arguments = self
      .arguments
      .iter()
      .map(|node| node.kind.lower(generator, cache, true).unwrap().into())
      .collect::<Vec<inkwell::values::BasicMetadataValueEnum<'ctx>>>();

    match self.kind {
      ast::IntrinsicKind::LengthOf => {
        let target_array = self.arguments.first().unwrap();

        let array_static_length = match target_array.kind.infer_flatten_type(cache) {
          ast::Type::Array(_, length) => length,
          _ => unreachable!(),
        };

        Some(
          generator
            .llvm_context
            .i32_type()
            .const_int(array_static_length as u64, false)
            .as_basic_value_enum(),
        )
      }
    }
  }
}

impl Lower for ast::ExternStatic {
  fn lower<'a, 'ctx>(
    &self,
    generator: &mut LlvmGenerator<'a, 'ctx>,
    cache: &cache::Cache,
    _access: bool,
  ) -> Option<inkwell::values::BasicValueEnum<'ctx>> {
    let llvm_global_value = generator.llvm_module.add_global(
      generator.memoize_or_retrieve_type(&self.ty, cache),
      None,
      self.name.as_str(),
    );

    Some(llvm_global_value.as_basic_value_enum())
  }
}

impl Lower for ast::StructValue {
  fn lower<'a, 'ctx>(
    &self,
    generator: &mut LlvmGenerator<'a, 'ctx>,
    cache: &cache::Cache,
    access: bool,
  ) -> Option<inkwell::values::BasicValueEnum<'ctx>> {
    let llvm_struct_type = generator
      .memoize_or_retrieve_type_by_binding(self.target_id.unwrap(), cache)
      .into_struct_type();

    let llvm_struct_alloca = generator.llvm_builder.build_alloca(
      llvm_struct_type,
      format!("struct.{}.alloca", self.struct_name).as_str(),
    );

    for (index, field) in self.fields.iter().enumerate() {
      let struct_field_gep = generator
        .llvm_builder
        // REVIEW: Is this conversion safe?
        .build_struct_gep(llvm_struct_alloca, index as u32, "struct.alloca.field.gep")
        .unwrap();

      let llvm_field_value = field.lower(generator, cache, true).unwrap();

      generator
        .llvm_builder
        // FIXME: For nested structs, they will return `None`.
        .build_store(struct_field_gep, llvm_field_value);
    }

    Some(if access {
      generator.access(llvm_struct_alloca)
    } else {
      llvm_struct_alloca.as_basic_value_enum()
    })
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
    _access: bool,
  ) -> Option<inkwell::values::BasicValueEnum<'ctx>> {
    for (index, variant) in self.variants.iter().enumerate() {
      let llvm_name = generator.mangle_name(&format!("enum.{}.{}", self.name, variant.0));

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

impl Lower for ast::IndexingExpr {
  fn lower<'a, 'ctx>(
    &self,
    generator: &mut LlvmGenerator<'a, 'ctx>,
    cache: &cache::Cache,
    _access: bool,
  ) -> Option<inkwell::values::BasicValueEnum<'ctx>> {
    // TODO: Consider adding support for indexing strings (or better yet, generalized indexing implementation).

    let llvm_index = self
      .index_expr
      .lower(generator, cache, false)
      .unwrap()
      .into_int_value();

    // REVIEW: Opted not to use access rules. Ensure this is correct.
    let llvm_target_array = generator
      .memoize_or_retrieve_value(self.target_id.unwrap(), cache, false, false)
      .unwrap();

    // TODO: Need a way to handle possible segfaults (due to an index being out-of-bounds).
    unsafe {
      // REVIEW: Figure out why there's a zero index (may want to look on `https://www.llvm.org/docs/GetElementPtr.html#why-is-the-extra-0-index-required`).
      let first_index = generator.llvm_context.i32_type().const_int(0, false);

      let llvm_gep_ptr = generator.llvm_builder.build_in_bounds_gep(
        llvm_target_array.into_pointer_value(),
        &[first_index, llvm_index],
        "array.index.gep",
      );

      let target_indexable_type = cache.force_get(&self.target_id.unwrap()).infer_type(cache);

      let target_indexable_size = match target_indexable_type {
        ast::Type::Array(_, size) => size,
        _ => unreachable!(),
      };

      // FIXME: Need to verify proper static+dynamic indexing during type checking.

      // REVIEW: Should we actually be de-referencing the pointer here?
      Some(generator.access(llvm_gep_ptr))
    }
  }
}

impl Lower for ast::StaticArrayValue {
  fn lower<'a, 'ctx>(
    &self,
    generator: &mut LlvmGenerator<'a, 'ctx>,
    cache: &cache::Cache,
    _access: bool,
  ) -> Option<inkwell::values::BasicValueEnum<'ctx>> {
    let llvm_values = self
      .elements
      .iter()
      .map(|element| element.lower(generator, cache, false).unwrap())
      .collect::<Vec<_>>();

    let llvm_array_type = if llvm_values.is_empty() {
      generator.memoize_or_retrieve_type(self.explicit_type.as_ref().unwrap(), cache)
    } else {
      llvm_values.first().unwrap().get_type()
    }
    // REVIEW: Is this cast (usize -> u32) safe?
    .array_type(llvm_values.len() as u32);

    let llvm_array_ptr = generator
      .llvm_builder
      .build_alloca(llvm_array_type, "array.value");

    for (index, llvm_value) in llvm_values.iter().enumerate() {
      let first_index = generator.llvm_context.i32_type().const_int(0, false);

      let llvm_index = generator
        .llvm_context
        .i32_type()
        // REVIEW: Is this conversion safe?
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

    // REVIEW: Might not need to load the array in order to initialize it, but to return it?
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
    _access: bool,
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

impl Lower for ast::UnsafeExpr {
  fn lower<'a, 'ctx>(
    &self,
    generator: &mut LlvmGenerator<'a, 'ctx>,
    cache: &cache::Cache,
    access: bool,
  ) -> Option<inkwell::values::BasicValueEnum<'ctx>> {
    // REVIEW: Is it correct to pass the `access` parameter along? If this will serve as an expression, then yes, otherwise (block) no.
    self.0.lower(generator, cache, access)
  }
}

impl Lower for ast::BinaryExpr {
  fn lower<'a, 'ctx>(
    &self,
    generator: &mut LlvmGenerator<'a, 'ctx>,
    cache: &cache::Cache,
    _access: bool,
  ) -> Option<inkwell::values::BasicValueEnum<'ctx>> {
    let mut llvm_left_value = self.left.lower(generator, cache, false).unwrap();
    let mut llvm_right_value = self.right.lower(generator, cache, false).unwrap();

    // REVIEW: Is it okay to semi-force an access here? Why not instead make use of the `access` parameter?
    // ... Maybe the operands should always be attempted to be accessed? What about strings? Will they ever be a
    // ... binary expression's operand?
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
      // BUG: Need to implement static checks for division by zero.
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
    access: bool,
  ) -> Option<inkwell::values::BasicValueEnum<'ctx>> {
    // REVIEW: Here we opted not to forward buffers. Ensure this is correct.
    // REVIEW: This may not be working, because the `memoize_or_retrieve` function directly lowers, regardless of expected access or not.
    let llvm_target = generator
      .memoize_or_retrieve_value(self.pattern.target_id.unwrap(), cache, false, access)
      .unwrap();

    Some(llvm_target)
  }
}

impl Lower for ast::IfExpr {
  fn lower<'a, 'ctx>(
    &self,
    generator: &mut LlvmGenerator<'a, 'ctx>,
    cache: &cache::Cache,
    _access: bool,
  ) -> Option<inkwell::values::BasicValueEnum<'ctx>> {
    // TODO: Process alternative branches.

    let llvm_condition = self.condition.lower(generator, cache, false).unwrap();
    let llvm_current_function = generator.llvm_function_buffer.unwrap();
    let ty = self.infer_type(cache).flatten(cache);

    // This if-expression will never yield a value if its type
    // is unit or never.
    let yields_expression = !ty.is_a_meta();

    let mut llvm_if_value = None;

    // Allocate the resulting if-value early on, if applicable.
    if yields_expression {
      let llvm_if_value_type = generator.memoize_or_retrieve_type(&ty, cache);

      let llvm_if_value_alloca = generator
        .llvm_builder
        .build_alloca(llvm_if_value_type, "if.value");

      llvm_if_value = Some(llvm_if_value_alloca.as_basic_value_enum());
    }

    let llvm_then_block = generator
      .llvm_context
      .append_basic_block(llvm_current_function, "if.then");

    // FIXME: Only add the `after` block if the `then` block doesn't terminate.
    let llvm_after_block = generator
      .llvm_context
      .append_basic_block(llvm_current_function, "if.after");

    let mut llvm_else_block_result = None;
    let mut llvm_else_block_value = None;

    // TODO: Simplify (use a buffer for the next block onto build the cond. br. to).
    if let Some(else_block) = &self.else_expr {
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
      llvm_else_block_value = else_block.lower(generator, cache, false);

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

    let llvm_then_block_value = self.then_expr.lower(generator, cache, false);

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

    // Leave the after block as current for further processing.
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
    cache: &cache::Cache,
    _access: bool,
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
        .const_int(value.clone() as u64, false)
        .as_basic_value_enum(),
      ast::Literal::Bool(value) => generator
        .llvm_context
        .bool_type()
        .const_int(value.clone() as u64, false)
        .as_basic_value_enum(),
      ast::Literal::String(value) => generator
        .llvm_builder
        .build_global_string_ptr(value.as_str(), "string_literal")
        .as_basic_value_enum(),
      ast::Literal::Nullptr(ty) => generator
        .lower_type(ty, cache)
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
    _access: bool,
  ) -> Option<inkwell::values::BasicValueEnum<'ctx>> {
    let return_type = TypeContext::infer_return_value_type(&self.body, cache);

    let llvm_function_type = generator.lower_prototype(&self.prototype, &return_type, cache);

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
      self.cache_id,
      llvm_function.as_global_value().as_basic_value_enum(),
    );

    // REVIEW: Is this conversion safe?
    let expected_param_count = self.prototype.parameters.len() as u32;

    assert_eq!(
      llvm_function.count_params(),
      if self.prototype.accepts_instance {
        expected_param_count + 1
      } else {
        expected_param_count
      }
    );

    // REVISE: The parameter counts aren't always guaranteed to be the same, given
    // ... if the prototype accepts an instance. This zip might cause unexpected problems.
    llvm_function
      .get_param_iter()
      .zip(self.prototype.parameters.iter())
      .for_each(|params| {
        params
          .0
          .set_name(format!("param.{}", params.1.name).as_str());
      });

    let llvm_entry_block = generator
      .llvm_context
      .append_basic_block(llvm_function, "fn.entry");

    generator.llvm_builder.position_at_end(llvm_entry_block);

    let yielded_result = self.body.lower(generator, cache, false);

    // FIXME: Abstract this logic for use within `closure`, and possibly wherever else this is needed, guided by calls to `attempt_build_return`?
    // If a block was left for further processing, and it has no terminator,
    // complete it here.
    if generator.get_current_block().get_terminator().is_none()
      && self.body.infer_type(cache).flatten(cache).is_a_never()
    {
      generator.llvm_builder.build_unreachable();
    } else {
      generator.attempt_build_return(yielded_result);
    }

    generator.llvm_function_buffer = None;

    Some(llvm_function.as_global_value().as_basic_value_enum())
  }
}

impl Lower for ast::ExternFunction {
  fn lower<'a, 'ctx>(
    &self,
    generator: &mut LlvmGenerator<'a, 'ctx>,
    cache: &cache::Cache,
    _access: bool,
  ) -> Option<inkwell::values::BasicValueEnum<'ctx>> {
    // NOTE: The return type is always explicitly-given for extern functions.
    let llvm_function_type = generator.lower_prototype(
      &self.prototype,
      self
        .prototype
        .return_type_hint
        .as_ref()
        .unwrap_or(&ast::Type::Unit),
      cache,
    );

    // TODO: Need to handle if the function already exists.

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

impl Lower for ast::BlockExpr {
  fn lower<'a, 'ctx>(
    &self,
    generator: &mut LlvmGenerator<'a, 'ctx>,
    cache: &cache::Cache,
    _access: bool,
  ) -> Option<inkwell::values::BasicValueEnum<'ctx>> {
    for statement in &self.statements {
      // FIXME: Some binding statements (such as let-statement) need to be manually
      // ... cached in the generator, this is because not all calls to lower it are made
      // ... using the `memoize_or_retrieve_value` helper function (such as this one!).
      statement.lower(generator, cache, false);

      // Do not continue lowering statements if the current block was terminated.
      if generator.get_current_block().get_terminator().is_some() {
        break;
      }
    }

    // REVIEW: This syntax may replace if/else expressions?
    // self.yields.as_ref().map(|value| {
    //   generator
    //     .lower_with_access_rules(&value.kind, cache)
    //     // TODO: Why doesn't the one below unwrap?
    //     .unwrap()
    // })

    if let Some(yields_value) = &self.yields {
      generator.lower_with_access_rules(&yields_value.kind, cache)
    } else {
      None
    }
  }
}

impl Lower for ast::ReturnStmt {
  fn lower<'a, 'ctx>(
    &self,
    generator: &mut LlvmGenerator<'a, 'ctx>,
    cache: &cache::Cache,
    _access: bool,
  ) -> Option<inkwell::values::BasicValueEnum<'ctx>> {
    let llvm_return_value = if let Some(return_value) = &self.value {
      Some(
        generator
          .lower_with_access_rules(&return_value.kind, cache)
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
    _access: bool,
  ) -> Option<inkwell::values::BasicValueEnum<'ctx>> {
    Some(match self.operator {
      ast::OperatorKind::Not => {
        let llvm_value = self.expr.lower(generator, cache, false).unwrap();
        let llvm_final_value = generator.attempt_access(llvm_value);

        // NOTE: We expect the value to be a boolean. This should be enforced during type-checking.
        generator
          .llvm_builder
          .build_not(llvm_final_value.into_int_value(), "not_op")
          .as_basic_value_enum()
      }
      ast::OperatorKind::SubtractOrNegate => {
        let llvm_value = self.expr.lower(generator, cache, false).unwrap();
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
        self.expr.lower(generator, cache, false).unwrap()

        // TODO: Continue implementation (if necessary).
        // todo!()
      }
      ast::OperatorKind::MultiplyOrDereference => {
        let llvm_value = self.expr.lower(generator, cache, false).unwrap();

        // BUG: If the value is a reference to a let-statement, the pointer of
        // ... the let-statement will be removed, but the actual pointer value will
        // ... not be accessed. Perhaps will need to remove special treatment of let-statements.
        generator.access(llvm_value.into_pointer_value())
      }
      ast::OperatorKind::Cast => {
        let llvm_value = self.expr.lower(generator, cache, false).unwrap();
        let llvm_final_value = generator.attempt_access(llvm_value);

        let llvm_to_type =
          generator.memoize_or_retrieve_type(self.cast_type.as_ref().unwrap(), cache);

        if !llvm_value.is_int_value() {
          // TODO: Implement for other cases.
          todo!();
        }

        generator.llvm_builder.build_cast(
          // FIXME: Different instruction opcodes depending on whether the target type
          // ... is bigger or smaller (extend vs. truncate). As well as from different
          // ... types of values, such as when going from int to float, etc.
          inkwell::values::InstructionOpcode::Trunc,
          llvm_final_value,
          llvm_to_type,
          "cast_op",
        )
      }
      _ => unreachable!(),
    })
  }
}

impl Lower for ast::BindingStmt {
  fn lower<'a, 'ctx>(
    &self,
    generator: &mut LlvmGenerator<'a, 'ctx>,
    cache: &cache::Cache,
    _access: bool,
  ) -> Option<inkwell::values::BasicValueEnum<'ctx>> {
    // REVIEW: Should we be lowering const expressions as global constants? What benefits does that provide? What about scoping?

    // REVISE: Optimize. The type for this construct may be cached.
    let value_type = self.value.kind.infer_flatten_type(cache);

    // Special cases. The allocation is done elsewhere.
    if matches!(value_type, ast::Type::Function(_)) {
      // REVISE: Cleanup the caching code.
      // REVIEW: Here create a definition for the closure, with the let statement as the name?

      let result = self.value.lower(generator, cache, false);

      // REVIEW: Won't let-statements always have a value?
      if let Some(llvm_value) = result {
        generator
          .llvm_cached_values
          .insert(self.cache_id, llvm_value);
      }

      return result;
    }

    let llvm_value_result = self.value.lower(generator, cache, true);

    // FIXME: What about for other things that may be in the same situation (their values are unit)?
    // Do not proceed if the value will never evaluate.
    if value_type.is_a_meta() {
      // REVIEW: Returning `None` here but below we return `Some()`.
      // ... What expects a value out of this, and would this decision affect that?
      return None;
    }

    let llvm_value = llvm_value_result.unwrap();
    let llvm_type = generator.memoize_or_retrieve_type(&value_type, cache);

    let llvm_alloca = generator
      .llvm_builder
      .build_alloca(llvm_type, format!("var.{}", self.name).as_str());

    generator.llvm_builder.build_store(llvm_alloca, llvm_value);

    let result = llvm_alloca.as_basic_value_enum();

    generator.llvm_cached_values.insert(self.cache_id, result);

    // BUG: This needs to return `Some` for the value of the binding-statement to be memoized.
    // ... However, this also implies that the binding-statement itself yields a value!
    Some(result)
  }
}

impl Lower for ast::CallExpr {
  fn lower<'a, 'ctx>(
    &self,
    generator: &mut LlvmGenerator<'a, 'ctx>,
    cache: &cache::Cache,
    _access: bool,
  ) -> Option<inkwell::values::BasicValueEnum<'ctx>> {
    let mut llvm_arguments = self
      .arguments
      .iter()
      .map(|node| node.kind.lower(generator, cache, true).unwrap().into())
      .collect::<Vec<_>>();

    // Insert the instance pointer as the first argument, if applicable.
    if let ast::NodeKind::MemberAccess(member_access) = &self.callee_expr.kind {
      // FIXME: This will panic for zero-length vectors. Find another way to prepend elements.
      llvm_arguments.insert(
        0,
        member_access
          .base_expr
          .lower(generator, cache, false)
          .unwrap()
          .into(),
      );
    }

    // BUG: It seems that this is causing stack-overflow because results aren't cached? What's going on? Or maybe it's the parser?
    // REVIEW: Here we opted not to forward buffers. Ensure this is correct.
    let llvm_target_callable = self
      .callee_expr
      .lower(generator, cache, false)
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

impl Lower for ast::InlineExprStmt {
  fn lower<'a, 'ctx>(
    &self,
    generator: &mut LlvmGenerator<'a, 'ctx>,
    cache: &cache::Cache,
    access: bool,
  ) -> Option<inkwell::values::BasicValueEnum<'ctx>> {
    // REVIEW: Is it correct to pass the `access` parameter here?
    self.expr.lower(generator, cache, access)
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
  pub(super) llvm_builder: inkwell::builder::Builder<'ctx>,
  pub(super) llvm_function_buffer: Option<inkwell::values::FunctionValue<'ctx>>,
  // TODO: Shouldn't this be a vector instead?
  llvm_cached_values: std::collections::HashMap<cache::Id, inkwell::values::BasicValueEnum<'ctx>>,
  llvm_cached_types: std::collections::HashMap<cache::Id, inkwell::types::BasicTypeEnum<'ctx>>,
  /// The next fall-through block (if any).
  pub(super) current_loop_block: Option<inkwell::basic_block::BasicBlock<'ctx>>,
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
      mangle_counter: 0,
    }
  }

  /// Lower a node while applying the access rules.
  fn lower_with_access_rules(
    &mut self,
    node: &ast::NodeKind,
    cache: &cache::Cache,
  ) -> Option<inkwell::values::BasicValueEnum<'ctx>> {
    let llvm_value_result = node.lower(self, cache, true);

    if let Some(llvm_value) = llvm_value_result {
      Some(self.apply_access_rules(node, llvm_value, cache))
    } else {
      None
    }
  }

  fn apply_access_rules(
    &mut self,
    node: &ast::NodeKind,
    llvm_value: inkwell::values::BasicValueEnum<'ctx>,
    cache: &cache::Cache,
  ) -> inkwell::values::BasicValueEnum<'ctx> {
    // REVISE: Need to remove the exclusive logic for let-statements / references (or simplify it).
    // REVIEW: Isn't there a need to resolve the type?

    if matches!(node, ast::NodeKind::Parameter(_)) {
      return llvm_value;
    }

    if let ast::NodeKind::Reference(reference) = &node {
      let target = cache
        .symbols
        .get(&reference.pattern.target_id.unwrap())
        .unwrap();

      // TODO: Ensure this recursive nature doesn't cause stack overflow.
      return self.apply_access_rules(target, llvm_value, cache);
    }

    self.attempt_access(llvm_value)
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
    // NOTE: The current module name isn't used because it's not guaranteed to be the
    // active module when the definition is memoized (for example, inter-module function
    // calls).
    let mangled_name = format!(".{}.{}", self.mangle_counter, name);

    self.mangle_counter += 1;

    mangled_name
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
    if llvm_value.is_pointer_value() && !LlvmGenerator::is_callable(llvm_value) {
      self.access(llvm_value.into_pointer_value())
    } else {
      llvm_value
    }
  }

  fn find_type_cache_id(&self, ty: &ast::Type, cache: &cache::Cache) -> Option<cache::Id> {
    Some(match ty.flatten(cache) {
      ast::Type::Struct(struct_type) => struct_type.cache_id.clone(),
      // REVIEW: Any more?
      _ => return None,
    })
  }

  // REVIEW: Ensure that this function is tail-recursive.
  fn lower_type(
    &mut self,
    ty: &ast::Type,
    cache: &cache::Cache,
  ) -> inkwell::types::BasicTypeEnum<'ctx> {
    match ty {
      ast::Type::Basic(primitive_type) => match primitive_type {
        ast::BasicType::Bool => self.llvm_context.bool_type().as_basic_type_enum(),
        ast::BasicType::Int(size) => {
          // REVIEW: Should we handle unsigned integers here?

          let llvm_int_type = self.llvm_context.custom_width_int_type(match size {
            ast::IntSize::I8 | ast::IntSize::U8 => 8,
            ast::IntSize::I16 | ast::IntSize::U16 => 16,
            ast::IntSize::I32 | ast::IntSize::U32 => 32,
            ast::IntSize::I64 | ast::IntSize::U64 => 64,
            ast::IntSize::Isize | ast::IntSize::Usize => 128,
          });

          llvm_int_type.as_basic_type_enum()
        }
        ast::BasicType::Char => self.llvm_context.i8_type().as_basic_type_enum(),
        ast::BasicType::String => self
          .llvm_context
          .i8_type()
          .ptr_type(inkwell::AddressSpace::Generic)
          .as_basic_type_enum(),
        // NOTE: The null primitive type is never lowered, only the nullptr value.
        ast::BasicType::Null => unreachable!(),
      },
      ast::Type::Array(element_type, size) => self
        .lower_type(&element_type, cache)
        .array_type(size.clone())
        .as_basic_type_enum(),
      ast::Type::Pointer(pointee_type) => self
        .lower_type(&pointee_type, cache)
        .ptr_type(inkwell::AddressSpace::Generic)
        .as_basic_type_enum(),
      // REVIEW: Is this redundant (because of `StubType`)?
      ast::Type::Struct(struct_type) => {
        let llvm_field_types = struct_type
          .fields
          .iter()
          .map(|field| self.lower_type(&field.1, cache))
          .collect::<Vec<_>>();

        let name = self.mangle_name(&format!("struct.{}", struct_type.name));
        let llvm_struct_type = self.llvm_context.opaque_struct_type(name.as_str());

        llvm_struct_type.set_body(llvm_field_types.as_slice(), false);

        llvm_struct_type.as_basic_type_enum()
      }
      // REVIEW: Why not resolve the type if it is a stub type, then proceed to lower it?
      ast::Type::Stub(stub_type) => {
        self.memoize_or_retrieve_type_by_binding(stub_type.pattern.target_id.unwrap(), cache)
      }
      ast::Type::Function(callable_type) => self
        .lower_callable_type(callable_type, cache)
        .ptr_type(inkwell::AddressSpace::Generic)
        .as_basic_type_enum(),
      // TODO: Implement.
      ast::Type::Reference(_reference_type) => todo!(),
      // TODO: Implement.
      ast::Type::This(this_type) => {
        self.memoize_or_retrieve_type_by_binding(this_type.target_id.unwrap(), cache)
      }
      // FIXME: Will never be able to treat void type as we expect here, because it is only usable to create void return types!
      // FIXME: Should be lowering to the void type instead, but not allowed by return type!
      // FIXME: What about when a resolved function type is encountered? Wouldn't it need to be lowered here?
      // REVIEW: Consider lowering the unit type as void? Only in case we actually use this, otherwise no. (This also serves as a bug catcher).
      // ast::Type::Unit => self
      //   .llvm_context
      //   .bool_type()
      //   .ptr_type(inkwell::AddressSpace::Generic)
      //   .as_basic_type_enum(),
      // Meta types are never to be lowered.
      ast::Type::Unit
      | ast::Type::Variable(_)
      | ast::Type::MetaInteger
      | ast::Type::Never
      | ast::Type::Any => {
        unreachable!()
      }
    }
  }

  fn lower_callable_type(
    &mut self,
    function_type: &ast::FunctionType,
    cache: &cache::Cache,
  ) -> inkwell::types::FunctionType<'ctx> {
    let llvm_parameter_types = function_type
      .parameter_types
      .iter()
      .map(|parameter_type| self.lower_type(&parameter_type, cache).into())
      .collect::<Vec<_>>();

    let llvm_return_type = self.lower_type(&function_type.return_type, cache);

    llvm_return_type.fn_type(llvm_parameter_types.as_slice(), function_type.is_variadic)
  }

  /// Returns a new LLVM function type based on the given prototype.
  ///
  /// The return value is required because the prototype's return type is
  /// merely an annotation, and is unreliable. Assumes the return type to be
  /// flat.
  fn lower_prototype(
    &mut self,
    prototype: &ast::Prototype,
    return_type: &ast::Type,
    cache: &cache::Cache,
  ) -> inkwell::types::FunctionType<'ctx> {
    let mut llvm_parameter_types = prototype
      .parameters
      .iter()
      .map(|parameter| {
        self
          .memoize_or_retrieve_type(&parameter.type_hint.as_ref().unwrap(), cache)
          .into()
      })
      .collect::<Vec<_>>();

    if prototype.accepts_instance {
      let llvm_instance_type =
        self.memoize_or_retrieve_type_by_binding(prototype.instance_type_id.unwrap(), cache);

      // FIXME: This will panic for zero-length vectors. Find another way to prepend elements.
      llvm_parameter_types.insert(
        0,
        llvm_instance_type
          .ptr_type(inkwell::AddressSpace::Generic)
          .into(),
      );
    }

    if !return_type.is_a_never() && !return_type.is_a_unit() {
      self
        .memoize_or_retrieve_type(return_type, cache)
        .fn_type(llvm_parameter_types.as_slice(), prototype.is_variadic)
        .ptr_type(inkwell::AddressSpace::Generic)
        .into()
    } else {
      self
        .llvm_context
        .void_type()
        .fn_type(llvm_parameter_types.as_slice(), prototype.is_variadic)
        .ptr_type(inkwell::AddressSpace::Generic)
        .as_basic_type_enum()
    }
    .into_pointer_type()
    .get_element_type()
    .into_function_type()
  }

  // TODO: Ensure that there isn't any possible recursion problems going on.
  fn memoize_or_retrieve_type(
    &mut self,
    ty: &ast::Type,
    cache: &cache::Cache,
  ) -> inkwell::types::BasicTypeEnum<'ctx> {
    if let Some(cache_id) = self.find_type_cache_id(ty, cache) {
      // REVIEW: Isn't this indirectly recursive? Will it cause problems?
      return self.memoize_or_retrieve_type_by_binding(cache_id, cache);
    }

    self.lower_type(ty, cache)
  }

  fn memoize_or_retrieve_type_by_binding(
    &mut self,
    cache_id: cache::Id,
    cache: &cache::Cache,
  ) -> inkwell::types::BasicTypeEnum<'ctx> {
    if let Some(existing_definition) = self.llvm_cached_types.get(&cache_id) {
      return existing_definition.clone();
    }

    // REVIEW: Consider making a separate map for types in the cache.

    let node = cache.force_get(&cache_id);

    // REVIEW: Why not perform type-flattening here instead?
    let ty = match &node {
      ast::NodeKind::StructType(struct_type) => ast::Type::Struct(struct_type.clone()),
      ast::NodeKind::TypeAlias(type_alias) => type_alias.ty.clone(),
      // REVIEW: Any more?
      _ => unreachable!(),
    };

    let llvm_type = self.lower_type(&ty, cache);

    self.llvm_cached_types.insert(cache_id, llvm_type);

    llvm_type
  }

  fn get_current_block(&self) -> inkwell::basic_block::BasicBlock<'ctx> {
    self.llvm_builder.get_insert_block().unwrap()
  }

  fn attempt_build_return(&mut self, return_value: Option<inkwell::values::BasicValueEnum<'ctx>>) {
    // REVIEW: Consider mixing this with the function's terminator check, for void functions?
    // Only build a single return instruction per block.
    if self.get_current_block().get_terminator().is_some() {
      return;
    } else if let Some(return_value) = return_value {
      self.llvm_builder.build_return(Some(&return_value));
    } else {
      self.llvm_builder.build_return(None);
    }
  }

  // REVIEW: Shouldn't this be used for any and all lowering? Possible problems with closures and/or structs if not?
  /// Attempt to retrieve an existing definition, otherwise proceed to
  /// lowering it and memoizing it under the current module.
  ///
  /// If specified, any modified buffers during the process will be kept,
  /// otherwise they will all be restored.
  fn memoize_or_retrieve_value(
    &mut self,
    cache_id: cache::Id,
    cache: &cache::Cache,
    forward_buffers: bool,
    apply_access_rules: bool,
  ) -> Option<inkwell::values::BasicValueEnum<'ctx>> {
    let node = cache.force_get(&cache_id);

    // BUG: If a definition is lowered elsewhere without the use of this function,
    // ... there will not be any call to `lower_with_access_rules` for it. This means
    // ... that those definitions will be cached as `PointerValue`s instead of possibly
    // ... needing to be lowered.
    // If the definition has been cached previously, simply retrieve it.
    if let Some(existing_definition) = self.llvm_cached_values.get(&cache_id) {
      // NOTE: The underlying LLVM value is not copied, but rather the reference to it.
      let existing_value_cloned = existing_definition.clone();

      return Some(if apply_access_rules {
        self.apply_access_rules(node, existing_value_cloned, cache)
      } else {
        existing_value_cloned
      });
    }

    let buffers = self.copy_buffers();
    let llvm_value_result = node.lower(self, cache, false);

    let result = if let Some(llvm_value) = llvm_value_result {
      // Cache the value without applying access rules.
      // This way, access rules may be chosen to be applied upon cache retrieval.
      self.llvm_cached_values.insert(cache_id, llvm_value);

      Some(if apply_access_rules {
        self.apply_access_rules(node, llvm_value, cache)
      } else {
        llvm_value
      })
    } else {
      None
    };

    // Restore buffers after processing, if requested.
    if !forward_buffers {
      self.restore_buffers(buffers);
    }

    result
  }
}

#[cfg(test)]
mod tests {
  use super::*;
  use crate::mock::tests::{ComparableMock, Mock};

  // TODO: Test mocking helpers themselves (in their own file).

  #[test]
  fn proper_initial_values() {
    // TODO:
  }

  #[test]
  fn lower_binding_stmt_const_val() {
    let llvm_context = inkwell::context::Context::create();
    let llvm_module = llvm_context.create_module("test");

    let binding_stmt = ast::NodeKind::BindingStmt(ast::BindingStmt {
      name: "a".to_string(),
      value: Mock::boxed_node(Mock::literal_int()),
      is_const_expr: false,
      cache_id: 0,
      type_hint: Some(ast::Type::Basic(ast::BasicType::Int(ast::IntSize::I32))),
    });

    Mock::new(&llvm_context, &llvm_module)
      .function()
      .lower(&binding_stmt, false)
      // TODO: Change name to `binding_stmt_const_val`, and the others.
      .compare_with_file("let_stmt_const_val");
  }

  #[test]
  fn lower_binding_stmt_ref_val() {
    let llvm_context = inkwell::context::Context::create();
    let llvm_module = llvm_context.create_module("test");
    let a_cache_id: cache::Id = 0;

    let binding_stmt_a = ast::NodeKind::BindingStmt(ast::BindingStmt {
      name: "a".to_string(),
      value: Mock::boxed_node(Mock::literal_int()),
      is_const_expr: false,
      cache_id: a_cache_id,
      type_hint: Some(ast::Type::Basic(ast::BasicType::Int(ast::IntSize::I32))),
    });

    let binding_stmt_b = ast::NodeKind::BindingStmt(ast::BindingStmt {
      name: "b".to_string(),
      value: Mock::reference(a_cache_id),
      is_const_expr: false,
      cache_id: a_cache_id + 1,
      type_hint: Some(ast::Type::Basic(ast::BasicType::Int(ast::IntSize::I32))),
    });

    Mock::new(&llvm_context, &llvm_module)
      .cache(binding_stmt_a, a_cache_id)
      .function()
      .lower_cache(a_cache_id, false)
      .lower(&binding_stmt_b, false)
      .compare_with_file("let_stmt_ref_val");
  }

  #[test]
  fn lower_binding_stmt_nullptr_val() {
    let llvm_context = inkwell::context::Context::create();
    let llvm_module = llvm_context.create_module("test");
    let ty = ast::Type::Basic(ast::BasicType::Int(ast::IntSize::I32));

    let binding_stmt = ast::NodeKind::BindingStmt(ast::BindingStmt {
      name: "a".to_string(),
      value: Mock::boxed_node(ast::NodeKind::Literal(ast::Literal::Nullptr(ty))),
      is_const_expr: false,
      cache_id: 0,
      // FIXME: Wrong type.
      type_hint: Some(ast::Type::Basic(ast::BasicType::Int(ast::IntSize::I32))),
    });

    Mock::new(&llvm_context, &llvm_module)
      .function()
      .lower(&binding_stmt, false)
      .compare_with_file("let_stmt_nullptr_val");
  }

  #[test]
  fn lower_binding_stmt_ptr_ref_val() {
    let llvm_context = inkwell::context::Context::create();
    let llvm_module = llvm_context.create_module("test");
    let ty = ast::Type::Basic(ast::BasicType::Int(ast::IntSize::I32));
    let a_cache_id: cache::Id = 0;

    let binding_stmt_a = ast::NodeKind::BindingStmt(ast::BindingStmt {
      name: "a".to_string(),
      value: Mock::boxed_node(ast::NodeKind::Literal(ast::Literal::Nullptr(ty.clone()))),
      is_const_expr: false,
      cache_id: a_cache_id,
      // FIXME: Wrong type.
      type_hint: Some(ast::Type::Basic(ast::BasicType::Int(ast::IntSize::I32))),
    });

    let binding_stmt_b = ast::NodeKind::BindingStmt(ast::BindingStmt {
      name: "b".to_string(),
      value: Mock::reference(a_cache_id),
      is_const_expr: false,
      cache_id: a_cache_id + 1,
      // FIXME: Wrong type.
      type_hint: Some(ast::Type::Basic(ast::BasicType::Int(ast::IntSize::I32))),
    });

    Mock::new(&llvm_context, &llvm_module)
      .cache(binding_stmt_a, a_cache_id)
      .function()
      .lower_cache(a_cache_id, false)
      .lower(&binding_stmt_b, false)
      .compare_with_file("let_stmt_ptr_ref_val");
  }

  #[test]
  fn lower_binding_stmt_string_val() {
    let llvm_context = inkwell::context::Context::create();
    let llvm_module = llvm_context.create_module("test");

    let binding_stmt = ast::NodeKind::BindingStmt(ast::BindingStmt {
      name: "a".to_string(),
      value: Mock::boxed_node(ast::NodeKind::Literal(ast::Literal::String(
        "hello".to_string(),
      ))),
      is_const_expr: false,
      cache_id: 0,
      // FIXME: Wrong type.
      type_hint: Some(ast::Type::Basic(ast::BasicType::Int(ast::IntSize::I32))),
    });

    Mock::new(&llvm_context, &llvm_module)
      .function()
      .lower(&binding_stmt, false)
      .compare_with_file("let_stmt_string_val");
  }

  #[test]
  fn lower_enum() {
    let llvm_context = inkwell::context::Context::create();
    let llvm_module = llvm_context.create_module("test");

    let enum_ = ast::NodeKind::Enum(ast::Enum {
      name: "a".to_string(),
      variants: vec![("b".to_string(), 0), ("c".to_string(), 1)],
      ty: ast::BasicType::Int(ast::IntSize::I32),
      cache_id: 0,
    });

    Mock::new(&llvm_context, &llvm_module)
      .module()
      .lower(&enum_, false)
      .compare_with_file("enum");
  }

  #[test]
  fn lower_return_stmt_unit() {
    let llvm_context = inkwell::context::Context::create();
    let llvm_module = llvm_context.create_module("test");
    let return_stmt = ast::NodeKind::ReturnStmt(ast::ReturnStmt { value: None });

    Mock::new(&llvm_context, &llvm_module)
      .function()
      .lower(&return_stmt, false)
      .compare_with_file("return_stmt_unit");
  }

  #[test]
  fn lower_return_stmt() {
    let llvm_context = inkwell::context::Context::create();
    let llvm_module = llvm_context.create_module("test");

    let return_stmt = ast::NodeKind::ReturnStmt(ast::ReturnStmt {
      value: Some(Mock::boxed_node(Mock::literal_int())),
    });

    Mock::new(&llvm_context, &llvm_module)
      .function()
      .lower(&return_stmt, false)
      .compare_with_file("return_stmt");
  }

  #[test]
  fn lower_extern_fn() {
    let llvm_context = inkwell::context::Context::create();
    let llvm_module = llvm_context.create_module("test");

    let extern_fn = ast::NodeKind::ExternFunction(ast::ExternFunction {
      name: "a".to_string(),
      prototype: Mock::prototype_simple(true),
      attributes: Vec::new(),
      cache_id: 0,
    });

    Mock::new(&llvm_context, &llvm_module)
      .module()
      .lower(&extern_fn, false)
      .compare_with_file("extern_fn");
  }

  #[test]
  fn lower_extern_static() {
    let llvm_context = inkwell::context::Context::create();
    let llvm_module = llvm_context.create_module("test");

    let extern_static = ast::NodeKind::ExternStatic(ast::ExternStatic {
      name: "a".to_string(),
      ty: ast::Type::Basic(ast::BasicType::Int(ast::IntSize::I32)),
      cache_id: 0,
    });

    Mock::new(&llvm_context, &llvm_module)
      .module()
      .lower(&extern_static, false)
      .compare_with_file("extern_static");
  }

  #[test]
  fn lower_if_expr_simple() {
    let llvm_context = inkwell::context::Context::create();
    let llvm_module = llvm_context.create_module("test");

    let if_expr = ast::NodeKind::IfExpr(ast::IfExpr {
      condition: Mock::boxed_node(ast::NodeKind::Literal(ast::Literal::Bool(true))),
      then_expr: Mock::boxed_node(Mock::literal_int()),
      alternative_branches: Vec::new(),
      else_expr: None,
    });

    Mock::new(&llvm_context, &llvm_module)
      .function()
      .lower(&if_expr, false)
      .compare_with_file("if_expr_simple");
  }
}
