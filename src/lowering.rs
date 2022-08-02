// FIXME: Absolutely no need to infer types anywhere in here. Simplify this without the
// ... inference / retrieval of types.

use crate::{
  ast, symbol_table, type_inference,
  visitor::{self, LoweringVisitor},
};

use inkwell::{types::BasicType, values::BasicValue};
use std::convert::TryFrom;

pub const MAIN_FUNCTION_NAME: &str = "main";

pub struct LlvmGeneratorBuffers<'ctx> {
  current_loop_block: Option<inkwell::basic_block::BasicBlock<'ctx>>,
  llvm_current_block: Option<inkwell::basic_block::BasicBlock<'ctx>>,
  llvm_function_buffer: Option<inkwell::values::FunctionValue<'ctx>>,
}

pub struct LoweringContext<'a, 'ctx> {
  /// The next fall-through block (if any).
  pub(super) current_loop_block: Option<inkwell::basic_block::BasicBlock<'ctx>>,
  pub(super) llvm_builder: inkwell::builder::Builder<'ctx>,
  pub(super) llvm_function_buffer: Option<inkwell::values::FunctionValue<'ctx>>,
  pub module_name: String,
  // REVIEW: Why do we need the cache here? If we retrieve nodes from it, remember that we've
  // ... outlined that option already because of design issues. Find a better way to handle lowering
  // ... of things like what a reference refers to.
  cache: &'a symbol_table::SymbolTable,
  type_cache: &'a type_inference::TypeCache,
  llvm_context: &'ctx inkwell::context::Context,
  llvm_module: &'a inkwell::module::Module<'ctx>,
  // TODO: Shouldn't this be a vector instead?
  llvm_cached_values:
    std::collections::HashMap<symbol_table::NodeId, Option<inkwell::values::BasicValueEnum<'ctx>>>,
  llvm_cached_types:
    std::collections::HashMap<symbol_table::NodeId, inkwell::types::BasicTypeEnum<'ctx>>,
  mangle_counter: usize,
  do_access: bool,
}

impl<'a, 'ctx> LoweringContext<'a, 'ctx> {
  pub fn new(
    type_cache: &'a type_inference::TypeCache,
    cache: &'a symbol_table::SymbolTable,
    llvm_context: &'ctx inkwell::context::Context,
    llvm_module: &'a inkwell::module::Module<'ctx>,
  ) -> Self {
    Self {
      cache,
      type_cache,
      // TODO: Proper name?
      module_name: "unnamed".to_string(),
      llvm_context,
      llvm_module,
      llvm_builder: llvm_context.create_builder(),
      llvm_function_buffer: None,
      llvm_cached_values: std::collections::HashMap::new(),
      llvm_cached_types: std::collections::HashMap::new(),
      current_loop_block: None,
      mangle_counter: 0,
      do_access: false,
    }
  }

  /// Lower a node with the `do_access` flag set to `true`, and apply
  /// access rules to it after, if it yielded an LLVM value.
  fn lower_with_access(
    &mut self,
    node: &ast::NodeKind,
  ) -> Option<inkwell::values::BasicValueEnum<'ctx>> {
    let llvm_value_result = self.lower_with_do_access_flag(node, true);

    if let Some(llvm_value) = llvm_value_result {
      // REVIEW: Why apply access rules here if we already specified the access flag?
      Some(self.apply_access_rules(node, llvm_value))
    } else {
      None
    }
  }

  /// Lower a node with the `do_access` flag set to `true`
  fn lower_with_do_access_flag(
    &mut self,
    node: &ast::NodeKind,
    do_access: bool,
  ) -> Option<inkwell::values::BasicValueEnum<'ctx>> {
    // REVIEW: Is the usage of a buffer required here? Will it interfere with anything
    // ... if not used?
    let do_access_buffer = self.do_access;

    self.do_access = do_access;

    let llvm_value_opt = self.dispatch(node);

    self.do_access = do_access_buffer;

    llvm_value_opt
  }

  fn apply_access_rules(
    &mut self,
    node: &ast::NodeKind,
    llvm_value: inkwell::values::BasicValueEnum<'ctx>,
  ) -> inkwell::values::BasicValueEnum<'ctx> {
    // REVISE: Need to remove the exclusive logic for let-statements / references (or simplify it).
    // REVIEW: Isn't there a need to resolve the type?

    // REVIEW: Isn't this superficial? Will this work as expected without type inference?
    if matches!(
      node.flatten(),
      ast::NodeKind::Parameter(_) | ast::NodeKind::Literal(ast::Literal::String(_))
    )
    // || node
    //   .infer_type(self.cache)
    //   .is(&ast::Type::Basic(ast::BasicType::String))
    {
      return llvm_value;
    }

    if let ast::NodeKind::Reference(reference) = &node {
      let target = self
        .cache
        .find_decl_via_link(&reference.pattern.link_id)
        .unwrap();

      // TODO: Ensure this recursive nature doesn't cause stack overflow.
      return self.apply_access_rules(target, llvm_value);
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
    if llvm_value.is_pointer_value() && !Self::is_callable(llvm_value) {
      self.access(llvm_value.into_pointer_value())
    } else {
      llvm_value
    }
  }

  fn find_type_id(&self, ty: &ast::Type) -> Option<symbol_table::NodeId> {
    Some(match ty.flatten(self.cache) {
      ast::Type::Struct(struct_type) => struct_type.id.clone(),
      // REVIEW: Any more?
      _ => return None,
    })
  }

  // REVIEW: Ensure that this function is tail-recursive.
  fn lower_type(&mut self, ty: &ast::Type) -> inkwell::types::BasicTypeEnum<'ctx> {
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
      ast::Type::StaticIndexable(element_type, size) => self
        .lower_type(&element_type)
        .array_type(*size)
        .as_basic_type_enum(),
      ast::Type::Pointer(pointee_type) => self
        .lower_type(&pointee_type)
        .ptr_type(inkwell::AddressSpace::Generic)
        .as_basic_type_enum(),
      // REVIEW: Is this redundant (because of `StubType`)?
      ast::Type::Struct(struct_type) => {
        let llvm_field_types = struct_type
          .fields
          .iter()
          .map(|field| self.lower_type(&field.1))
          .collect::<Vec<_>>();

        let name = self.mangle_name(&format!("struct.{}", struct_type.name));
        let llvm_struct_type = self.llvm_context.opaque_struct_type(name.as_str());

        llvm_struct_type.set_body(llvm_field_types.as_slice(), false);

        llvm_struct_type.as_basic_type_enum()
      }
      // REVIEW: Why not resolve the type if it is a stub type, then proceed to lower it?
      ast::Type::Stub(stub_type) => {
        self.memoize_or_retrieve_type_by_binding(stub_type.pattern.link_id)
      }
      ast::Type::Signature(callable_type) => self
        .lower_callable_type(callable_type)
        .ptr_type(inkwell::AddressSpace::Generic)
        .as_basic_type_enum(),
      // TODO: Implement.
      ast::Type::Reference(_reference_type) => todo!(),
      // TODO: Implement.
      ast::Type::This(this_type) => {
        self.memoize_or_retrieve_type_by_binding(this_type.target_id.unwrap())
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
      | ast::Type::AnyInteger
      | ast::Type::Never
      | ast::Type::Any
      | ast::Type::Variable(_)
      | ast::Type::Constructor(..) => unreachable!(),
    }
  }

  fn lower_callable_type(
    &mut self,
    signature_type: &ast::SignatureType,
  ) -> inkwell::types::FunctionType<'ctx> {
    let llvm_parameter_types = signature_type
      .parameter_types
      .iter()
      .map(|parameter_type| self.lower_type(&parameter_type).into())
      .collect::<Vec<_>>();

    let llvm_return_type = self.lower_type(&signature_type.return_type);

    llvm_return_type.fn_type(llvm_parameter_types.as_slice(), signature_type.is_variadic)
  }

  /// Returns a new LLVM function type based on the given signature.
  ///
  /// The return value is required because the signature's return type is
  /// merely an annotation, and is unreliable. Assumes the return type to be
  /// flat.
  fn lower_signature(
    &mut self,
    signature: &ast::Signature,
    return_type: &ast::Type,
  ) -> inkwell::types::FunctionType<'ctx> {
    let mut llvm_parameter_types = signature
      .parameters
      .iter()
      .map(|parameter| {
        self
          .memoize_or_retrieve_type(self.type_cache.get(&parameter.id).unwrap())
          .into()
      })
      .collect::<Vec<_>>();

    if signature.accepts_instance {
      let llvm_instance_type =
        self.memoize_or_retrieve_type_by_binding(signature.instance_type_id.unwrap());

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
        .memoize_or_retrieve_type(return_type)
        .fn_type(llvm_parameter_types.as_slice(), signature.is_variadic)
        .ptr_type(inkwell::AddressSpace::Generic)
        .into()
    } else {
      self
        .llvm_context
        .void_type()
        .fn_type(llvm_parameter_types.as_slice(), signature.is_variadic)
        .ptr_type(inkwell::AddressSpace::Generic)
        .as_basic_type_enum()
    }
    .into_pointer_type()
    .get_element_type()
    .into_function_type()
  }

  // TODO: Ensure that there isn't any possible recursion problems going on.
  fn memoize_or_retrieve_type(&mut self, ty: &ast::Type) -> inkwell::types::BasicTypeEnum<'ctx> {
    if let Some(id) = self.find_type_id(ty) {
      // REVIEW: Isn't this indirectly recursive? Will it cause problems?
      return self.memoize_or_retrieve_type_by_binding(id);
    }

    self.lower_type(ty)
  }

  fn memoize_or_retrieve_type_by_binding(
    &mut self,
    id: symbol_table::NodeId,
  ) -> inkwell::types::BasicTypeEnum<'ctx> {
    if let Some(existing_definition) = self.llvm_cached_types.get(&id) {
      return existing_definition.clone();
    }

    // REVIEW: Consider making a separate map for types in the cache.

    let declaration = self.cache.find_decl_via_link(&id).unwrap();

    // REVIEW: Why not perform type-flattening here instead?
    let ty = match &declaration {
      ast::NodeKind::Struct(struct_type) => ast::Type::Struct(struct_type.as_ref().clone()),
      ast::NodeKind::TypeAlias(type_alias) => type_alias.ty.clone(),
      // REVIEW: Any more?
      _ => unreachable!(),
    };

    let llvm_type = self.lower_type(&ty);

    self.llvm_cached_types.insert(id, llvm_type);

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
  /// Attempt to retrieve an existing declaration, otherwise proceed to
  /// lower and memoize it under the current module.
  ///
  /// If specified, any modified buffers during the process will be kept,
  /// otherwise they will all be restored.
  fn memoize_declaration(
    &mut self,
    node_id: &symbol_table::NodeId,
    forward_buffers: bool,
    apply_access_rules: bool,
  ) -> Option<inkwell::values::BasicValueEnum<'ctx>> {
    let declaration = self.cache.declarations.get(node_id).unwrap();

    // BUG: If a definition is lowered elsewhere without the use of this function,
    // ... there will not be any call to `lower_with_access_rules` for it. This means
    // ... that those definitions will be cached as `PointerValue`s instead of possibly
    // ... needing to be lowered.
    // If the definition has been cached previously, simply retrieve it.
    if let Some(cached_value_opt) = self.llvm_cached_values.get(node_id) {
      // NOTE: The underlying LLVM value is not copied, but rather the reference to it.
      let cached_value_cloned = cached_value_opt.clone();

      return cached_value_cloned.and_then(|cached_value| {
        if apply_access_rules {
          Some(self.apply_access_rules(declaration, cached_value))
        } else {
          Some(cached_value)
        }
      });
    }

    let buffers = self.copy_buffers();
    let llvm_value_opt = self.dispatch(declaration);

    // Cache the value without applying access rules.
    // This way, access rules may be chosen to be applied upon cache retrieval.
    self.llvm_cached_values.insert(*node_id, llvm_value_opt);

    // REVIEW: This was causing a forced-access bug. But what if the declaration is a pointer
    // ... value and the retrieval wanted to apply access rules? Should it be done by the caller manually instead?
    // Some(if self.do_access {
    //   self.apply_access_rules(declaration, llvm_value)
    // } else {
    //   llvm_value
    // })

    // Restore buffers after processing, if requested.
    if !forward_buffers {
      self.restore_buffers(buffers);
    }

    llvm_value_opt
  }
}

impl<'a, 'ctx> visitor::LoweringVisitor<'ctx> for LoweringContext<'a, 'ctx> {
  fn visit_call_expr(
    &mut self,
    call_expr: &ast::CallExpr,
  ) -> Option<inkwell::values::BasicValueEnum<'ctx>> {
    let mut llvm_arguments = call_expr
      .arguments
      .iter()
      .map(|node| self.lower_with_access(node).unwrap().into())
      .collect::<Vec<_>>();

    // Insert the instance pointer as the first argument, if applicable.
    if let ast::NodeKind::MemberAccess(member_access) = call_expr.callee_expr.as_ref() {
      // FIXME: This will panic for zero-length vectors. Find another way to prepend elements.
      llvm_arguments.insert(0, self.dispatch(&member_access.base_expr).unwrap().into());
    }

    // BUG: It seems that this is causing stack-overflow because results aren't cached? What's going on? Or maybe it's the parser?
    // REVIEW: Here we opted not to forward buffers. Ensure this is correct.
    // let llvm_target_callable = call_expr
    //   .callee_expr
    //   .lower(generator, false)
    //   .unwrap()
    //   .into_pointer_value();
    let llvm_target_callable = self
      .dispatch(&call_expr.callee_expr)
      .unwrap()
      .into_pointer_value();

    let llvm_call_value = self.llvm_builder.build_call(
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

  fn visit_binding_stmt(
    &mut self,
    binding_stmt: &ast::BindingStmt,
  ) -> Option<inkwell::values::BasicValueEnum<'ctx>> {
    // REVIEW: Should we be lowering const expressions as global constants? What benefits does that provide? What about scoping?

    // REVIEW: Always retrieve from the type cache regardless of type hint?
    // REVIEW: Temporarily chose to try given type-hint before retrieving from the type cache.
    // ... This is to allow unit tests to properly execute, but it seems like the right way to go as well.
    let value_type = binding_stmt
      .type_hint
      .as_ref()
      .unwrap_or_else(|| self.type_cache.get(&binding_stmt.id).unwrap());

    // Special case. The allocation is done elsewhere.
    if matches!(value_type, ast::Type::Signature(_)) {
      // REVISE: Cleanup the caching code.
      // REVIEW: Here create a definition for the closure, with the let statement as the name?

      let result = self.dispatch(&binding_stmt.value);

      // REVIEW: Won't let-statements always have a value?
      if let Some(llvm_value) = result {
        self
          .llvm_cached_values
          .insert(binding_stmt.id, Some(llvm_value));
      }

      return result;
    }

    let llvm_value_result = self.lower_with_do_access_flag(&binding_stmt.value, true);

    // FIXME: What about for other things that may be in the same situation (their values are unit)?
    // Do not proceed if the value will never evaluate.
    if value_type.is_a_meta() {
      // REVIEW: Returning `None` here but below we return `Some()`.
      // ... What expects a value out of this, and would this decision affect that?
      return None;
    }

    let llvm_value = llvm_value_result.unwrap();
    let llvm_type = self.memoize_or_retrieve_type(&value_type);

    let llvm_alloca = self
      .llvm_builder
      .build_alloca(llvm_type, format!("var.{}", binding_stmt.name).as_str());

    self.llvm_builder.build_store(llvm_alloca, llvm_value);

    let result = llvm_alloca.as_basic_value_enum();

    self
      .llvm_cached_values
      .insert(binding_stmt.id, Some(result));

    // FIXME: Temporary. In the future this shouldn't yield a value? Review.
    Some(result)
  }

  fn visit_unary_expr(
    &mut self,
    unary_expr: &ast::UnaryExpr,
  ) -> Option<inkwell::values::BasicValueEnum<'ctx>> {
    Some(match unary_expr.operator {
      ast::OperatorKind::Not => {
        let llvm_value = self.dispatch(&unary_expr.operand).unwrap();
        let llvm_final_value = self.attempt_access(llvm_value);

        // NOTE: We expect the value to be a boolean. This should be enforced during type-checking.
        self
          .llvm_builder
          .build_not(llvm_final_value.into_int_value(), "not_op")
          .as_basic_value_enum()
      }
      ast::OperatorKind::SubtractOrNegate => {
        let llvm_value = self.dispatch(&unary_expr.operand).unwrap();
        let llvm_final_value = self.attempt_access(llvm_value);

        // NOTE: We expect the value to be an integer or float. This should be enforced during type-checking.
        if llvm_value.is_int_value() {
          self
            .llvm_builder
            .build_int_neg(llvm_final_value.into_int_value(), "int.negate_op")
            .as_basic_value_enum()
        } else {
          self
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
        // unary_expr.expr.lower(generator, cache, false).unwrap()
        self.dispatch(&unary_expr.operand).unwrap()

        // TODO: Continue implementation (if necessary).
        // todo!()
      }
      ast::OperatorKind::MultiplyOrDereference => {
        let llvm_value = self.dispatch(&unary_expr.operand).unwrap();

        // BUG: If the value is a reference to a let-statement, the pointer of
        // ... the let-statement will be removed, but the actual pointer value will
        // ... not be accessed. Perhaps will need to remove special treatment of let-statements.
        self.access(llvm_value.into_pointer_value())
      }
      ast::OperatorKind::Cast => {
        let llvm_value = self.dispatch(&unary_expr.operand).unwrap();
        let llvm_final_value = self.attempt_access(llvm_value);

        let llvm_to_type = self.memoize_or_retrieve_type(unary_expr.cast_type.as_ref().unwrap());

        if !llvm_value.is_int_value() {
          // TODO: Implement for other cases.
          todo!();
        }

        self.llvm_builder.build_cast(
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

  fn visit_return_stmt(
    &mut self,
    return_stmt: &ast::ReturnStmt,
  ) -> Option<inkwell::values::BasicValueEnum<'ctx>> {
    let llvm_return_value = if let Some(return_value) = &return_stmt.value {
      Some(self.lower_with_do_access_flag(&return_value, true).unwrap())
    } else {
      None
    };

    self.attempt_build_return(llvm_return_value);

    None
  }

  fn enter_block_expr(
    &mut self,
    block: &ast::BlockExpr,
  ) -> Option<inkwell::values::BasicValueEnum<'ctx>> {
    for statement in &block.statements {
      // FIXME: Some binding statements (such as let-statement) need to be manually
      // ... cached in the generator, this is because not all calls to lower it are made
      // ... using the `memoize_or_retrieve_value` helper function (such as this one!).
      // statement.lower(generator, cache, false);
      self.dispatch(statement);

      // Do not continue lowering statements if the current block was terminated.
      if self.get_current_block().get_terminator().is_some() {
        break;
      }
    }

    // REVIEW: This syntax may replace if/else expressions? Use `and_then`?
    // self.yields.as_ref().map(|value| {
    //   generator
    //     .lower_with_access_rules(&value, cache)
    //     // TODO: Why doesn't the one below unwrap?
    //     .unwrap()
    // })

    if let Some(yields_value) = &block.yields {
      self.lower_with_access(&yields_value)
    } else {
      None
    }
  }

  fn visit_extern_function(
    &mut self,
    extern_fn: &ast::ExternFunction,
  ) -> Option<inkwell::values::BasicValueEnum<'ctx>> {
    // NOTE: The return type is always explicitly-given for extern functions.
    let llvm_signature_type = self.lower_signature(
      &extern_fn.signature,
      extern_fn
        .signature
        .return_type_hint
        .as_ref()
        .map_or(&ast::Type::Unit, |return_type| return_type),
    );

    // TODO: Need to handle if the function already exists.

    let llvm_external_function = self.llvm_module.add_function(
      extern_fn.name.as_str(),
      llvm_signature_type,
      Some(inkwell::module::Linkage::External),
    );

    Some(
      llvm_external_function
        .as_global_value()
        .as_basic_value_enum(),
    )
  }

  fn enter_function(
    &mut self,
    function: &ast::Function,
  ) -> Option<inkwell::values::BasicValueEnum<'ctx>> {
    // BUG: If we do choose to lower declarations such as functions on-demand (i.e. a reference
    // ... to a function yet to be lowered), buffers would interfere with the lowering of such
    // ... function, so we would need to stash buffers and pop them when entering and exiting
    // ... functions.

    // let return_type = TypeContext::infer_return_value_type(&function.body, self.cache);
    let return_type = self
      .type_cache
      .get(&function.signature.return_type_id)
      .unwrap();

    let llvm_signature_type = self.lower_signature(&function.signature, &return_type);
    let is_main = function.name == MAIN_FUNCTION_NAME;

    // TODO: Prepend `fn` to the name.
    let llvm_function_name = if is_main {
      // TODO: Name being cloned. Is this okay?
      function.name.to_owned()
    } else {
      self.mangle_name(&function.name)
    };

    assert!(self
      .llvm_module
      .get_function(llvm_function_name.as_str())
      .is_none());

    let llvm_function = self.llvm_module.add_function(
      llvm_function_name.as_str(),
      llvm_signature_type,
      Some(if is_main {
        inkwell::module::Linkage::External
      } else {
        inkwell::module::Linkage::Private
      }),
    );

    self.llvm_function_buffer = Some(llvm_function);

    // FIXME: Still getting stack-overflow errors when using recursive functions (specially multiple of them at the same time). Investigate whether that's caused here or elsewhere.
    // Manually cache the function now to allow for recursive function calls.
    self.llvm_cached_values.insert(
      function.id,
      Some(llvm_function.as_global_value().as_basic_value_enum()),
    );

    // REVIEW: Is this conversion safe?
    let expected_param_count = function.signature.parameters.len() as u32;

    assert_eq!(
      llvm_function.count_params(),
      if function.signature.accepts_instance {
        expected_param_count + 1
      } else {
        expected_param_count
      }
    );

    // REVISE: The parameter counts aren't always guaranteed to be the same, given
    // ... if the signature accepts an instance. This zip might cause unexpected problems.
    llvm_function
      .get_param_iter()
      .zip(function.signature.parameters.iter())
      .for_each(|params| {
        params
          .0
          .set_name(format!("param.{}", params.1.name).as_str());
      });

    let llvm_entry_block = self
      .llvm_context
      .append_basic_block(llvm_function, "fn.entry");

    self.llvm_builder.position_at_end(llvm_entry_block);

    let yielded_result = self.enter_block_expr(&function.body);

    // FIXME: Abstract this logic for use within `closure`, and possibly wherever else this is needed, guided by calls to `attempt_build_return`?
    // If a block was left for further processing, and it has no terminator,
    // complete it here.
    self.attempt_build_return(yielded_result);

    self.llvm_function_buffer = None;

    Some(llvm_function.as_global_value().as_basic_value_enum())
  }

  fn visit_literal(
    &mut self,
    literal: &ast::Literal,
  ) -> Option<inkwell::values::BasicValueEnum<'ctx>> {
    Some(match literal {
      ast::Literal::Int(value, integer_kind) => {
        let llvm_int_type = self.llvm_context.custom_width_int_type(match integer_kind {
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
      ast::Literal::Char(value) => self
        .llvm_context
        .i8_type()
        .const_int(value.clone() as u64, false)
        .as_basic_value_enum(),
      ast::Literal::Bool(value) => self
        .llvm_context
        .bool_type()
        .const_int(value.clone() as u64, false)
        .as_basic_value_enum(),
      ast::Literal::String(value) => self
        .llvm_builder
        .build_global_string_ptr(value.as_str(), "string_literal")
        .as_basic_value_enum(),
      ast::Literal::Nullptr(_, ty) => self
        // FIXME: In the future, nullptr literal will no longer have a type attached to it.
        // ... Instead, use the type cache to retrieve it's type.
        .lower_type(ty.as_ref().unwrap())
        .ptr_type(inkwell::AddressSpace::Generic)
        .const_null()
        .as_basic_value_enum(),
    })
  }

  fn visit_if_expr(
    &mut self,
    if_expr: &ast::IfExpr,
  ) -> Option<inkwell::values::BasicValueEnum<'ctx>> {
    // TODO: Process alternative branches.

    let llvm_condition = self.dispatch(&if_expr.condition).unwrap();
    let llvm_current_function = self.llvm_function_buffer.unwrap();
    let mut llvm_if_value = None;

    let ty = self
      .type_cache
      .get(&if_expr.id)
      .unwrap()
      .flatten(self.cache);

    // This if-expression will never yield a value if its type
    // is unit or never.
    let yields_expression = !ty.is_a_meta();

    // Allocate the resulting if-value early on, if applicable.
    if yields_expression {
      let llvm_if_value_type = self.memoize_or_retrieve_type(&ty);

      let llvm_if_value_alloca = self
        .llvm_builder
        .build_alloca(llvm_if_value_type, "if.value");

      llvm_if_value = Some(llvm_if_value_alloca.as_basic_value_enum());
    }

    let llvm_then_block = self
      .llvm_context
      .append_basic_block(llvm_current_function, "if.then");

    // FIXME: Only add the `after` block if the `then` block doesn't terminate.
    let llvm_after_block = self
      .llvm_context
      .append_basic_block(llvm_current_function, "if.after");

    let mut llvm_else_block_result = None;
    let mut llvm_else_block_value = None;

    // TODO: Simplify (use a buffer for the next block onto build the cond. br. to).
    // Lower the else branch if applicable.
    if let Some(else_branch) = &if_expr.else_branch {
      let llvm_else_block = self
        .llvm_context
        .append_basic_block(llvm_current_function, "if.else");

      llvm_else_block_result = Some(llvm_else_block);

      self.llvm_builder.build_conditional_branch(
        llvm_condition.into_int_value(),
        llvm_then_block,
        llvm_else_block,
      );

      self.llvm_builder.position_at_end(llvm_else_block);
      llvm_else_block_value = self.dispatch(else_branch);

      // FIXME: Is this correct? Or should we be using the `else_block` directly here?
      // Fallthrough if applicable.
      if self.get_current_block().get_terminator().is_none() {
        self
          .llvm_builder
          .build_unconditional_branch(llvm_after_block);
      }
    } else {
      // NOTE: At this point, the condition must have been verified
      // to be a boolean by the type-checker.
      self.llvm_builder.build_conditional_branch(
        llvm_condition.into_int_value(),
        llvm_then_block,
        llvm_after_block,
      );
    }

    self.llvm_builder.position_at_end(llvm_then_block);

    let llvm_then_block_opt = self.dispatch(&if_expr.then_branch);

    // FIXME: Is this correct? Or should we be using `get_current_block()` here? Or maybe this is just a special case to not leave the `then` block without a terminator? Investigate.
    // Fallthrough if applicable.
    if self.get_current_block().get_terminator().is_none() {
      self
        .llvm_builder
        .build_unconditional_branch(llvm_after_block);
    }

    if yields_expression {
      // TODO: Is it guaranteed to have a first instruction? Think (at this point both block return a value, correct?).
      self
        .llvm_builder
        .position_before(&llvm_then_block.get_last_instruction().unwrap());

      self.llvm_builder.build_store(
        llvm_if_value.unwrap().into_pointer_value(),
        // BUG: There is a bug with the `type.ko` integration test.
        llvm_then_block_opt.unwrap(),
      );

      // TODO: Is it guaranteed to have a first instruction? Think (at this point both block return a value, correct?).
      self.llvm_builder.position_before(
        &llvm_else_block_result
          .unwrap()
          .get_last_instruction()
          .unwrap(),
      );

      self.llvm_builder.build_store(
        llvm_if_value.unwrap().into_pointer_value(),
        llvm_else_block_value.unwrap(),
      );
    }

    // Leave the after block as current for further processing.
    self.llvm_builder.position_at_end(llvm_after_block);

    // If an expression is to be yielded, it must be accessed. A pointer
    // shouldn't be yielded.
    if let Some(llvm_if_value) = llvm_if_value {
      Some(self.access(llvm_if_value.into_pointer_value()))
    } else {
      None
    }
  }

  fn visit_reference(
    &mut self,
    reference: &ast::Reference,
  ) -> Option<inkwell::values::BasicValueEnum<'ctx>> {
    // REVIEW: This may not be working, because the `memoize_declaration` function directly lowers, regardless of expected access or not.
    let llvm_target = self
      // REVIEW: Here we opted not to forward buffers. Ensure this is correct.
      .memoize_declaration(
        &self.cache.links.get(&reference.pattern.link_id).unwrap(),
        false,
        self.do_access,
      )
      .unwrap();

    Some(llvm_target)
  }

  fn visit_binary_expr(
    &mut self,
    binary_expr: &ast::BinaryExpr,
  ) -> Option<inkwell::values::BasicValueEnum<'ctx>> {
    let mut llvm_left_value = self.dispatch(&binary_expr.left_operand).unwrap();
    let mut llvm_right_value = self.dispatch(&binary_expr.right_operand).unwrap();

    // REVIEW: Is it okay to semi-force an access here? Why not instead make use of the `access` parameter?
    // ... Maybe the operands should always be attempted to be accessed? What about strings? Will they ever be a
    // ... binary expression's operand?
    llvm_left_value = self.attempt_access(llvm_left_value);
    llvm_right_value = self.attempt_access(llvm_right_value);

    // NOTE: By this point, we assume that both values are of the same type.
    let is_int_values = llvm_left_value.is_int_value();

    // TODO: Make use of.
    // let is_signed = llvm_left_value.into_int_value().get_sign_extended_constant();

    let llvm_operation = match binary_expr.operator {
      ast::OperatorKind::Add if is_int_values => self
        .llvm_builder
        .build_int_add(
          llvm_left_value.into_int_value(),
          llvm_right_value.into_int_value(),
          "int.add_op",
        )
        .as_basic_value_enum(),
      ast::OperatorKind::Add => self
        .llvm_builder
        .build_float_add(
          llvm_left_value.into_float_value(),
          llvm_right_value.into_float_value(),
          "float.add_op",
        )
        .as_basic_value_enum(),
      ast::OperatorKind::SubtractOrNegate if is_int_values => self
        .llvm_builder
        .build_int_sub(
          llvm_left_value.into_int_value(),
          llvm_right_value.into_int_value(),
          "int.subtract_op",
        )
        .as_basic_value_enum(),
      ast::OperatorKind::SubtractOrNegate => self
        .llvm_builder
        .build_float_sub(
          llvm_left_value.into_float_value(),
          llvm_right_value.into_float_value(),
          "float.subtract_op",
        )
        .as_basic_value_enum(),
      ast::OperatorKind::MultiplyOrDereference if is_int_values => self
        .llvm_builder
        .build_int_mul(
          llvm_left_value.into_int_value(),
          llvm_right_value.into_int_value(),
          "int.multiply_op",
        )
        .as_basic_value_enum(),
      ast::OperatorKind::MultiplyOrDereference => self
        .llvm_builder
        .build_float_mul(
          llvm_left_value.into_float_value(),
          llvm_right_value.into_float_value(),
          "float.multiply_op",
        )
        .as_basic_value_enum(),
      // BUG: Need to implement static checks for division by zero.
      // TODO: Support for unsgined division?
      ast::OperatorKind::Divide if is_int_values => self
        .llvm_builder
        .build_int_signed_div(
          llvm_left_value.into_int_value(),
          llvm_right_value.into_int_value(),
          "int.divide_op",
        )
        .as_basic_value_enum(),
      ast::OperatorKind::Divide => self
        .llvm_builder
        .build_float_div(
          llvm_left_value.into_float_value(),
          llvm_right_value.into_float_value(),
          "float.divide_op",
        )
        .as_basic_value_enum(),
      ast::OperatorKind::LessThan if is_int_values => self
        .llvm_builder
        .build_int_compare(
          // TODO: Support for unsigned?
          inkwell::IntPredicate::SLT,
          llvm_left_value.into_int_value(),
          llvm_right_value.into_int_value(),
          "int.slt_op",
        )
        .as_basic_value_enum(),
      ast::OperatorKind::LessThan => self
        .llvm_builder
        .build_float_compare(
          inkwell::FloatPredicate::OLT,
          llvm_left_value.into_float_value(),
          llvm_right_value.into_float_value(),
          "float.slt_op",
        )
        .as_basic_value_enum(),
      ast::OperatorKind::GreaterThan if is_int_values => self
        .llvm_builder
        .build_int_compare(
          // TODO: Support for unsigned?
          inkwell::IntPredicate::SGT,
          llvm_left_value.into_int_value(),
          llvm_right_value.into_int_value(),
          "int.sgt_op",
        )
        .as_basic_value_enum(),
      ast::OperatorKind::GreaterThan => self
        .llvm_builder
        .build_float_compare(
          inkwell::FloatPredicate::OGT,
          llvm_left_value.into_float_value(),
          llvm_right_value.into_float_value(),
          "float.gt_op",
        )
        .as_basic_value_enum(),
      ast::OperatorKind::LessThanOrEqual if is_int_values => self
        .llvm_builder
        .build_int_compare(
          inkwell::IntPredicate::SLE,
          llvm_left_value.into_int_value(),
          llvm_right_value.into_int_value(),
          "int.sltoe_op",
        )
        .as_basic_value_enum(),
      ast::OperatorKind::LessThanOrEqual => self
        .llvm_builder
        .build_float_compare(
          inkwell::FloatPredicate::OLE,
          llvm_left_value.into_float_value(),
          llvm_right_value.into_float_value(),
          "float.ltoe_op",
        )
        .as_basic_value_enum(),
      ast::OperatorKind::GreaterThanOrEqual if is_int_values => self
        .llvm_builder
        .build_int_compare(
          inkwell::IntPredicate::SGE,
          llvm_left_value.into_int_value(),
          llvm_right_value.into_int_value(),
          "int.sgtoe_op",
        )
        .as_basic_value_enum(),
      ast::OperatorKind::GreaterThanOrEqual => self
        .llvm_builder
        .build_float_compare(
          inkwell::FloatPredicate::OGE,
          llvm_left_value.into_float_value(),
          llvm_right_value.into_float_value(),
          "float.gtoe_op",
        )
        .as_basic_value_enum(),
      ast::OperatorKind::Equality if is_int_values => self
        .llvm_builder
        .build_int_compare(
          inkwell::IntPredicate::EQ,
          llvm_left_value.into_int_value(),
          llvm_right_value.into_int_value(),
          "int.eq_op",
        )
        .as_basic_value_enum(),
      ast::OperatorKind::Equality => self
        .llvm_builder
        .build_float_compare(
          inkwell::FloatPredicate::OEQ,
          llvm_left_value.into_float_value(),
          llvm_right_value.into_float_value(),
          "float.eq_op",
        )
        .as_basic_value_enum(),
      ast::OperatorKind::And => self
        .llvm_builder
        .build_and(
          llvm_left_value.into_int_value(),
          llvm_right_value.into_int_value(),
          "and_op",
        )
        .as_basic_value_enum(),
      ast::OperatorKind::Or => self
        .llvm_builder
        .build_or(
          llvm_left_value.into_int_value(),
          llvm_right_value.into_int_value(),
          "or_op",
        )
        .as_basic_value_enum(),
      ast::OperatorKind::Nand => self
        .llvm_builder
        .build_not(
          self.llvm_builder.build_and(
            llvm_left_value.into_int_value(),
            llvm_right_value.into_int_value(),
            "",
          ),
          "nand_op",
        )
        .as_basic_value_enum(),
      ast::OperatorKind::Nor => self
        .llvm_builder
        .build_not(
          self.llvm_builder.build_or(
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

  fn enter_unsafe_expr(
    &mut self,
    unsafe_expr: &ast::UnsafeExpr,
  ) -> Option<inkwell::values::BasicValueEnum<'ctx>> {
    self.dispatch(&unsafe_expr.0)
  }

  fn visit_parameter(
    &mut self,
    parameter: &ast::Parameter,
  ) -> Option<inkwell::values::BasicValueEnum<'ctx>> {
    // TODO: In the future, consider having an `alloca` per parameter, for simpler tracking of them?

    Some(
      self
        .llvm_function_buffer
        .unwrap()
        .get_nth_param(parameter.position)
        .unwrap(),
    )
  }

  fn visit_array(&mut self, array: &ast::Array) -> Option<inkwell::values::BasicValueEnum<'ctx>> {
    let llvm_values = array
      .elements
      .iter()
      .map(|element| self.dispatch(element).unwrap())
      .collect::<Vec<_>>();

    let array_type = self.type_cache.get(&array.id).unwrap();
    let llvm_array_type = self.memoize_or_retrieve_type(array_type);

    let llvm_array_ptr = self
      .llvm_builder
      .build_alloca(llvm_array_type, "array.value");

    for (index, llvm_value) in llvm_values.iter().enumerate() {
      let first_index = self.llvm_context.i32_type().const_int(0, false);

      let llvm_index = self
        .llvm_context
        .i32_type()
        // REVIEW: Is this conversion safe?
        .const_int(index as u64, false);

      // FIXME: There is no bounds checking guard being inserted (panic).
      unsafe {
        let llvm_gep =
          self
            .llvm_builder
            .build_gep(llvm_array_ptr, &[first_index, llvm_index], "array.init");

        self.llvm_builder.build_store(llvm_gep, llvm_value.clone());
      }
    }

    // REVIEW: Might not need to access the array in order to initialize it, but to return it?
    // BUG: There seems to be a bug regarding this, and indexing arrays. "Expected PointerValue variant".
    // ... See commented code for possible* made up solution?
    // let llvm_array_value = self
    //   .access(llvm_array_ptr)
    //   .into_array_value()
    //   .as_basic_value_enum();

    Some(if self.do_access {
      self
        .access(llvm_array_ptr)
        .into_array_value()
        .as_basic_value_enum()
    } else {
      llvm_array_ptr.as_basic_value_enum()
    })

    // Some(llvm_array_value)
  }

  fn visit_indexing_expr(
    &mut self,
    indexing_expr: &ast::IndexingExpr,
  ) -> Option<inkwell::values::BasicValueEnum<'ctx>> {
    // TODO: Consider adding support for indexing strings (or better yet, generalized indexing implementation).

    let llvm_index = self
      .dispatch(&indexing_expr.index_expr)
      .unwrap()
      .into_int_value();

    // FIXME: Might need an abstraction to specify to NOT lower with
    // ... / disregard access flag? This is here for debugging:
    // self.do_access = false;

    // REVIEW: Opted not to use access rules. Ensure this is correct.
    let llvm_target = self
      .lower_with_do_access_flag(&indexing_expr.target_expr, false)
      .unwrap();

    // TODO: Need a way to handle possible segfaults (due to an index being out-of-bounds).
    unsafe {
      // REVIEW: Figure out why there's a zero index (may want to look on `https://www.llvm.org/docs/GetElementPtr.html#why-is-the-extra-0-index-required`).
      let first_index = self.llvm_context.i32_type().const_int(0, false);

      let llvm_gep_ptr = self.llvm_builder.build_in_bounds_gep(
        // BUG: Access bug?
        llvm_target.into_pointer_value(),
        &[first_index, llvm_index],
        "array.index.gep",
      );

      // FIXME: Need to verify proper static+dynamic indexing during type checking.

      // REVIEW: Should we actually be de-referencing the pointer here?
      Some(self.access(llvm_gep_ptr))
    }
  }

  fn visit_enum(&mut self, enum_: &ast::Enum) -> Option<inkwell::values::BasicValueEnum<'ctx>> {
    for (index, variant) in enum_.variants.iter().enumerate() {
      let llvm_name = self.mangle_name(&format!("enum.{}.{}", enum_.name, variant.0));

      let llvm_variant_global = self.llvm_module.add_global(
        self.llvm_context.i32_type(),
        Some(inkwell::AddressSpace::Const),
        llvm_name.as_str(),
      );

      llvm_variant_global
        .set_initializer(&self.llvm_context.i32_type().const_int(index as u64, false));
    }

    None
  }

  fn visit_struct_value(
    &mut self,
    struct_value: &ast::StructValue,
  ) -> Option<inkwell::values::BasicValueEnum<'ctx>> {
    let llvm_struct_type = self
      .memoize_or_retrieve_type_by_binding(struct_value.target_id)
      .into_struct_type();

    let llvm_struct_alloca = self.llvm_builder.build_alloca(
      llvm_struct_type,
      format!("struct.{}.alloca", struct_value.struct_name).as_str(),
    );

    for (index, field) in struct_value.fields.iter().enumerate() {
      let struct_field_gep = self
        .llvm_builder
        // REVIEW: Is this conversion safe?
        .build_struct_gep(llvm_struct_alloca, index as u32, "struct.alloca.field.gep")
        .unwrap();

      let llvm_field_value = self.lower_with_do_access_flag(field, true).unwrap();

      self
        .llvm_builder
        // FIXME: For nested structs, they will return `None`.
        .build_store(struct_field_gep, llvm_field_value);
    }

    // TODO:
    // Some(if access {
    //   self.access(llvm_struct_alloca)
    // } else {
    //   llvm_struct_alloca.as_basic_value_enum()
    // })
    Some(llvm_struct_alloca.as_basic_value_enum())
  }

  fn visit_extern_static(
    &mut self,
    extern_static: &ast::ExternStatic,
  ) -> Option<inkwell::values::BasicValueEnum<'ctx>> {
    let llvm_global_value = self.llvm_module.add_global(
      self.memoize_or_retrieve_type(&extern_static.ty),
      None,
      extern_static.name.as_str(),
    );

    Some(llvm_global_value.as_basic_value_enum())
  }

  fn visit_intrinsic_call(
    &mut self,
    intrinsic_call: &ast::IntrinsicCall,
  ) -> Option<inkwell::values::BasicValueEnum<'ctx>> {
    // REVIEW: No need to use the `access` parameter?

    // TODO: Unused.
    let _llvm_arguments = intrinsic_call
      .arguments
      .iter()
      .map(|node| self.lower_with_do_access_flag(node, true).unwrap().into())
      .collect::<Vec<inkwell::values::BasicMetadataValueEnum<'ctx>>>();

    match intrinsic_call.kind {
      ast::IntrinsicKind::LengthOf => {
        let target_array = intrinsic_call.arguments.first().unwrap();

        let array_length = match target_array.infer_flatten_type(self.cache) {
          ast::Type::StaticIndexable(_, size) => size,
          _ => unreachable!(),
        };

        Some(
          self
            .llvm_context
            // REVIEW: Why not use `i32_type`?
            .i32_type()
            .const_int(array_length as u64, false)
            .as_basic_value_enum(),
        )
      }
    }
  }

  fn visit_closure(
    &mut self,
    closure: &ast::Closure,
  ) -> Option<inkwell::values::BasicValueEnum<'ctx>> {
    // REVIEW: Closures don't have a unique id.
    // ... Don't we need to set the buffer unique id for closures as well?

    let buffers = self.copy_buffers();
    // let mut modified_signature = self.signature.clone();

    for (_index, _capture) in closure.captures.iter().enumerate() {
      // let capture_node = cache.force_get(&capture.1.unwrap());
      // let capture_node_type = (&*capture_node).infer_type(cache);
      // let computed_parameter_index = self.signature.parameters.len() as usize + index;

      // REVIEW: Is the parameter position correct?
      // TODO: Re-implement, after parameters were made definitions.
      // modified_signature.parameters.push((
      //   format!("capture.{}", capture.0),
      //   capture_node_type,
      //   computed_parameter_index as u32,
      // ))
    }

    // FIXME: Use the modified signature.
    let llvm_signature_type = self.lower_signature(
      &closure.signature,
      // &TypeContext::infer_return_value_type(&closure.body, self.cache),
      // TODO: Implement with the addition of type inference.
      todo!(),
    );

    let llvm_function_name = self.mangle_name(&String::from("closure"));

    assert!(self
      .llvm_module
      .get_function(llvm_function_name.as_str())
      .is_none());

    let llvm_function = self.llvm_module.add_function(
      llvm_function_name.as_str(),
      llvm_signature_type,
      Some(inkwell::module::Linkage::Private),
    );

    // TODO: Use a zipper, along with a chain.
    // for (i, llvm_parameter) in llvm_function.get_param_iter().enumerate() {
    //   // TODO: Ensure safe access.
    //   // FIXME: The llvm function's parameter count is longer than that of the signatures. This is because of inserted captures. Fix this bug.
    //   let parameter = &self.signature.parameters[i];

    //   parameter.lower(generator, cache);
    //   llvm_parameter.set_name(format!("param.{}", parameter.0).as_str());
    // }

    self.llvm_function_buffer = Some(llvm_function);

    let llvm_entry_block = self
      .llvm_context
      .append_basic_block(llvm_function, "closure.entry");

    self.llvm_builder.position_at_end(llvm_entry_block);

    let yielded_result = self.enter_block_expr(&closure.body);

    self.attempt_build_return(yielded_result);

    // FIXME: Might be missing the same check for never type as function.

    let result = llvm_function.as_global_value().as_basic_value_enum();

    self.restore_buffers(buffers);

    Some(result)
  }

  fn visit_member_access(
    &mut self,
    member_access: &ast::MemberAccess,
  ) -> Option<inkwell::values::BasicValueEnum<'ctx>> {
    let llvm_struct = self
      .dispatch(&member_access.base_expr)
      .unwrap()
      .into_pointer_value();

    // Flatten the type in case it is a `ThisType`.
    let llvm_struct_type = crate::force_match!(
      member_access.base_expr.infer_flatten_type(self.cache),
      ast::Type::Struct
    );

    // TODO: Must disallow fields and methods with the same name on semantic check phase.
    // First, check if its a field.
    if let Some(field_index) = llvm_struct_type
      .fields
      .iter()
      .position(|field| field.0 == member_access.member_name)
    {
      let field_gep = self
        .llvm_builder
        // REVIEW: Is this conversion safe?
        .build_struct_gep(llvm_struct, field_index as u32, "struct.member.gep")
        .unwrap();

      // TODO: Review that this is correct. Previously we had hard-coded "true" condition.
      return Some(if self.do_access {
        self.access(field_gep).as_basic_value_enum()
      } else {
        field_gep.as_basic_value_enum()
      });
    }

    // REVIEW: Is it safe to use the binding id of an inferred struct type?
    // Otherwise, it must be a method.
    let impl_method_info = self
      .cache
      .struct_impls
      .get(&llvm_struct_type.id)
      .unwrap()
      .iter()
      .find(|x| x.1 == member_access.member_name)
      .unwrap();

    // REVIEW: Opted to not use access rules. Ensure this is correct.
    self.memoize_declaration(&impl_method_info.0, false, false)
  }

  fn enter_struct_impl(
    &mut self,
    struct_impl: &ast::StructImpl,
  ) -> Option<inkwell::values::BasicValueEnum<'ctx>> {
    for _static_method in &struct_impl.static_methods {
      // TODO: Lower static methods?
    }

    for method in &struct_impl.member_methods {
      self.enter_function(method);
    }

    None
  }

  fn visit_parentheses_expr(
    &mut self,
    parentheses_expr: &ast::ParenthesesExpr,
  ) -> Option<inkwell::values::BasicValueEnum<'ctx>> {
    self.dispatch(&parentheses_expr.0)
  }

  fn visit_sizeof_intrinsic(
    &mut self,
    sizeof_intrinsic: &ast::SizeofIntrinsic,
  ) -> Option<inkwell::values::BasicValueEnum<'ctx>> {
    let ty = self.memoize_or_retrieve_type(&sizeof_intrinsic.ty);

    Some(ty.size_of().unwrap().as_basic_value_enum())
  }

  fn visit_inline_expr_stmt(
    &mut self,
    inline_expr_stmt: &ast::InlineExprStmt,
  ) -> Option<inkwell::values::BasicValueEnum<'ctx>> {
    self.dispatch(&inline_expr_stmt.expr)
  }
}

#[cfg(test)]
mod tests {
  use super::*;
  use crate::mock::tests::{ComparableMock, Mock};

  #[test]
  fn proper_initial_values() {
    // TODO:
  }

  #[test]
  fn lower_binding_stmt_const_val() {
    let type_cache = type_inference::TypeCache::new();
    let cache = symbol_table::SymbolTable::new();
    let llvm_context = inkwell::context::Context::create();
    let llvm_module = llvm_context.create_module("test");
    let mut mock = Mock::new(&type_cache, &cache, &llvm_context, &llvm_module);

    let binding_stmt = mock.binding(
      "a",
      Mock::literal_int(),
      Some(ast::Type::Basic(ast::BasicType::Int(ast::IntSize::I32))),
    );

    mock
      .llvm_function()
      .lower(&binding_stmt)
      .compare_with_file("binding_stmt_const_val");
  }

  #[test]
  fn lower_binding_stmt_ref_val() {
    let type_cache = type_inference::TypeCache::new();
    let mut cache = symbol_table::SymbolTable::new();
    let llvm_context = inkwell::context::Context::create();
    let llvm_module = llvm_context.create_module("test");
    let a_id: symbol_table::NodeId = 0;

    let binding_stmt_rc_a = std::rc::Rc::new(ast::BindingStmt {
      name: "a".to_string(),
      value: Box::new(Mock::literal_int()),
      is_const_expr: false,
      id: a_id,
      type_hint: Some(Mock::int_type()),
    });

    cache
      .declarations
      .insert(a_id, ast::NodeKind::BindingStmt(binding_stmt_rc_a.clone()));

    cache.links.insert(a_id, a_id);

    let binding_stmt_a = ast::NodeKind::BindingStmt(binding_stmt_rc_a);

    let binding_stmt_b = ast::NodeKind::BindingStmt(std::rc::Rc::new(ast::BindingStmt {
      name: "b".to_string(),
      value: Box::new(ast::NodeKind::Reference(Mock::reference(a_id))),
      is_const_expr: false,
      id: a_id + 1,
      type_hint: Some(Mock::int_type()),
    }));

    Mock::new(&type_cache, &cache, &llvm_context, &llvm_module)
      .llvm_function()
      .lower(&binding_stmt_a)
      .lower(&binding_stmt_b)
      .compare_with_file("binding_stmt_ref_val");
  }

  #[test]
  fn lower_binding_stmt_nullptr_val() {
    let type_cache = type_inference::TypeCache::new();
    let cache = symbol_table::SymbolTable::new();
    let llvm_context = inkwell::context::Context::create();
    let llvm_module = llvm_context.create_module("test");
    let ty = ast::Type::Basic(ast::BasicType::Int(ast::IntSize::I32));

    let binding_stmt = ast::NodeKind::BindingStmt(std::rc::Rc::new(ast::BindingStmt {
      name: "a".to_string(),
      value: Box::new(ast::NodeKind::Literal(ast::Literal::Nullptr(
        1,
        Some(ty.clone()),
      ))),
      is_const_expr: false,
      id: 0,
      type_hint: Some(ast::Type::Pointer(Box::new(ty))),
    }));

    Mock::new(&type_cache, &cache, &llvm_context, &llvm_module)
      .llvm_function()
      .lower(&binding_stmt)
      .compare_with_file("binding_stmt_nullptr_val");
  }

  #[test]
  fn lower_binding_stmt_ptr_ref_val() {
    let type_cache = type_inference::TypeCache::new();
    let mut cache = symbol_table::SymbolTable::new();
    let llvm_context = inkwell::context::Context::create();
    let llvm_module = llvm_context.create_module("test");
    let ty = ast::Type::Basic(ast::BasicType::Int(ast::IntSize::I32));
    let pointer_type = ast::Type::Pointer(Box::new(ty.clone()));
    let a_id: symbol_table::NodeId = 0;

    let binding_stmt_rc_a = std::rc::Rc::new(ast::BindingStmt {
      name: "a".to_string(),
      value: Box::new(ast::NodeKind::Literal(ast::Literal::Nullptr(
        a_id + 2,
        Some(ty.clone()),
      ))),
      is_const_expr: false,
      id: a_id,
      type_hint: Some(pointer_type.clone()),
    });

    cache
      .declarations
      .insert(a_id, ast::NodeKind::BindingStmt(binding_stmt_rc_a.clone()));

    cache.links.insert(a_id, a_id);

    let binding_stmt_a = ast::NodeKind::BindingStmt(binding_stmt_rc_a);

    let binding_stmt_b = ast::NodeKind::BindingStmt(std::rc::Rc::new(ast::BindingStmt {
      name: "b".to_string(),
      value: Box::new(ast::NodeKind::Reference(Mock::reference(a_id))),
      is_const_expr: false,
      id: a_id + 1,
      type_hint: Some(pointer_type),
    }));

    Mock::new(&type_cache, &cache, &llvm_context, &llvm_module)
      .llvm_function()
      .lower(&binding_stmt_a)
      .lower(&binding_stmt_b)
      .compare_with_file("binding_stmt_ptr_ref_val");
  }

  #[test]
  fn lower_binding_stmt_string_val() {
    let cache = symbol_table::SymbolTable::new();
    let type_cache = type_inference::TypeCache::new();
    let llvm_context = inkwell::context::Context::create();
    let llvm_module = llvm_context.create_module("test");
    let mut mock = Mock::new(&type_cache, &cache, &llvm_context, &llvm_module);
    let value = ast::NodeKind::Literal(ast::Literal::String("hello".to_string()));
    let binding_stmt = mock.binding("a", value, Some(ast::Type::Basic(ast::BasicType::String)));

    mock
      .llvm_function()
      .lower(&binding_stmt)
      .compare_with_file("binding_stmt_string_val");
  }

  #[test]
  fn lower_enum() {
    let type_cache = type_inference::TypeCache::new();
    let cache = symbol_table::SymbolTable::new();
    let llvm_context = inkwell::context::Context::create();
    let llvm_module = llvm_context.create_module("test");

    let enum_ = ast::NodeKind::Enum(std::rc::Rc::new(ast::Enum {
      name: "a".to_string(),
      variants: vec![("b".to_string(), 0), ("c".to_string(), 1)],
      value_type: ast::Type::Basic(ast::BasicType::Int(ast::IntSize::I32)),
      id: 0,
    }));

    Mock::new(&type_cache, &cache, &llvm_context, &llvm_module)
      .module()
      .lower(&enum_)
      .compare_with_file("enum");
  }

  #[test]
  fn lower_return_stmt_unit() {
    let type_cache = type_inference::TypeCache::new();
    let cache = symbol_table::SymbolTable::new();
    let llvm_context = inkwell::context::Context::create();
    let llvm_module = llvm_context.create_module("test");
    let return_stmt = ast::NodeKind::ReturnStmt(ast::ReturnStmt { value: None });

    Mock::new(&type_cache, &cache, &llvm_context, &llvm_module)
      .llvm_function()
      .lower(&return_stmt)
      .compare_with_file("return_stmt_unit");
  }

  #[test]
  fn lower_return_stmt() {
    let type_cache = type_inference::TypeCache::new();
    let cache = symbol_table::SymbolTable::new();
    let llvm_context = inkwell::context::Context::create();
    let llvm_module = llvm_context.create_module("test");

    let return_stmt = ast::NodeKind::ReturnStmt(ast::ReturnStmt {
      value: Some(Box::new(Mock::literal_int())),
    });

    Mock::new(&type_cache, &cache, &llvm_context, &llvm_module)
      .llvm_function()
      .lower(&return_stmt)
      .compare_with_file("return_stmt");
  }

  #[test]
  fn lower_extern_fn() {
    let type_cache = type_inference::TypeCache::new();
    let cache = symbol_table::SymbolTable::new();
    let llvm_context = inkwell::context::Context::create();
    let llvm_module = llvm_context.create_module("test");

    let extern_fn = ast::NodeKind::ExternFunction(std::rc::Rc::new(ast::ExternFunction {
      name: "a".to_string(),
      // TODO: Provide parameters in the signature.
      signature: Mock::signature_simple(true),
      attributes: Vec::new(),
      id: 0,
    }));

    Mock::new(&type_cache, &cache, &llvm_context, &llvm_module)
      .module()
      .lower(&extern_fn)
      .compare_with_file("extern_fn");
  }

  #[test]
  fn lower_extern_static() {
    let type_cache = type_inference::TypeCache::new();
    let cache = symbol_table::SymbolTable::new();
    let llvm_context = inkwell::context::Context::create();
    let llvm_module = llvm_context.create_module("test");

    let extern_static = ast::NodeKind::ExternStatic(std::rc::Rc::new(ast::ExternStatic {
      name: "a".to_string(),
      ty: ast::Type::Basic(ast::BasicType::Int(ast::IntSize::I32)),
      id: 0,
    }));

    Mock::new(&type_cache, &cache, &llvm_context, &llvm_module)
      .module()
      .lower(&extern_static)
      .compare_with_file("extern_static");
  }

  #[test]
  fn lower_if_expr_simple() {
    let mut cache = symbol_table::SymbolTable::new();
    let if_expr_id = cache.next_id();
    let mut type_cache = type_inference::TypeCache::new();

    type_cache.insert(if_expr_id, ast::Type::Unit);

    let llvm_context = inkwell::context::Context::create();
    let llvm_module = llvm_context.create_module("test");

    let if_expr = ast::NodeKind::IfExpr(ast::IfExpr {
      id: if_expr_id,
      condition: Box::new(ast::NodeKind::Literal(ast::Literal::Bool(true))),
      then_branch: Box::new(Mock::literal_int()),
      alternative_branches: Vec::new(),
      else_branch: None,
    });

    Mock::new(&type_cache, &cache, &llvm_context, &llvm_module)
      .llvm_function()
      .lower(&if_expr)
      .compare_with_file("if_expr_simple");
  }
}
