use crate::{
  diagnostic, entry_point_check_pass, int_kind,
  node::{self, Node},
  pass,
};
use inkwell::{types::AnyType, values::BasicValue};

/// Visit the node and return its resulting LLVM value, or if it
/// was already previously visited, simply retrieve and return
/// the result from the LLVM values map.
///
/// Returns `None` if visiting the node did not insert a result
/// into the LLVM values map.
macro_rules! visit_or_retrieve_value {
  ($self:expr, $node:expr) => {{
    // TODO: Use `if let` syntax.
    if !$self.llvm_value_map.contains_key($node) {
      match $node {
        NodeValueKey::BoolLiteral(bool_literal) => $self.visit_bool_literal(bool_literal)?,
        NodeValueKey::IntLiteral(int_literal) => $self.visit_int_literal(int_literal)?,
        NodeValueKey::CallExpr(call_expr) => $self.visit_call_expr(call_expr)?,
      };
    }

    crate::pass_assert!($self.llvm_value_map.contains_key($node));

    $self.llvm_value_map.get(&$node).unwrap()
  }};
}

// TODO: Consider generalizing into a single function.
/// Visit the node and return its resulting LLVM type, or if it
/// was already previously visited, simply retrieve and return
/// the result from the LLVM types map.
///
/// Returns `None` if visiting the node did not insert a result
/// into the LLVM types map.
macro_rules! visit_or_retrieve_type {
  ($self:expr, $node:expr) => {{
    // TODO: Use `if let` syntax.
    if !$self.llvm_type_map.contains_key($node) {
      match $node {
        NodeKindKey::IntKind(int_kind) => $self.visit_int_kind(&int_kind)?,
        NodeKindKey::BoolKind(bool_kind) => $self.visit_bool_kind(&bool_kind)?,
        NodeKindKey::Prototype(prototype) => $self.visit_prototype(&prototype)?,
      };
    }

    crate::pass_assert!($self.llvm_type_map.contains_key($node));

    $self.llvm_type_map.get(&$node).unwrap()
  }};
}

// TODO: Visiting a basic block will remove it from the map. Is this okay?
macro_rules! visit_or_retrieve_block {
  ($self:expr, $block:expr) => {{
    if !$self.llvm_basic_block_map.contains_key($block) {
      $self.visit_block($block)?;
    }

    crate::pass_assert!($self.llvm_basic_block_map.contains_key($block));

    $self.llvm_basic_block_map.remove($block).unwrap()
  }};
}

#[derive(Hash, Eq, PartialEq)]
enum NodeValueKey<'a> {
  BoolLiteral(&'a node::BoolLiteral),
  IntLiteral(&'a node::IntLiteral),
  CallExpr(&'a node::CallExpr<'a>),
}

#[derive(Hash, Eq, PartialEq)]
enum NodeKindKey<'a> {
  Prototype(&'a node::Prototype),
  IntKind(&'a int_kind::IntKind),
  BoolKind(&'a int_kind::BoolKind),
}

impl<'a> From<&'a node::KindHolder> for NodeKindKey<'a> {
  fn from(kind: &'a node::KindHolder) -> Self {
    match kind {
      node::KindHolder::IntKind(kind) => NodeKindKey::IntKind(kind),
      node::KindHolder::BoolKind(kind) => NodeKindKey::BoolKind(kind),
    }
  }
}

impl<'a> From<node::ExprTransport<'a>> for NodeValueKey<'a> {
  fn from(expr_transport: node::ExprTransport<'a>) -> Self {
    match expr_transport {
      node::ExprTransport::BoolLiteral(bool_literal) => NodeValueKey::BoolLiteral(bool_literal),
      node::ExprTransport::IntLiteral(int_literal) => NodeValueKey::IntLiteral(int_literal),
      node::ExprTransport::CallExpr(call_expr) => NodeValueKey::CallExpr(call_expr),
    }
  }
}

impl<'a> From<&'a node::ExprHolder<'a>> for NodeValueKey<'a> {
  fn from(expr_holder: &'a node::ExprHolder<'a>) -> Self {
    match expr_holder {
      node::ExprHolder::BoolLiteral(bool_literal) => NodeValueKey::BoolLiteral(bool_literal),
      node::ExprHolder::IntLiteral(int_literal) => NodeValueKey::IntLiteral(int_literal),
      node::ExprHolder::CallExpr(call_expr) => NodeValueKey::CallExpr(call_expr),
    }
  }
}

fn mangle_name(scope_name: &String, name: &String) -> String {
  format!(".{}.{}", scope_name, name)
}

pub struct LlvmLoweringPass<'a, 'ctx> {
  llvm_context: &'ctx inkwell::context::Context,
  pub llvm_module: &'a inkwell::module::Module<'ctx>,
  llvm_type_map: std::collections::HashMap<NodeKindKey<'a>, inkwell::types::AnyTypeEnum<'ctx>>,
  llvm_value_map:
    std::collections::HashMap<NodeValueKey<'a>, inkwell::values::BasicValueEnum<'ctx>>,
  llvm_basic_block_map:
    std::collections::HashMap<&'a node::Block<'a>, inkwell::basic_block::BasicBlock<'ctx>>,
  /// A mapping of `then` block nodes to their corresponding `after` LLVM basic blocks.
  llvm_basic_block_fallthrough_map:
    std::collections::HashMap<&'a node::Block<'a>, inkwell::basic_block::BasicBlock<'ctx>>,
  llvm_function_like_buffer: Option<inkwell::values::FunctionValue<'ctx>>,
  // TODO: Consider making Option?
  llvm_builder_buffer: inkwell::builder::Builder<'ctx>,
  module_buffer: Option<&'a node::Module<'a>>,
}

impl<'a, 'ctx> LlvmLoweringPass<'a, 'ctx> {
  pub fn new(
    llvm_context: &'ctx inkwell::context::Context,
    llvm_module: &'a inkwell::module::Module<'ctx>,
  ) -> Self {
    Self {
      llvm_context,
      llvm_module,
      llvm_type_map: std::collections::HashMap::new(),
      llvm_value_map: std::collections::HashMap::new(),
      llvm_basic_block_map: std::collections::HashMap::new(),
      llvm_basic_block_fallthrough_map: std::collections::HashMap::new(),
      llvm_function_like_buffer: None,
      llvm_builder_buffer: llvm_context.create_builder(),
      module_buffer: None,
    }
  }

  fn get_function_type_from(
    parameters: &[inkwell::types::BasicMetadataTypeEnum<'ctx>],
    llvm_return_type: &inkwell::types::AnyTypeEnum<'ctx>,
    is_variadic: bool,
  ) -> Result<inkwell::types::FunctionType<'ctx>, diagnostic::Diagnostic> {
    Ok(match llvm_return_type {
      inkwell::types::AnyTypeEnum::IntType(int_type) => int_type.fn_type(parameters, is_variadic),
      inkwell::types::AnyTypeEnum::FloatType(float_type) => {
        float_type.fn_type(parameters, is_variadic)
      }
      inkwell::types::AnyTypeEnum::VoidType(void_type) => {
        void_type.fn_type(parameters, is_variadic)
      }
      _ => return Err(diagnostic::unreachable()),
    })
  }

  // TODO: Make use of, or remove code.
  fn _conditional_visit_kind(&mut self, kind: &'a NodeKindKey<'a>) -> pass::PassResult {
    if !self.llvm_type_map.contains_key(&kind) {
      match kind {
        NodeKindKey::Prototype(prototype) => prototype.accept(self),
        NodeKindKey::IntKind(int_kind) => int_kind.accept(self),
        NodeKindKey::BoolKind(bool_kind) => bool_kind.accept(self),
      }?;
    }

    Ok(())
  }
}

// impl From<&inkwell::values::BasicValueEnum> for

impl<'a, 'ctx> pass::Pass<'a> for LlvmLoweringPass<'a, 'ctx> {
  fn get_requirements(&self) -> pass::PassRequirements {
    pass::PassRequirements {
      ignore_previous_errors: false,
      ..pass::PassRequirements::default()
    }
  }

  fn visit(&mut self, node: &'a dyn node::Node) -> pass::PassResult {
    if !node.is_top_level() {
      return Ok(());
    }

    node.accept(self)
  }

  fn visit_int_kind(&mut self, int_kind: &'a int_kind::IntKind) -> pass::PassResult {
    self.llvm_type_map.insert(
      NodeKindKey::IntKind(&int_kind),
      match int_kind.size {
        int_kind::IntSize::Bit8 => self.llvm_context.i8_type().as_any_type_enum(),
        int_kind::IntSize::Bit16 => self.llvm_context.i16_type().as_any_type_enum(),
        int_kind::IntSize::Bit32 => self.llvm_context.i32_type().as_any_type_enum(),
        int_kind::IntSize::Bit64 => self.llvm_context.i64_type().as_any_type_enum(),
      },
    );

    Ok(())
  }

  fn visit_bool_kind(&mut self, bool_kind: &'a int_kind::BoolKind) -> pass::PassResult {
    self.llvm_type_map.insert(
      NodeKindKey::BoolKind(&bool_kind),
      self.llvm_context.bool_type().as_any_type_enum(),
    );

    Ok(())
  }

  fn visit_function(&mut self, function: &'a node::Function<'a>) -> pass::PassResult {
    let llvm_function_type =
      visit_or_retrieve_type!(self, &NodeKindKey::Prototype(&function.prototype));

    crate::pass_assert!(llvm_function_type.is_function_type());
    crate::pass_assert!(self.module_buffer.is_some());

    let llvm_function_name = if function.prototype.name == entry_point_check_pass::ENTRY_POINT_NAME
    {
      function.prototype.name.clone()
    } else {
      mangle_name(&self.module_buffer.unwrap().name, &function.prototype.name)
    };

    let llvm_function = self.llvm_module.add_function(
      llvm_function_name.as_str(),
      llvm_function_type.into_function_type(),
      Some(
        if function.prototype.name == entry_point_check_pass::ENTRY_POINT_NAME {
          inkwell::module::Linkage::External
        } else {
          inkwell::module::Linkage::Private
        },
      ),
    );

    // TODO: Find a way to use only one loop to process both local parameters and LLVM's names.
    for (i, ref mut llvm_parameter) in llvm_function.get_param_iter().enumerate() {
      let (parameter_name, _) = &function.prototype.parameters[i];

      llvm_parameter.set_name(parameter_name.as_str());
    }

    self.llvm_function_like_buffer = Some(llvm_function);
    self.visit_block(&function.body)?;
    // FIXME: Commented out for debugging.
    // crate::pass_assert!(self.llvm_function_like_buffer.unwrap().verify(false));

    Ok(())
  }

  fn visit_module(&mut self, module: &'a node::Module<'a>) -> pass::PassResult {
    // Reset all buffers when visiting a new module.
    self.llvm_builder_buffer.clear_insertion_position();
    self.llvm_function_like_buffer = None;
    self.module_buffer = Some(module);

    for top_level_node in module.symbol_table.values() {
      match top_level_node {
        node::TopLevelNodeHolder::Function(function) => self.visit_function(function)?,
        node::TopLevelNodeHolder::External(external) => self.visit_external(external)?,
      };
    }

    Ok(())
  }

  fn visit_external(&mut self, external: &'a node::External) -> pass::PassResult {
    let llvm_function_type =
      visit_or_retrieve_type!(self, &NodeKindKey::Prototype(&external.prototype));

    crate::pass_assert!(llvm_function_type.is_function_type());

    // TODO: Are externs always 'External' linkage?
    self.llvm_function_like_buffer = Some(self.llvm_module.add_function(
      external.prototype.name.as_str(),
      llvm_function_type.into_function_type(),
      Some(inkwell::module::Linkage::External),
    ));

    crate::pass_assert!(self.llvm_function_like_buffer.unwrap().verify(false));

    Ok(())
  }

  fn visit_block(&mut self, block: &'a node::Block<'a>) -> pass::PassResult {
    crate::pass_assert!(self.llvm_function_like_buffer.is_some());

    let llvm_block = self.llvm_context.append_basic_block(
      self.llvm_function_like_buffer.unwrap(),
      block.llvm_name.as_str(),
    );

    self.llvm_basic_block_map.insert(block, llvm_block);
    self.llvm_builder_buffer.position_at_end(llvm_block);

    for statement in &block.statements {
      match statement {
        node::AnyStmtNode::ReturnStmt(return_stmt) => self.visit_return_stmt(&return_stmt)?,
        // TODO: Consider relocating as an associated function for `Pass`? Does this occur more than once?
        node::AnyStmtNode::ExprWrapperStmt(expr) => match expr {
          // TODO: Implement missing cases.
          node::ExprHolder::CallExpr(call_expr) => self.visit_call_expr(call_expr)?,
          _ => todo!(),
        },
        node::AnyStmtNode::LetStmt(let_stmt) => self.visit_let_stmt(&let_stmt)?,
        node::AnyStmtNode::IfStmt(if_stmt) => self.visit_if_stmt(&if_stmt)?,
      };
    }

    Ok(())
  }

  fn visit_return_stmt(&mut self, return_stmt: &'a node::ReturnStmt<'a>) -> pass::PassResult {
    crate::pass_assert!(self.llvm_builder_buffer.get_insert_block().is_some());

    if return_stmt.value.is_some() {
      visit_or_retrieve_value!(
        self,
        &NodeValueKey::from(node::ExprTransport::from(
          return_stmt.value.as_ref().unwrap()
        ))
      );
    }

    self
      .llvm_builder_buffer
      .build_return(if return_stmt.value.is_some() {
        Some(
          self
            .llvm_value_map
            .get(&NodeValueKey::from(node::ExprTransport::from(
              return_stmt.value.as_ref().unwrap(),
            )))
            .unwrap(),
        )
      } else {
        None
      });

    Ok(())
  }

  fn visit_let_stmt(&mut self, let_stmt: &'a node::LetStmt<'a>) -> pass::PassResult {
    use inkwell::types::BasicType;

    crate::pass_assert!(self.llvm_builder_buffer.get_insert_block().is_some());

    let llvm_any_type =
      visit_or_retrieve_type!(self, &NodeKindKey::from(&let_stmt.kind_group.kind));

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
      .build_alloca(llvm_type, let_stmt.name.as_str());

    let llvm_value = visit_or_retrieve_value!(self, &NodeValueKey::from(&let_stmt.value));

    // FIXME: Consider adding a method to TAKE the values from `llvm_value_map` and `llvm_type_map`, as errors occur when dealing with reference values and types instead of OWNED ones.

    self
      .llvm_builder_buffer
      // TODO: Calling `to_owned()` clones the value. This is not ideal, and could cause problems? Or, SHOULD we be cloning values?
      .build_store(llvm_alloca_inst_ptr, llvm_value.to_owned());

    // TODO: No insertion into the value map?

    Ok(())
  }

  fn visit_bool_literal(&mut self, bool_literal: &'a node::BoolLiteral) -> pass::PassResult {
    self.llvm_value_map.insert(
      node::ExprTransport::BoolLiteral(bool_literal).into(),
      inkwell::values::BasicValueEnum::IntValue(
        self
          .llvm_context
          .bool_type()
          .const_int(bool_literal.value as u64, false),
      ),
    );

    Ok(())
  }

  fn visit_int_literal(&mut self, int_literal: &'a node::IntLiteral) -> pass::PassResult {
    let llvm_type = visit_or_retrieve_type!(self, &NodeKindKey::IntKind(&int_literal.kind));

    let llvm_value = match llvm_type {
      inkwell::types::AnyTypeEnum::IntType(int_type) => {
        int_type.const_int(int_literal.value as u64, false)
      }
      _ => {
        return Err(diagnostic::Diagnostic {
          // TODO: Better error message?
          message: "expected integer type".to_string(),
          severity: diagnostic::Severity::Internal,
        });
      }
    };

    self.llvm_value_map.insert(
      node::ExprTransport::IntLiteral(&int_literal).into(),
      inkwell::values::BasicValueEnum::IntValue(llvm_value),
    );

    Ok(())
  }

  fn visit_call_expr(&mut self, call_expr: &'a node::CallExpr<'a>) -> pass::PassResult {
    crate::pass_assert!(self.llvm_builder_buffer.get_insert_block().is_some());

    match &call_expr.callee {
      node::Stub::Callable { name: _, value } => {
        crate::pass_assert!(value.is_some());

        match value.as_ref().unwrap() {
          // TODO: Need to stop callable from lowering twice or more times.
          node::StubValueTransport::Function(function) => self.visit_function(function)?,
          node::StubValueTransport::External(external) => self.visit_external(external)?,
        }
      } // TODO: Prevent other types of stubs.
    };

    let mut arguments = Vec::new();

    arguments.reserve(call_expr.arguments.len());

    for argument in &call_expr.arguments {
      // TODO: Cloning argument.
      let llvm_value = visit_or_retrieve_value!(self, &NodeValueKey::from(argument.clone()));

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

    // TODO: Process & provide call arguments.
    let llvm_call_result = self
      .llvm_builder_buffer
      .build_call(
        self.llvm_function_like_buffer.unwrap(),
        arguments.as_slice(),
        "call",
      )
      .try_as_basic_value();

    crate::pass_assert!(llvm_call_result.is_left());

    self.llvm_value_map.insert(
      NodeValueKey::CallExpr(call_expr),
      llvm_call_result.left().unwrap(),
    );

    Ok(())
  }

  fn visit_if_stmt(&mut self, if_stmt: &'a node::IfStmt<'a>) -> pass::PassResult {
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
      .llvm_basic_block_fallthrough_map
      .insert(&if_stmt.then_block, llvm_after_block);

    let llvm_then_block = visit_or_retrieve_block!(self, &if_stmt.then_block);

    // Position the builder on the `after` block, for the next statement(s) (if any).
    // It is important to do this after visiting the `then` block.
    self.llvm_builder_buffer.position_at_end(llvm_after_block);

    // TODO: Does it matter if the condition is visited before the `then` block? (Remember that the code is not being executed).
    let llvm_condition = visit_or_retrieve_value!(self, &NodeValueKey::from(&if_stmt.condition));

    // TODO: Assert the condition is also an integer with bit-size 1 (boolean).
    crate::pass_assert!(llvm_condition.is_int_value());

    let llvm_temporary_builder = self.llvm_context.create_builder();

    llvm_temporary_builder.position_at_end(llvm_parent_block);

    llvm_temporary_builder.build_conditional_branch(
      llvm_condition.into_int_value(),
      llvm_then_block,
      llvm_after_block,
    );

    // If there is no terminator instruction on the `then` LLVM basic block,
    // build a link to continue the code after the if statement.
    if llvm_then_block.get_terminator().is_none() {
      llvm_temporary_builder.position_at_end(llvm_then_block);
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

    Ok(())
  }

  fn visit_prototype(&mut self, prototype: &'a node::Prototype) -> pass::PassResult {
    // TODO: Creating regardless.
    let llvm_void_type = self.llvm_context.void_type().as_any_type_enum();

    // TODO: Simplify process of lowering parameters for externals as well.
    let mut parameters = vec![];

    parameters.reserve(prototype.parameters.len());

    // TODO: Further on, need to make use of the parameter's name somehow (maybe during lookups?).
    for (parameter_name, parameter_kind_group) in &prototype.parameters {
      let llvm_parameter =
        visit_or_retrieve_type!(self, &NodeKindKey::from(&parameter_kind_group.kind));

      parameters.push(match llvm_parameter {
        // TODO: Add other types as they become available.
        inkwell::types::AnyTypeEnum::IntType(int_type) => {
          inkwell::types::BasicMetadataTypeEnum::IntType(*int_type)
        }
        inkwell::types::AnyTypeEnum::VoidType(_) => {
          return Err(diagnostic::Diagnostic {
            message: format!("type of parameter `{}` cannot be void", parameter_name),
            severity: diagnostic::Severity::Internal,
          })
        }
        _ => {
          return Err(diagnostic::Diagnostic {
            message: format!("unsupported parameter type for `{}`", parameter_name),
            severity: diagnostic::Severity::Internal,
          })
        }
      });
    }

    let llvm_function_type = Self::get_function_type_from(
      parameters.as_slice(),
      if let Some(return_kind_group) = &prototype.return_kind_group {
        visit_or_retrieve_type!(self, &NodeKindKey::from(&return_kind_group.kind))
      } else {
        &llvm_void_type
      },
      prototype.is_variadic,
    )?;

    self.llvm_type_map.insert(
      NodeKindKey::Prototype(&prototype),
      llvm_function_type.as_any_type_enum(),
    );

    Ok(())
  }
}

#[cfg(test)]
mod tests {
  use super::*;
  use pass::Pass;

  #[test]
  fn proper_initial_values<'a>() {
    let llvm_context = inkwell::context::Context::create();
    let llvm_module: inkwell::module::Module<'_> = llvm_context.create_module("test");

    assert_eq!(
      true,
      LlvmLoweringPass::new(&llvm_context, &llvm_module)
        .llvm_type_map
        .is_empty()
    );
  }

  // TODO:
  // #[test]
  // fn visit_or_retrieve_type() {
  //   let llvm_context = inkwell::context::Context::create();
  //   let llvm_module = llvm_context.create_module("test");
  //   let mut llvm_lowering_pass = LlvmLoweringPass::new(&llvm_context, &llvm_module);

  //   let int_kind = node::KindTransport::IntKind(&int_kind::IntKind {
  //     size: int_kind::IntSize::Bit32,
  //     is_signed: true,
  //   });

  //   let visit_or_retrieve_result = visit_or_retrieve_type!(llvm_lowering_pass, &int_kind);

  //   assert_eq!(true, visit_or_retrieve_result.is_ok());
  //   assert_eq!(true, visit_or_retrieve_result.ok().is_some());
  //   assert_eq!(1, llvm_lowering_pass.llvm_type_map.len());
  // }

  #[test]
  fn visit_int_kind() {
    let llvm_context = inkwell::context::Context::create();
    let llvm_module = llvm_context.create_module("test");
    let mut llvm_lowering_pass = LlvmLoweringPass::new(&llvm_context, &llvm_module);

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
    let mut llvm_lowering_pass = LlvmLoweringPass::new(&llvm_context, &llvm_module);
    let module = node::Module::new("test");

    llvm_lowering_pass.module_buffer = Some(&module);

    let function = node::Function {
      is_public: false,
      prototype: node::Prototype {
        name: "foo".to_string(),
        return_kind_group: None,
        parameters: vec![],
        is_variadic: false,
      },
      body: node::Block {
        llvm_name: "entry".to_string(),

        statements: vec![node::AnyStmtNode::ReturnStmt(node::ReturnStmt {
          value: None,
        })],
      },
    };

    let visit_function_result = llvm_lowering_pass.visit_function(&function);

    assert_eq!(true, visit_function_result.is_ok());
    assert_eq!(true, llvm_lowering_pass.llvm_function_like_buffer.is_some());
  }

  // TODO: Add more tests: `visit_prototype()`, etc.
}
