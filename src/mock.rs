#[cfg(test)]
pub mod tests {
  use crate::{ast, cache, lowering, visitor::LoweringVisitor};
  use crate::{name_resolution, type_inference};

  pub trait ComparableMock: ToString {
    fn compare_with(&self, expected: &str) {
      Mock::compare(&self.to_string(), expected);
    }

    fn compare_with_file(&self, file_name: &str) {
      Mock::compare_with_file(&self.to_string(), file_name);
    }
  }

  pub trait ThenMock<'a, 'ctx> {
    fn then(&mut self) -> &mut Mock<'a, 'ctx>;
  }

  pub struct FunctionMock<'a, 'ctx> {
    mock: &'a mut Mock<'a, 'ctx>,
    function: inkwell::values::FunctionValue<'ctx>,
  }

  impl<'a, 'ctx> FunctionMock<'a, 'ctx> {
    fn new(
      builder: &'a mut Mock<'a, 'ctx>,
      function: inkwell::values::FunctionValue<'ctx>,
    ) -> Self {
      Self {
        mock: builder,
        function,
      }
    }

    pub fn lower(&mut self, node: &ast::NodeKind) -> &mut Self {
      self.mock.lowering_context.dispatch(node);

      self
    }

    pub fn finish_and_verify(mut self) -> Self {
      // TODO: Create builder, build return void and verify module.

      self
    }
  }

  impl<'a, 'ctx> ThenMock<'a, 'ctx> for FunctionMock<'a, 'ctx> {
    fn then(&mut self) -> &mut Mock<'a, 'ctx> {
      &mut self.mock
    }
  }

  impl ToString for FunctionMock<'_, '_> {
    fn to_string(&self) -> String {
      self
        .function
        .as_global_value()
        .print_to_string()
        .to_string()
    }
  }

  impl ComparableMock for FunctionMock<'_, '_> {
    //
  }

  pub struct ModuleMock<'a, 'ctx> {
    mock: &'a mut Mock<'a, 'ctx>,
  }

  impl ModuleMock<'_, '_> {
    pub fn lower(&mut self, node: &ast::NodeKind) -> &mut Self {
      // TODO:
      // node.lower(&mut self.mock.lowering_visitor, &self.mock.cache, access);
      self.mock.lowering_context.dispatch(node);

      self
    }
  }

  impl ToString for ModuleMock<'_, '_> {
    fn to_string(&self) -> String {
      self.mock.llvm_module.print_to_string().to_string()
    }
  }

  impl ComparableMock for ModuleMock<'_, '_> {
    //
  }

  pub struct Mock<'a, 'ctx> {
    context: &'ctx inkwell::context::Context,
    llvm_module: &'a inkwell::module::Module<'ctx>,
    lowering_context: lowering::LoweringContext<'a, 'ctx>,
    id_counter: usize,
  }

  impl<'a, 'ctx> Mock<'a, 'ctx> {
    pub fn free_function(statements: Vec<ast::NodeKind>) -> ast::NodeKind {
      ast::NodeKind::Function(std::rc::Rc::new(ast::Function {
        attributes: Vec::new(),
        body: std::rc::Rc::new(ast::BlockExpr {
          id: 1,
          statements,
          yields: None,
        }),
        id: 0,
        generics: None,
        name: "test_function".to_string(),
        signature: ast::Signature {
          accepts_instance: false,
          instance_type_id: None,
          is_extern: false,
          is_variadic: false,
          parameters: Vec::new(),
          return_type_hint: None,
          return_type_id: 2,
          this_parameter: None,
        },
        static_owner_name: None,
      }))
    }

    pub fn free_binding(
      id: cache::Id,
      name: &str,
      value: ast::NodeKind,
      type_hint: Option<ast::Type>,
    ) -> ast::NodeKind {
      ast::NodeKind::BindingStmt(std::rc::Rc::new(ast::BindingStmt {
        id,
        value: Box::new(value),
        is_const_expr: false,
        name: name.to_string(),
        type_hint,
      }))
    }

    pub fn int_type() -> ast::Type {
      ast::Type::Basic(ast::BasicType::Int(ast::IntSize::I32))
    }

    pub fn literal_int() -> ast::NodeKind {
      ast::NodeKind::Literal(ast::Literal::Int(1, ast::IntSize::I32))
    }

    pub fn reference(link_id: cache::Id) -> ast::Reference {
      ast::Reference {
        pattern: ast::Pattern {
          link_id,
          qualifier: None,
          base_name: "test_reference".to_string(),
          sub_name: None,
          symbol_kind: name_resolution::SymbolKind::Definition,
        },
      }
    }

    pub fn binding(
      &mut self,
      name: &str,
      value: ast::NodeKind,
      type_hint: Option<ast::Type>,
    ) -> ast::NodeKind {
      Self::free_binding(self.next_id(), name, value, type_hint)
    }

    pub fn signature_simple(is_extern: bool) -> ast::Signature {
      ast::Signature {
        parameters: Vec::new(),
        return_type_hint: None,
        is_variadic: false,
        is_extern,
        accepts_instance: false,
        instance_type_id: None,
        // FIXME: Temporary.
        return_type_id: 99,
        this_parameter: None,
      }
    }

    pub fn compare(actual: &str, expected: &str) {
      // REVIEW: Perhaps this can be improved (in terms of efficiency).

      let comments_regex = regex::Regex::new(r";[^\n]*").unwrap();
      let whitespace_regex = regex::Regex::new(r"[\s]+").unwrap();
      let actual_no_comments = comments_regex.replace_all(actual, "");
      let expected_no_comments = comments_regex.replace_all(expected, "");
      let actual_normalized_whitespace = whitespace_regex.replace_all(&actual_no_comments, " ");
      let expected_normalized_whitespace = whitespace_regex.replace_all(&expected_no_comments, " ");

      pretty_assertions::assert_eq!(
        actual_normalized_whitespace.trim(),
        expected_normalized_whitespace.trim()
      );
    }

    pub fn compare_with_file(actual: &str, file_name: &str) {
      let mut path = std::path::PathBuf::from("tests");

      path.push("lowering");
      path.push(file_name);
      path.set_extension("ll");
      Mock::compare(actual, std::fs::read_to_string(path).unwrap().as_str());
    }

    pub fn new(
      type_cache: &'a type_inference::TypeCache,
      cache: &'a cache::Cache,
      context: &'ctx inkwell::context::Context,
      module: &'a inkwell::module::Module<'ctx>,
    ) -> Self {
      Self {
        context,
        llvm_module: module,
        lowering_context: lowering::LoweringContext::new(&type_cache, &cache, context, module),
        id_counter: 0,
      }
    }

    pub fn llvm_function(&'a mut self) -> FunctionMock<'a, 'ctx> {
      let function =
        self
          .llvm_module
          .add_function("test", self.context.void_type().fn_type(&[], false), None);

      let entry_block = self.context.append_basic_block(function, "entry");

      self
        .lowering_context
        .llvm_builder
        .position_at_end(entry_block);

      self.lowering_context.llvm_function_buffer = Some(function);

      FunctionMock::new(self, function)
    }

    pub fn module(&'a mut self) -> ModuleMock<'a, 'ctx> {
      ModuleMock { mock: self }
    }

    pub fn _verify(&mut self) -> &mut Self {
      assert!(self.llvm_module.verify().is_ok());

      self
    }

    fn next_id(&mut self) -> cache::Id {
      let id = self.id_counter;

      self.id_counter += 1;

      id
    }
  }
}
