// REVISE: Instead of placing everything inside the `tests` module, label
// ... each implementation (impl) with `#[cfg(test)]`.
#[cfg(test)]
pub mod tests {
  use crate::{ast, cache, llvm_lowering::LlvmGenerator};
  use crate::{llvm_lowering::Lower, name_resolution};

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

    pub fn with_loop(&mut self) -> &mut Self {
      self.mock.generator.current_loop_block =
        Some(self.mock.generator.llvm_builder.get_insert_block().unwrap());

      self
    }

    pub fn lower(&mut self, node: &ast::NodeKind, access: bool) -> &mut Self {
      node.lower(&mut self.mock.generator, &self.mock.cache, access);

      self
    }

    pub fn lower_cache(&mut self, binding_id: cache::BindingId, access: bool) -> &mut Self {
      self.mock.cache.force_get(&binding_id).lower(
        &mut self.mock.generator,
        &self.mock.cache,
        access,
      );

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
    pub fn lower(&mut self, node: &ast::NodeKind, access: bool) -> &mut Self {
      node.lower(&mut self.mock.generator, &self.mock.cache, access);

      self
    }

    pub fn lower_cache(&mut self, binding_id: cache::BindingId, access: bool) -> &mut Self {
      self.mock.cache.force_get(&binding_id).lower(
        &mut self.mock.generator,
        &self.mock.cache,
        access,
      );

      self
    }
  }

  impl ToString for ModuleMock<'_, '_> {
    fn to_string(&self) -> String {
      self.mock.module.print_to_string().to_string()
    }
  }

  impl ComparableMock for ModuleMock<'_, '_> {
    //
  }

  pub struct Mock<'a, 'ctx> {
    context: &'ctx inkwell::context::Context,
    module: &'a inkwell::module::Module<'ctx>,
    generator: LlvmGenerator<'a, 'ctx>,
    cache: cache::Cache,
  }

  impl<'a, 'ctx> Mock<'a, 'ctx> {
    pub fn literal_int() -> ast::NodeKind {
      ast::NodeKind::Literal(ast::Literal::Int(1, ast::IntSize::I32))
    }

    pub fn reference(binding_id: cache::BindingId) -> Box<ast::Node> {
      Mock::node(ast::NodeKind::Reference(ast::Reference {
        pattern: ast::Pattern {
          global_qualifier: None,
          base_name: "test".to_string(),
          symbol_kind: name_resolution::SymbolKind::Definition,
          target_id: Some(binding_id),
        },
      }))
    }

    pub fn prototype_simple(is_extern: bool) -> ast::Prototype {
      ast::Prototype {
        parameters: Vec::new(),
        return_type: ast::Type::Unit,
        is_variadic: false,
        is_extern,
        accepts_instance: false,
        instance_type_id: None,
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

      assert_eq!(
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

    pub fn node(kind: ast::NodeKind) -> Box<ast::Node> {
      Box::new(ast::Node { kind, span: 0..0 })
    }

    pub fn new(
      context: &'ctx inkwell::context::Context,
      module: &'a inkwell::module::Module<'ctx>,
    ) -> Self {
      Self {
        context,
        module,
        generator: LlvmGenerator::new(context, module),
        cache: cache::Cache::new(),
      }
    }

    pub fn function(&'a mut self) -> FunctionMock<'a, 'ctx> {
      let function =
        self
          .module
          .add_function("test", self.context.void_type().fn_type(&[], false), None);

      let entry_block = self.context.append_basic_block(function, "entry");

      self.generator.llvm_builder.position_at_end(entry_block);
      self.generator.llvm_function_buffer = Some(function);

      FunctionMock::new(self, function)
    }

    pub fn module(&'a mut self) -> ModuleMock<'a, 'ctx> {
      ModuleMock { mock: self }
    }

    pub fn verify(&mut self) -> &mut Self {
      assert!(self.module.verify().is_ok());

      self
    }

    pub fn lower_without_context(&mut self, node: &ast::NodeKind, access: bool) -> &mut Self {
      node.lower(&mut self.generator, &self.cache, access);

      self
    }

    pub fn cache(&mut self, node: ast::NodeKind, binding_id: usize) -> &mut Self {
      self.cache.symbols.insert(binding_id, node);

      self
    }
  }
}
