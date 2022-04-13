use crate::llvm_lowering::Lower;
use crate::{ast, cache, llvm_lowering::LlvmGenerator};

pub trait ComparableMock: ToString {
  fn compare_with(&self, expected: &str) {
    Mock::compare(&self.to_string(), expected);
  }

  fn compare_with_file(&self, file_name: &str) {
    Mock::compare_with_file(&self.to_string(), file_name);
  }
}

pub trait BackMock<'a, 'ctx> {
  fn then(&mut self) -> &mut Mock<'a, 'ctx>;
}

pub struct FunctionMock<'a, 'ctx> {
  mock: &'a mut Mock<'a, 'ctx>,
  function: inkwell::values::FunctionValue<'ctx>,
}

impl<'a, 'ctx> FunctionMock<'a, 'ctx> {
  pub fn new(
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

  pub fn lower(&mut self, node: &ast::NodeKind) -> &mut Self {
    node.lower(&mut self.mock.generator, &self.mock.cache);

    self
  }
}

impl<'a, 'ctx> BackMock<'a, 'ctx> for FunctionMock<'a, 'ctx> {
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
    node.lower(&mut self.mock.generator, &self.mock.cache);

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
  pub fn compare(actual: &str, expected: &str) {
    let normalize_regex = regex::Regex::new(r"[\s]+").unwrap();
    let actual_fixed = normalize_regex.replace_all(actual, " ");
    let expected_fixed = normalize_regex.replace_all(expected, " ");

    assert_eq!(actual_fixed.trim(), expected_fixed.trim());
  }

  pub fn compare_with_file(actual: &str, file_name: &str) {
    let mut path = std::path::PathBuf::from("tests");

    path.push("lowering");
    path.push(file_name);
    path.set_extension("ll");
    Mock::compare(actual, std::fs::read_to_string(path).unwrap().as_str());
  }

  pub fn node(kind: ast::NodeKind) -> ast::Node {
    ast::Node { kind, span: 0..0 }
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

    FunctionMock::new(self, function)
  }

  pub fn module(&'a mut self) -> ModuleMock<'a, 'ctx> {
    ModuleMock { mock: self }
  }

  pub fn verify(&mut self) -> &mut Self {
    assert!(self.module.verify().is_ok());

    self
  }

  pub fn lower_without_context(&mut self, node: &ast::NodeKind) -> &mut Self {
    node.lower(&mut self.generator, &self.cache);

    self
  }
}
