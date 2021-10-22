extern crate inkwell;
extern crate ionlang;

#[cfg(test)]
mod tests {
  #[test]
  fn function_lowering_test() {
    let llvm_context = inkwell::context::Context::create();
    let llvm_module = llvm_context.create_module("test");

    // let llvm_lowering_pass =
    //   ionlang::llvm_lowering_pass::LlvmLoweringPass::new(&llvm_context, &llvm_module);

    // TODO: Continue implementation.
    // llvm_lowering_pass.visit_function(function: &function::Function)

    assert_eq!(1, 1);
  }

  // TODO: Write more integration tests, and ensure they're picked up when running 'cargo test'.
}
