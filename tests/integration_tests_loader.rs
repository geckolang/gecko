extern crate gecko;
extern crate inkwell;

#[cfg(test)]
mod tests {
  use gecko::lint::Lint;
  use gecko::llvm_lowering::Lower;
  use gecko::semantic_check::SemanticCheck;
  use std::io::Read;

  fn load_test_file(name: &str) -> String {
    let mut path_buffer = std::env::current_dir().unwrap();

    path_buffer.push("tests");
    path_buffer.push("sources");
    path_buffer.push(name);
    path_buffer.set_extension("ko");

    let mut file = std::fs::File::open(path_buffer).unwrap();
    let mut contents = String::new();

    file.read_to_string(&mut contents).unwrap();

    contents
  }

  fn lex(source_code: &str) -> Vec<gecko::lexer::Token> {
    let tokens = gecko::lexer::Lexer::from_str(source_code).lex_all();

    // REVIEW: What about illegal tokens?
    // REVISE: This might be inefficient for larger programs, so consider passing an option to the lexer.
    // Filter tokens to only include those that are relevant (ignore whitespace, comments, etc.).
    tokens
      .unwrap()
      .into_iter()
      .filter(|token| {
        !matches!(
          token.0,
          gecko::lexer::TokenKind::Whitespace(_) | gecko::lexer::TokenKind::Comment(_)
        )
      })
      .collect()
  }

  #[test]
  fn integration_tests() {
    let source_files = vec!["recursion", "shorthand", "simplest_program", "struct"];
    let mut sources = Vec::new();

    for source_file in &source_files {
      sources.push(load_test_file(&source_file));
    }

    let llvm_context = inkwell::context::Context::create();
    let llvm_module = llvm_context.create_module("test");
    let mut cache = gecko::cache::Cache::new();
    let mut name_resolver = gecko::name_resolution::NameResolver::new();
    let mut lint_context = gecko::lint::LintContext::new();
    let mut llvm_generator = gecko::llvm_lowering::LlvmGenerator::new(&llvm_context, &llvm_module);
    let mut semantic_check_context = gecko::semantic_check::SemanticCheckContext::new();
    let mut ast = std::collections::HashMap::new();

    // TODO: Ensure both vectors are of same length, and zip iterators.
    // Read, lex, parse, perform name resolution (declarations)
    // and collect the AST (top-level nodes) from each source file.
    for (index, source_file) in source_files.iter().enumerate() {
      let tokens = lex(&sources[index]);
      let mut parser = gecko::parser::Parser::new(tokens, &mut cache);
      let top_level_nodes = parser.parse_all();

      assert!(top_level_nodes.is_ok());

      // REVISE: File names need to conform to identifier rules.
      let global_qualifier = (String::from("string"), source_file.to_string());

      name_resolver.create_module(global_qualifier.clone());
      ast.insert(global_qualifier, top_level_nodes.unwrap());
    }

    // After all the ASTs have been collected, perform name resolution step.
    assert!(name_resolver.run(&mut ast, &mut cache).is_empty());

    // Once symbols are resolved, we can proceed to the other phases.
    for inner_ast in ast.values_mut() {
      for top_level_node in inner_ast {
        top_level_node.check(&mut semantic_check_context, &mut cache);

        // REVIEW: Can we mix linting with type-checking without any problems?
        top_level_node.lint(&mut cache, &mut lint_context);
      }
    }

    // Lowering cannot proceed if there was an error.
    // TODO: Missing semantic check context's diagnostics.
    assert!(lint_context.diagnostic_builder.diagnostics.is_empty());

    // REVISE: Any way for better efficiency (less loops)?
    // Once symbols are resolved, we can proceed to the other phases.
    for (global_qualifier, inner_ast) in &mut ast {
      // REVIEW: Must join package and module name for uniqueness?
      llvm_generator.module_name = global_qualifier.1.clone();

      for top_level_node in inner_ast {
        top_level_node.lower(&mut llvm_generator, &mut cache, false);
      }
    }

    assert!(llvm_module.verify().is_ok());
  }
}
