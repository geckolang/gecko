extern crate gecko;
extern crate inkwell;

#[cfg(test)]
mod tests {
  use gecko::lint::Lint;
  use gecko::llvm_lowering::Lower;
  use gecko::name_resolution::Resolve;
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

  // REVISE: Test isn't working for some reason.
  #[test]
  fn test_sources() {
    // TODO: Complete implementation.

    let source_files = vec!["recursion", "shorthands"];
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
    let mut type_context = gecko::semantic_check::SemanticCheckContext::new();
    let mut ast = std::collections::HashMap::new();
    let mut diagnostics = Vec::new();

    // Read, lex, parse, perform name resolution (declarations)
    // and collect the AST (top-level nodes) from each source file.
    for (index, source_code) in source_files.iter().enumerate() {
      let tokens = lex(source_code);
      let mut parser = gecko::parser::Parser::new(tokens, &mut cache);

      let mut top_level_nodes = match parser.parse_all() {
        Ok(nodes) => nodes,
        Err(_diagnostic) => {
          assert_eq!(false, true);

          return;
        }
      };

      // REVISE: File names need to conform to identifier rules.
      let source_file_name = source_files[index].to_string();

      name_resolver.create_module(source_file_name.clone());

      for top_level_node in &mut top_level_nodes {
        top_level_node.declare(&mut name_resolver);
      }

      ast.insert(source_file_name, top_level_nodes);
    }

    // After all the ASTs have been collected, perform actual name resolution.
    for (module_name, inner_ast) in &mut ast {
      name_resolver.set_active_module(module_name.clone());

      for top_level_node in inner_ast {
        top_level_node.resolve(&mut name_resolver);
      }
    }

    diagnostics.extend(name_resolver.diagnostic_builder.diagnostics.clone());

    // Cannot continue to other phases if name resolution failed.
    if diagnostics
      .iter()
      .any(|diagnostic| diagnostic.severity == gecko::diagnostic::Severity::Error)
    {
      assert_eq!(false, true);
      todo!();
    }

    // Once symbols are resolved, we can proceed to the other phases.
    for inner_ast in ast.values_mut() {
      for top_level_node in inner_ast {
        top_level_node.check(&mut type_context, &mut cache);

        // REVIEW: Can we mix linting with type-checking without any problems?
        top_level_node.lint(&mut cache, &mut lint_context);
      }
    }

    // REVISE: Any way for better efficiency (less loops)?
    // Lowering cannot proceed if there was an error.
    if diagnostics
      .iter()
      .any(|diagnostic| diagnostic.severity == gecko::diagnostic::Severity::Error)
    {
      assert_eq!(false, true);

      return;
    }

    // Once symbols are resolved, we can proceed to the other phases.
    for (module_name, inner_ast) in &mut ast {
      llvm_generator.module_name = module_name.clone();

      for top_level_node in inner_ast {
        top_level_node.lower(&mut llvm_generator, &mut cache);
      }
    }

    // assert_eq!(true, false);
    assert!(llvm_module.verify().is_ok());
    assert!(diagnostics.is_empty());
  }

  // TODO: Write more integration tests, and ensure they're picked up when running 'cargo test'.
}
