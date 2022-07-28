extern crate gecko;
extern crate inkwell;

#[cfg(test)]
mod tests {
  use gecko::{
    lowering, name_resolution, type_check, type_inference,
    visitor::{self, LoweringVisitor},
  };
  use pretty_assertions::assert_eq;
  use std::{fs, io::Read};

  fn load_test_file(path: &std::path::PathBuf) -> String {
    let mut file = std::fs::File::open(path).unwrap();
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

  fn lower_file(
    source_file_contents: &str,
    qualifier: gecko::name_resolution::Qualifier,
  ) -> String {
    let mut cache = gecko::cache::Cache::new();
    // let mut lint_context = gecko::lint::LintContext::new();
    // let mut llvm_generator = gecko::lowering::LlvmGenerator::new(&llvm_context, &llvm_module);
    let mut ast_map = std::collections::BTreeMap::new();
    let tokens = lex(source_file_contents);
    let mut parser = gecko::parser::Parser::new(tokens, &mut cache);
    let top_level_nodes = parser.parse_all();

    assert!(top_level_nodes.is_ok());
    ast_map.insert(qualifier.clone(), top_level_nodes.unwrap());
    assert!(name_resolution::run(&mut ast_map, &mut cache).is_empty());

    let (type_inference_diagnostics, type_cache) = type_inference::run(&ast_map, &cache);

    assert!(type_inference_diagnostics.is_empty());

    let mut type_check_context = type_check::TypeCheckContext::new(&cache);

    // // Once symbols are resolved, we can proceed to the other phases.
    for inner_ast in ast_map.values() {
      // FIXME: Linting disabled temporarily.
      // REVIEW: Can we mix linting with type-checking without any problems?
      for top_level_node in inner_ast {
        // top_level_node.lint(&mut lint_context);
        // TODO: Combine type-checking with the new type-inference to ensure smoothness.
        // visitor::traverse(top_level_node, &mut type_check_context);
      }

      assert!(type_check_context.diagnostics.is_empty());
    }

    let llvm_context = inkwell::context::Context::create();
    let llvm_module = llvm_context.create_module(&qualifier.module_name);

    let mut lowering_context =
      lowering::LoweringContext::new(&type_cache, &cache, &llvm_context, &llvm_module);

    // // FIXME: Temporarily commented out.
    // // assert!(lint_context.diagnostics.is_empty());

    // // REVISE: Any way for better efficiency (less loops)?
    // // Once symbols are resolved, we can proceed to the other phases.
    for (qualifier, inner_ast) in &mut ast_map {
      // REVIEW: Must join package and module name for uniqueness?
      lowering_context.module_name = qualifier.module_name.clone();

      for top_level_node in inner_ast {
        // top_level_node.lower(&mut llvm_generator, &mut cache, false);
        lowering_context.dispatch(top_level_node);
      }
    }

    let llvm_module_string = llvm_module.print_to_string().to_string();

    if let Err(e) = llvm_module.verify() {
      println!("\nIn:\n======\n{}======", llvm_module_string);

      panic!(
        "... LLVM module verification failed: \n======\n{}\n======",
        e.to_string()
      );
    }

    llvm_module_string
  }

  #[test]
  fn integration_tests() {
    // REVISE: Unsafe unwrap.
    let tests_path = std::env::current_dir().unwrap().join("tests");
    let tests_output_path = tests_path.join("output");

    // REVIEW: Consider sorting to avoid fragile module output comparison test.
    // REVISE: Unsafe unwrap.
    let mut source_files = fs::read_dir(&tests_path.join("integration"))
      .unwrap()
      .map(|path| path.unwrap().path())
      .filter(|path| path.is_file() && path.extension().unwrap() == "ko")
      .collect::<Vec<_>>();

    // Sort files to ensure consistency, and avoid fragile module output comparison.
    source_files.sort();

    let mut sources = Vec::new();

    for source_file in &source_files {
      sources.push(load_test_file(source_file));
    }

    // TODO: Ensure both vectors are of same length, and zip iterators.
    // Read, lex, parse, perform name resolution (declarations)
    // and collect the AST (top-level nodes) from each source file.
    for (index, source_file) in source_files.iter().enumerate() {
      let source_file_name = source_file
        .file_stem()
        .unwrap()
        .to_str()
        .unwrap()
        .to_string();

      let qualifier = gecko::name_resolution::Qualifier {
        package_name: String::from("integration_tests"),
        // FIXME: File names need to conform to identifier rules.
        module_name: source_file_name.clone(),
      };

      let output_file_path = tests_output_path
        .join(source_file_name)
        .with_extension("ll");

      let output_file_contents = fs::read_to_string(output_file_path);

      assert!(output_file_contents.is_ok());

      assert_eq!(
        lower_file(&sources[index], qualifier).trim(),
        output_file_contents.unwrap().trim()
      );
    }
  }
}
