use crate::{
  ast, cache, lowering,
  visitor::{self, AnalysisVisitor},
};

pub fn run(ast_map: &mut ast::AstMap, cache: &mut cache::Cache) -> Vec<ast::Diagnostic> {
  if ast_map.is_empty() {
    return Vec::new();
  }

  let mut name_res_decl = NameResDeclContext::new(ast_map.keys().next().unwrap().clone(), cache);
  // let mut name_resolver = NameResolver::new(ast_map.keys().next().unwrap().clone());

  // // BUG: Cannot be processed linearly. Once an import is used, the whole corresponding
  // // ... module must be recursively processed first!

  for (qualifier, ast) in ast_map.iter() {
    // REVIEW: Shouldn't the module be created before, on the Driver?
    name_res_decl.create_module(qualifier.clone());
    name_res_decl.current_scope_qualifier = Some(qualifier.clone());

    for node in ast.iter() {
      visitor::traverse(node, &mut name_res_decl);
    }
  }

  if !name_res_decl.diagnostics.is_empty() {
    return name_res_decl.diagnostics;
  }

  let mut name_res_link =
    NameResLinkContext::new(&name_res_decl.global_scopes, name_res_decl.scope_map, cache);

  for (qualifier, ast) in ast_map {
    name_res_link.current_scope_qualifier = Some(qualifier.clone());

    for node in ast {
      // FIXME: Need to set active module here. Since the ASTs are jumbled-up together,
      // ... an auxiliary map must be accepted in the parameters.
      // node.resolve(&mut name_resolver, cache);
      // name_res_link.dispatch(node);
      visitor::traverse(node, &mut name_res_link);
    }
  }

  name_res_link.diagnostics
}

#[derive(Hash, PartialEq, Eq, Clone, Debug)]
pub enum SymbolKind {
  Definition,
  Type,
}

#[derive(Hash, PartialEq, Eq, Clone, Debug)]
pub struct Symbol {
  pub base_name: String,
  pub sub_name: Option<String>,
  pub kind: SymbolKind,
}

pub type Scope = std::collections::HashMap<Symbol, cache::Id>;

#[derive(Clone, PartialEq, Eq, Hash, Debug, PartialOrd, Ord)]
pub struct Qualifier {
  pub package_name: String,
  pub module_name: String,
}

pub struct NameResDeclContext<'a> {
  /// A mapping of a scope's unique key to its own scope, and all visible parent
  /// relative scopes, excluding the global scope.
  pub scope_map: std::collections::HashMap<cache::Id, Vec<Scope>>,
  /// Contains the modules with their respective top-level definitions.
  pub global_scopes: std::collections::HashMap<Qualifier, Scope>,
  pub diagnostics: Vec<ast::Diagnostic>,
  cache: &'a mut cache::Cache,
  current_scope_qualifier: Option<Qualifier>,
  /// Contains volatile, relative scopes.
  ///
  /// Only used during the declare step. This is reset when the module changes,
  /// although by that time, all the relative scopes should have been popped automatically.
  relative_scopes: Vec<Scope>,
}

impl<'a> NameResDeclContext<'a> {
  pub fn new(initial_module_qualifier: Qualifier, cache: &'a mut cache::Cache) -> Self {
    let mut result = Self {
      cache,
      diagnostics: Vec::new(),
      current_scope_qualifier: None,
      global_scopes: std::collections::HashMap::new(),
      relative_scopes: Vec::new(),
      scope_map: std::collections::HashMap::new(),
    };

    result.create_module(initial_module_qualifier);

    result
  }

  /// Set per-file. A new global scope is created per-module.
  ///
  /// Return `false` if a module associated with the given global qualifier was
  /// already previously defined, or `true` if it was created.
  pub fn create_module(&mut self, qualifier: Qualifier) -> bool {
    if self.global_scopes.contains_key(&qualifier) {
      return false;
    }

    self.current_scope_qualifier = Some(qualifier.clone());

    self
      .global_scopes
      .insert(qualifier, std::collections::HashMap::new());

    self.relative_scopes.clear();

    true
  }

  // FIXME: What about registering on the cache? If this is implemented, there is no longer a need to register
  // ... the root nodes on the cache.
  /// Register a local symbol to a binding id in the current scope.
  ///
  /// Returns `false`, and creates an error diagnostic in the local diagnostic builder, if
  /// the symbol was already defined in the current scope, or `true` if it was successfully
  /// registered.
  fn declare_symbol(&mut self, symbol: Symbol, id: cache::Id) -> bool {
    // Check for existing definitions.
    if self.current_scope_contains(&symbol) {
      self.diagnostics.push(
        // TODO: Include sub-name if available.
        codespan_reporting::diagnostic::Diagnostic::error()
          .with_message(format!("re-definition of `{}`", symbol.base_name)),
      );

      // REVIEW: What about calling the child's declare function?
      return false;
    }

    // Bind the symbol to the current scope for name resolution lookup.
    self.bind(symbol, id);

    true
  }

  fn declare_node(&mut self, symbol: Symbol, id: cache::Id, node: ast::NodeKind) {
    self.cache.declarations.insert(id, node);

    self.declare_symbol(symbol, id);
  }

  /// Retrieve the last pushed relative scope, or if there are none,
  /// the global scope of the current module.
  ///
  /// This function assumes that the current scope qualifier has been set,
  /// which occurs when a module is created.
  fn get_current_scope(&mut self) -> &mut Scope {
    if self.relative_scopes.is_empty() {
      // REVISE: Relying on the assumption that at least a single module was created.
      return self
        .global_scopes
        .get_mut(self.current_scope_qualifier.as_ref().unwrap())
        .unwrap();
    }

    self.relative_scopes.last_mut().unwrap()
  }

  fn push_scope(&mut self) {
    self.relative_scopes.push(std::collections::HashMap::new());
  }

  /// Pop the last scope off the relatives scopes stack, and return it.
  ///
  /// Will panic if there are no relative scopes.
  fn force_pop_scope(&mut self) -> Scope {
    self.relative_scopes.pop().unwrap()
  }

  /// Force-pop the last scope off the relatives scopes stack, and create
  /// a scope tree. This tree will then be inserted into the scope map.
  ///
  /// If an entry with the same unique id already exists, the scope tree will
  /// be appended onto the existing definition.
  fn finish_scope_tree(&mut self, id: cache::Id) {
    let mut scope_tree = vec![self.force_pop_scope()];

    // Clone the relative scope tree.
    scope_tree.extend(self.relative_scopes.iter().rev().cloned());

    // Append to the existing definition, if applicable.
    if self.scope_map.contains_key(&id) {
      scope_tree.extend(self.scope_map.remove(&id).unwrap());
    }

    self.scope_map.insert(id, scope_tree);
  }

  /// Register a name on the last scope for name resolution lookups.
  ///
  /// If there are no relative scopes, the symbol is registered in the global scope.
  fn bind(&mut self, symbol: Symbol, id: cache::Id) {
    self.get_current_scope().insert(symbol, id);
  }

  fn current_scope_contains(&mut self, key: &Symbol) -> bool {
    self.get_current_scope().contains_key(key)
  }
}

impl<'a> AnalysisVisitor for NameResDeclContext<'a> {
  fn before_dispatch(&mut self, node: &ast::NodeKind) {
    // TODO: There might be a way to simplify this (macro or trait?).
    match node {
      ast::NodeKind::Function(function) => {
        self.cache.declarations.insert(
          function.id,
          ast::NodeKind::Function(std::rc::Rc::clone(&function)),
        );
      }
      ast::NodeKind::ExternFunction(extern_fn) => {
        self.cache.declarations.insert(
          extern_fn.id,
          ast::NodeKind::ExternFunction(std::rc::Rc::clone(&extern_fn)),
        );
      }
      ast::NodeKind::ExternStatic(extern_static) => {
        self.cache.declarations.insert(
          extern_static.id,
          ast::NodeKind::ExternStatic(std::rc::Rc::clone(&extern_static)),
        );
      }
      ast::NodeKind::Enum(enum_) => {
        self
          .cache
          .declarations
          .insert(enum_.id, ast::NodeKind::Enum(std::rc::Rc::clone(&enum_)));
      }
      ast::NodeKind::TypeAlias(type_alias) => {
        self.cache.declarations.insert(
          type_alias.id,
          ast::NodeKind::TypeAlias(std::rc::Rc::clone(&type_alias)),
        );
      }

      ast::NodeKind::Struct(struct_) => {
        self.cache.declarations.insert(
          struct_.id,
          ast::NodeKind::Struct(std::rc::Rc::clone(&struct_)),
        );
      }
      // NOTE: Bindings are not top-level, however they statements, part of blocks.
      // This means that they are registered as `ast::NodeKind`s, under blocks. They
      // must be dispatched.
      ast::NodeKind::BindingStmt(binding_stmt) => {
        self.cache.declarations.insert(
          binding_stmt.id,
          ast::NodeKind::BindingStmt(std::rc::Rc::clone(&binding_stmt)),
        );
      }
      _ => {}
    }
  }

  fn visit_extern_function(&mut self, extern_fn: &ast::ExternFunction) {
    self.declare_symbol(
      Symbol {
        base_name: extern_fn.name.clone(),
        sub_name: None,
        kind: SymbolKind::Definition,
      },
      extern_fn.id,
    );
  }

  fn visit_parameter(&mut self, parameter: &ast::Parameter) {
    self.declare_symbol(
      Symbol {
        base_name: parameter.name.clone(),
        sub_name: None,
        kind: SymbolKind::Definition,
      },
      parameter.id,
    );
  }

  fn enter_function(&mut self, function: &ast::Function) {
    // BUG: Something is wrong with this scope tree. Consider adding tests for this API.
    // ... The `push_scope()` and `close_scope_tree()` lines where commented out. Once uncommented,
    // ... everything SEEMS to be working fine, but still need tests if there aren't any already, and
    // ... review of the code, and possibly integration tests.
    // Parameter scope.
    self.push_scope();

    // REVIEW: Perhaps make generics their own node?
    if let Some(generics) = &function.generics {
      for generic_parameter in &generics.parameters {
        self.declare_symbol(
          Symbol {
            base_name: generic_parameter.clone(),
            sub_name: None,
            kind: SymbolKind::Type,
          },
          // FIXME: Temporary cache id.
          100,
        );
      }
    }

    // TODO: Cleanup.
    self.declare_symbol(
      Symbol {
        base_name: if let Some(static_owner_name) = &function.static_owner_name {
          static_owner_name.clone()
        } else {
          function.name.clone()
        },
        sub_name: if function.static_owner_name.is_some() {
          Some(function.name.clone())
        } else {
          None
        },
        kind: SymbolKind::Definition,
      },
      function.id,
    );
  }

  fn exit_function(&mut self, function: &ast::Function) -> () {
    // NOTE: The scope tree won't be overwritten by the block's, nor the
    // signature's scope tree, instead they will be merged, as expected.
    self.finish_scope_tree(function.id);
  }

  fn enter_block_expr(&mut self, _block: &ast::BlockExpr) {
    self.push_scope();
  }

  fn exit_block_expr(&mut self, block: &ast::BlockExpr) -> () {
    self.finish_scope_tree(block.id);
  }

  fn visit_binding_stmt(&mut self, binding_stmt: &ast::BindingStmt) {
    self.declare_symbol(
      Symbol {
        base_name: binding_stmt.name.clone(),
        sub_name: None,
        kind: SymbolKind::Definition,
      },
      binding_stmt.id,
    );
  }

  fn visit_enum(&mut self, enum_: &ast::Enum) {
    self.declare_symbol(
      Symbol {
        base_name: enum_.name.clone(),
        sub_name: None,
        kind: SymbolKind::Type,
      },
      enum_.id,
    );

    for variant in &enum_.variants {
      self.declare_symbol(
        Symbol {
          base_name: enum_.name.clone(),
          sub_name: Some(variant.0.clone()),
          kind: SymbolKind::Definition,
        },
        variant.1.to_owned(),
      );
    }
  }

  fn visit_struct(&mut self, struct_type: &ast::Struct) {
    self.declare_symbol(
      Symbol {
        base_name: struct_type.name.clone(),
        sub_name: None,
        kind: SymbolKind::Type,
      },
      struct_type.id,
    );
  }

  fn visit_extern_static(&mut self, extern_static: &ast::ExternStatic) {
    self.declare_symbol(
      Symbol {
        base_name: extern_static.name.clone(),
        sub_name: None,
        kind: SymbolKind::Definition,
      },
      extern_static.id,
    );
  }

  fn visit_type_alias(&mut self, type_alias: &ast::TypeAlias) {
    self.declare_symbol(
      Symbol {
        base_name: type_alias.name.clone(),
        sub_name: None,
        kind: SymbolKind::Type,
      },
      type_alias.id,
    );
  }

  fn enter_struct_impl(&mut self, _struct_impl: &ast::StructImpl) {
    // REVIEW: Is there a need to bind this on the cache?

    // self.push_scope();

    // for method in &_struct_impl.member_methods {
    //   method.declare(resolver);
    // }

    // REVIEW: Is this scope the correct one to declare the static methods?
    // ... What about inside the `impl`'s scope itself, so that member methods can
    // ... access static ones without the need for a qualifier?
    // for static_method in &_struct_impl.static_methods {
    //   static_method.declare(resolver);
    // }
  }

  fn exit_struct_impl(&mut self, _struct_impl: &ast::StructImpl) -> () {
    // FIXME: Doesn't this just simply invalidate all previous methods' declarations?
    // self.force_pop_scope();
  }

  fn visit_trait(&mut self, _trait: &ast::Trait) {
    // REVIEW: Is there a need to declare symbol here?
    // resolver.declare_symbol((self.name.clone(), SymbolKind::Type), self.id);
  }
}

pub struct NameResLinkContext<'a> {
  /// A mapping of a scope's unique key to its own scope, and all visible parent
  /// relative scopes, excluding the global scope.
  scope_map: std::collections::HashMap<cache::Id, Vec<Scope>>,
  cache: &'a mut cache::Cache,
  current_scope_qualifier: Option<Qualifier>,
  current_struct_type_id: Option<cache::Id>,
  // REVIEW: If we can get rid of these flags, we may possibly use the traverse method instead
  // ... of manual visitation.
  /// The unique id of the current block's scope. Used in the resolve step.
  block_id_stack: Vec<cache::Id>,
  /// Contains the modules with their respective top-level definitions.
  global_scopes: &'a std::collections::HashMap<Qualifier, Scope>,
  pub diagnostics: Vec<ast::Diagnostic>,
}

impl<'a> NameResLinkContext<'a> {
  pub fn new(
    global_scopes: &'a std::collections::HashMap<Qualifier, Scope>,
    scope_map: std::collections::HashMap<cache::Id, Vec<Scope>>,
    cache: &'a mut cache::Cache,
  ) -> Self {
    Self {
      scope_map,
      cache,
      current_scope_qualifier: None,
      current_struct_type_id: None,
      block_id_stack: Vec::new(),
      global_scopes,
      diagnostics: vec![],
    }
  }
}

impl<'a> NameResLinkContext<'a> {
  /// Lookup a symbol in the global scope of a specific package and module.
  fn lookup(&mut self, qualifier: Qualifier, symbol: &Symbol) -> Option<cache::Id> {
    if !self.global_scopes.contains_key(&qualifier) {
      return None;
    }

    let global_scope = self.global_scopes.get(&qualifier).unwrap();

    if let Some(id) = global_scope.get(&symbol) {
      return Some(id.clone());
    }

    None
  }

  /// Lookup a symbol starting from the nearest scope, all the way to the global scope
  /// of the current module.
  fn local_lookup(&mut self, symbol: &Symbol) -> Option<cache::Id> {
    // If applicable, lookup on the relative scopes. This may not
    // be the case for when resolving global entities such as struct
    // types that reference other structs in their fields (in such case,
    // the relative scopes will be empty and the `current_block_id`
    // buffer would be `None`).
    if let Some(current_block_id) = self.block_id_stack.last() {
      let scope_tree = self.scope_map.get(&current_block_id).unwrap();

      // First, attempt to find the symbol in the relative scopes.
      for scope in scope_tree {
        if let Some(id) = scope.get(&symbol) {
          return Some(id.clone());
        }
      }
    }

    // REVISE: Unsafe unwrap.
    // Otherwise, attempt to find the symbol in the current module's global scope.
    self.lookup(self.current_scope_qualifier.clone().unwrap(), symbol)
  }

  fn local_lookup_or_error(&mut self, symbol: &Symbol) -> Option<cache::Id> {
    if let Some(id) = self.local_lookup(symbol) {
      return Some(id.clone());
    }

    // TODO: Include sub-name if available.
    self.diagnostics.push(
      codespan_reporting::diagnostic::Diagnostic::error()
        .with_message(format!("undefined reference to `{}`", symbol.base_name)),
    );

    None
  }
}

impl<'a> AnalysisVisitor for NameResLinkContext<'a> {
  fn visit_pattern(&mut self, pattern: &ast::Pattern) {
    let symbol = Symbol {
      base_name: pattern.base_name.clone(),
      sub_name: pattern.sub_name.clone(),
      kind: pattern.symbol_kind.clone(),
    };

    if let Some(qualifier) = &pattern.qualifier {
      // TODO: Abstract and reuse error handling.
      if self.lookup(qualifier.clone(), &symbol).is_none() {
        self.diagnostics.push(
          codespan_reporting::diagnostic::Diagnostic::error().with_message(format!(
            "qualified symbol does not exist: {}::{}::{}",
            qualifier.package_name, qualifier.module_name, pattern.base_name
          )),
        );
      }

      return;
    }

    // REVISE: A bit misleading, since `lookup_or_error` returns `Option<>`.
    self.local_lookup_or_error(&symbol).map(|target_id| {
      self.cache.links.insert(pattern.link_id, target_id);
    });
  }

  fn enter_function(&mut self, function: &ast::Function) {
    // BUG: This must be checked only within the initial package. Currently, the main function
    // ... can be defined elsewhere on its dependencies (even if they're libraries).
    // REVIEW: Should we have this check positioned here? Or should it be placed elsewhere?
    // ... Also, should the main function binding id be located under the cache?
    if function.name == lowering::MAIN_FUNCTION_NAME {
      if self.cache.main_function_id.is_some() {
        self.diagnostics.push(
          codespan_reporting::diagnostic::Diagnostic::error()
            .with_message("multiple main functions defined"),
        );
      } else {
        self.cache.main_function_id = Some(function.id);
      }
    }
  }

  fn enter_block_expr(&mut self, block: &ast::BlockExpr) {
    // BUG: Something's wrong when an if-expression is present, with a block as its `then` value. It won't resolve declarations.
    self.block_id_stack.push(block.id);
  }

  fn exit_block_expr(&mut self, _block: &ast::BlockExpr) {
    self.block_id_stack.pop();
  }

  fn visit_signature(&mut self, signature: &ast::Signature) {
    if let Some(instance_type_id) = &signature.instance_type_id {
      self.cache.links.insert(
        instance_type_id.to_owned(),
        self.current_struct_type_id.unwrap(),
      );
    }

    for parameter in &signature.parameters {
      self.cache.declarations.insert(
        parameter.id,
        ast::NodeKind::Parameter(std::rc::Rc::clone(parameter)),
      );
    }
  }

  fn visit_struct_value(&mut self, struct_value: &ast::StructValue) {
    self
      .local_lookup_or_error(&Symbol {
        base_name: struct_value.struct_name.clone(),
        sub_name: None,
        kind: SymbolKind::Type,
      })
      .map(|target_id| {
        self.cache.links.insert(struct_value.target_id, target_id);
      });
  }

  fn visit_closure(&mut self, _closure: &ast::Closure) {
    // FIXME: Continue implementation.

    // for (_index, capture) in closure.captures.iter().enumerate() {
    //   capture.1 = self.local_lookup_or_error(&Symbol {
    //     base_name: capture.0.clone(),
    //     sub_name: None,
    //     kind: SymbolKind::Definition,
    //   });

    //   // FIXME: Anything else needs to be done here?
    // }

    // Cache the existing relative scopes, and create a new, empty
    // environment within the resolver, then restore the cached scopes
    // after the body has been resolved. This is done to encapsulate the
    // closure's environment.
    // let relative_scopes_buffer = resolver.relative_scopes.clone();

    // resolver.relative_scopes.clear();
    // _closure.body.resolve(resolver, cache);

    // REVIEW: Should this closing of relative scopes occur
    // ... before or after the return type is possibly inferred?
    // resolver.relative_scopes = relative_scopes_buffer;

    // _closure.signature.resolve(resolver, cache);

    // cache
    //   .symbols
    //   .insert(self.id, ast::NodeKind::Closure(self.clone()));
  }

  fn enter_struct_impl(&mut self, struct_impl: &ast::StructImpl) {
    self.current_struct_type_id = Some(struct_impl.target_struct_pattern.link_id);

    self.cache.add_struct_impl(
      struct_impl.target_struct_pattern.link_id,
      struct_impl
        .member_methods
        .iter()
        .map(|method| (method.id, method.name.clone()))
        .collect::<Vec<_>>(),
    );
  }

  fn exit_struct_impl(&mut self, _struct_impl: &ast::StructImpl) {
    self.current_struct_type_id = None;
  }

  // fn visit_this_type() {
  //   if let Some(this_type_id) = resolver.current_struct_type_id {
  //     self.target_id = Some(this_type_id);
  //   } else {
  //     resolver.diagnostics.push(
  //       codespan_reporting::diagnostic::Diagnostic::error()
  //         .with_message("type `This` cannot be used outside of a struct implementation"),
  //     );
  //   }
  // }

  // fn visit_type() {
  //   match self {
  //     ast::Type::Stub(stub_type) => stub_type.resolve(resolver, cache),
  //     ast::Type::This(this_type) => this_type.resolve(resolver, cache),
  //     ast::Type::Pointer(pointee_type) => pointee_type.resolve(resolver, cache),
  //     ast::Type::Array(element_type, _) => element_type.resolve(resolver, cache),
  //     ast::Type::Struct(struct_type) => struct_type.resolve(resolver, cache),
  //     ast::Type::Function(function_type) => function_type.resolve(resolver, cache),
  //     // REVIEW: Are there any other types that may need to be resolved?
  //     _ => {}
  //   };
  // }
}

// TODO: Add essential tests.
#[cfg(test)]
mod tests {
  use super::*;
  use pretty_assertions::{assert_eq, assert_ne};

  fn mock_qualifier() -> Qualifier {
    Qualifier {
      package_name: String::from("test_package"),
      module_name: String::from("test_module"),
    }
  }

  fn mock_symbol() -> Symbol {
    Symbol {
      base_name: String::from("test"),
      sub_name: None,
      kind: SymbolKind::Definition,
    }
  }

  // #[test]
  // fn proper_initial_values() {
  //   let name_resolver = NameResolver::new(mock_qualifier());

  //   // TODO: Awaiting fix of module system.
  //   // assert!(name_resolver.current_scope_qualifier.is_none());
  //   assert!(name_resolver.relative_scopes.is_empty());
  //   assert!(name_resolver.scope_map.is_empty());
  //   // assert!(name_resolver.global_scopes.is_empty());
  // }

  // #[test]
  // fn push_pop_scope() {
  //   let mut name_resolver = NameResolver::new(mock_qualifier());

  //   assert!(name_resolver.relative_scopes.is_empty());
  //   name_resolver.push_scope();
  //   assert_eq!(1, name_resolver.relative_scopes.len());
  //   name_resolver.force_pop_scope();
  //   assert!(name_resolver.relative_scopes.is_empty());
  // }

  // #[test]
  // fn get_current_scope() {
  //   let mut name_resolver = NameResolver::new(mock_qualifier());

  //   name_resolver.get_current_scope();
  // }

  // #[test]
  // fn current_scope_contains() {
  //   let mut name_resolver = NameResolver::new(mock_qualifier());
  //   let symbol = mock_symbol();

  //   assert!(!name_resolver.current_scope_contains(&symbol));
  //   name_resolver.bind(symbol.clone(), 0);
  //   assert!(name_resolver.current_scope_contains(&symbol));
  // }

  // #[test]
  // fn declare_symbol() {
  //   let id: cache::Id = 0;
  //   let symbol = mock_symbol();
  //   let mut name_resolver = NameResolver::new(mock_qualifier());

  //   assert!(name_resolver.declare_symbol(symbol.clone(), id.clone()));
  //   assert!(name_resolver.diagnostics.is_empty());
  //   assert!(!name_resolver.declare_symbol(symbol.clone(), id));
  //   assert_eq!(1, name_resolver.diagnostics.len());
  //   assert!(name_resolver.current_scope_contains(&symbol));
  // }

  // #[test]
  // fn create_module() {
  //   let mut name_resolver = NameResolver::new(mock_qualifier());

  //   assert!(!name_resolver.create_module(mock_qualifier()));
  //   assert!(name_resolver.current_scope_qualifier.is_some());
  // }

  // #[test]
  // fn lookup() {
  //   let mut name_resolver = NameResolver::new(mock_qualifier());
  //   let symbol = mock_symbol();

  //   assert!(name_resolver.lookup(mock_qualifier(), &symbol).is_none());
  //   name_resolver.bind(symbol.clone(), 0);
  //   assert!(name_resolver.lookup(mock_qualifier(), &symbol).is_some());
  // }

  // #[test]
  // fn local_lookup() {
  //   let mut name_resolver = NameResolver::new(mock_qualifier());
  //   let symbol = mock_symbol();
  //   let id: cache::Id = 0;

  //   // REVIEW: Ensure this is test is well-formed.
  //   assert!(name_resolver.local_lookup(&symbol).is_none());
  //   name_resolver.push_scope();
  //   name_resolver.bind(symbol.clone(), id.clone());
  //   name_resolver.close_scope_tree(id);
  //   name_resolver.current_block_id = Some(id);
  //   assert!(name_resolver.local_lookup(&symbol).is_some());
  // }

  // #[test]
  // fn local_lookup_or_error() {
  //   let mut name_resolver = NameResolver::new(mock_qualifier());

  //   // REVIEW: Consider testing behavior of the function as well?
  //   assert!(name_resolver.diagnostics.is_empty());

  //   assert!(name_resolver
  //     .local_lookup_or_error(&mock_symbol())
  //     .is_none());

  //   assert_eq!(1, name_resolver.diagnostics.len());
  // }

  // #[test]
  // fn close_scope_tree() {
  //   let mut name_resolver = NameResolver::new(mock_qualifier());

  //   name_resolver.push_scope();
  //   name_resolver.close_scope_tree(0);
  //   assert!(name_resolver.relative_scopes.is_empty());
  //   assert_eq!(1, name_resolver.scope_map.len());
  // }
}
