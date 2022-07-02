use crate::{ast, cache, llvm_lowering};

#[derive(Hash, PartialEq, Eq, Clone, Debug)]
pub enum SymbolKind {
  Definition,
  // REVISE: Can be more than that. Do not lie.
  /// A global type. Can be a struct, or enum.
  Type,
}

#[derive(Hash, PartialEq, Eq, Clone, Debug)]
pub struct Symbol {
  pub base_name: String,
  pub sub_name: Option<String>,
  pub kind: SymbolKind,
}

type Scope = std::collections::HashMap<Symbol, cache::BindingId>;

pub trait Resolve {
  fn declare(&self, _resolver: &mut NameResolver) {
    //
  }

  // REVIEW: Should this function should return a `Result<()>` in case that the resolution fails?
  fn resolve(&mut self, _resolver: &mut NameResolver, _cache: &mut cache::Cache) {
    //
  }
}

impl Resolve for ast::Type {
  fn resolve(&mut self, resolver: &mut NameResolver, cache: &mut cache::Cache) {
    match self {
      ast::Type::Stub(stub_type) => stub_type.resolve(resolver, cache),
      ast::Type::This(this_type) => this_type.resolve(resolver, cache),
      ast::Type::Pointer(pointee_type) => pointee_type.resolve(resolver, cache),
      ast::Type::Array(element_type, _) => element_type.resolve(resolver, cache),
      ast::Type::Struct(struct_type) => struct_type.resolve(resolver, cache),
      ast::Type::Function(function_type) => function_type.resolve(resolver, cache),
      // REVIEW: Are there any other types that may need to be resolved?
      _ => {}
    };
  }
}

impl Resolve for ast::NodeKind {
  // REVIEW: This `dispatch` may actually only apply for top-level nodes, so there might be room for simplification.

  fn declare(&self, resolver: &mut NameResolver) {
    crate::dispatch!(self, Resolve::declare, resolver);
  }

  fn resolve(&mut self, resolver: &mut NameResolver, cache: &mut cache::Cache) {
    crate::dispatch!(self, Resolve::resolve, resolver, cache);
  }
}

impl Resolve for ast::SizeofIntrinsic {
  fn resolve(&mut self, resolver: &mut NameResolver, cache: &mut cache::Cache) {
    self.ty.resolve(resolver, cache);
  }
}

impl Resolve for ast::Using {
  //
}

impl Resolve for ast::ParenthesesExpr {
  fn declare(&self, resolver: &mut NameResolver) {
    self.expr.kind.declare(resolver);
  }

  fn resolve(&mut self, resolver: &mut NameResolver, cache: &mut cache::Cache) {
    self.expr.kind.resolve(resolver, cache);
  }
}

impl Resolve for ast::Trait {
  fn declare(&self, _resolver: &mut NameResolver) {
    // REVIEW: Is there a need to declare symbol here?
    // resolver.declare_symbol((self.name.clone(), SymbolKind::Type), self.binding_id);
  }
}

impl Resolve for ast::ThisType {
  fn resolve(&mut self, resolver: &mut NameResolver, _cache: &mut cache::Cache) {
    if let Some(this_type_id) = resolver.current_struct_type_id {
      self.target_id = Some(this_type_id);
    } else {
      resolver.diagnostics.push(
        codespan_reporting::diagnostic::Diagnostic::error()
          .with_message("type `This` cannot be used outside of a struct implementation"),
      );
    }
  }
}

impl Resolve for ast::StructImpl {
  fn declare(&self, resolver: &mut NameResolver) {
    // REVIEW: Is there a need to bind this on the cache?

    resolver.push_scope();

    for method in &self.member_methods {
      method.declare(resolver);
    }

    // FIXME: Doesn't this just simply invalidate all previous methods' declarations?
    resolver.force_pop_scope();

    // REVIEW: Is this scope the correct one to declare the static methods?
    // ... What about inside the `impl`'s scope itself, so that member methods can
    // ... access static ones without the need for a qualifier?
    for static_method in &self.static_methods {
      static_method.declare(resolver);
    }
  }

  fn resolve(&mut self, resolver: &mut NameResolver, cache: &mut cache::Cache) {
    self.target_struct_pattern.resolve(resolver, cache);

    if let Some(trait_pattern) = &mut self.trait_pattern {
      trait_pattern.resolve(resolver, cache);
    }

    // REVIEW: We can't unwrap here because the lookup might have failed.
    // ... Is this done in other parts? Certain resolve methods depend on
    // ... other things being resolved already, this could be dangerous.
    let struct_type_id_result = self.target_struct_pattern.target_id;

    if let Some(struct_type_id) = struct_type_id_result {
      cache.add_struct_impl(
        struct_type_id,
        self
          .member_methods
          .iter()
          .map(|method| (method.binding_id, method.name.clone()))
          .collect::<Vec<_>>(),
      );

      resolver.current_struct_type_id = Some(struct_type_id);

      for member_method in &mut self.member_methods {
        member_method.resolve(resolver, cache);
      }

      // REVIEW: Is it okay to export it to the current struct type id?
      // ... If so, what purpose does it serve?
      for static_method in &mut self.static_methods {
        static_method.resolve(resolver, cache);
      }

      resolver.current_struct_type_id = None;
    }
  }
}

impl Resolve for ast::MemberAccess {
  fn resolve(&mut self, resolver: &mut NameResolver, cache: &mut cache::Cache) {
    self.base_expr.kind.resolve(resolver, cache);
  }
}

impl Resolve for ast::Closure {
  fn declare(&self, resolver: &mut NameResolver) {
    // TODO: Here, captures should be force-declared.
    // FIXME: The body is resolved within a virtual environment. This means that declarations from here may not be accessible. To solve this, perhaps we may virtualize all but the last scope (this declaration's body's scope).

    self.prototype.declare(resolver);
    self.body.declare(resolver);
  }

  fn resolve(&mut self, resolver: &mut NameResolver, cache: &mut cache::Cache) {
    // FIXME: Continue implementation.

    for (_index, capture) in self.captures.iter_mut().enumerate() {
      capture.1 = resolver.local_lookup_or_error(&Symbol {
        base_name: capture.0.clone(),
        sub_name: None,
        kind: SymbolKind::Definition,
      });

      // FIXME: Anything else needs to be done here?
    }

    // Cache the existing relative scopes, and create a new, empty
    // environment within the resolver, then restore the cached scopes
    // after the body has been resolved. This is done to encapsulate the
    // closure's environment.
    let relative_scopes_buffer = resolver.relative_scopes.clone();

    resolver.relative_scopes.clear();
    self.body.resolve(resolver, cache);

    // REVIEW: Should this closing of relative scopes occur
    // ... before or after the return type is possibly inferred?
    resolver.relative_scopes = relative_scopes_buffer;

    self.prototype.resolve(resolver, cache);
  }
}

impl Resolve for ast::TypeAlias {
  fn declare(&self, resolver: &mut NameResolver) {
    resolver.declare_symbol(
      Symbol {
        base_name: self.name.clone(),
        sub_name: None,
        kind: SymbolKind::Type,
      },
      self.binding_id,
    );
  }

  fn resolve(&mut self, resolver: &mut NameResolver, cache: &mut cache::Cache) {
    self.ty.resolve(resolver, cache);

    cache
      .symbols
      .insert(self.binding_id, ast::NodeKind::TypeAlias(self.clone()));
  }
}

// REVIEW: This might be getting too complicated. Maybe we should keep it simple in this case?
impl Resolve for ast::Pattern {
  fn resolve(&mut self, resolver: &mut NameResolver, _cache: &mut cache::Cache) {
    let symbol = Symbol {
      base_name: self.base_name.clone(),
      sub_name: self.sub_name.clone(),
      kind: self.symbol_kind.clone(),
    };

    if let Some(qualifier) = &self.qualifier {
      // REVISE: A bit misleading, since `lookup_or_error` returns `Option<>`.
      self.target_id = resolver.lookup(qualifier.clone(), &symbol);

      // TODO: Abstract and reuse error handling.
      if self.target_id.is_none() {
        resolver.diagnostics.push(
          codespan_reporting::diagnostic::Diagnostic::error().with_message(format!(
            "qualified symbol does not exist: {}::{}::{}",
            qualifier.package_name, qualifier.module_name, self.base_name
          )),
        );
      }

      return;
    }

    // REVISE: A bit misleading, since `lookup_or_error` returns `Option<>`.
    self.target_id = resolver.local_lookup_or_error(&symbol);
  }
}

impl Resolve for ast::IntrinsicCall {
  //
}

impl Resolve for ast::ExternStatic {
  fn declare(&self, resolver: &mut NameResolver) {
    resolver.declare_symbol(
      Symbol {
        base_name: self.name.clone(),
        sub_name: None,
        kind: SymbolKind::Definition,
      },
      self.binding_id,
    );
  }

  fn resolve(&mut self, resolver: &mut NameResolver, cache: &mut cache::Cache) {
    self.ty.resolve(resolver, cache);

    cache
      .symbols
      .insert(self.binding_id, ast::NodeKind::ExternStatic(self.clone()));
  }
}

impl Resolve for ast::StubType {
  fn resolve(&mut self, resolver: &mut NameResolver, cache: &mut cache::Cache) {
    self.pattern.resolve(resolver, cache);
  }
}

impl Resolve for ast::StructValue {
  fn resolve(&mut self, resolver: &mut NameResolver, cache: &mut cache::Cache) {
    // REVISE: A bit misleading, since `lookup_or_error` returns `Option<>`.
    self.target_id = resolver.local_lookup_or_error(&Symbol {
      base_name: self.struct_name.clone(),
      sub_name: None,
      kind: SymbolKind::Type,
    });

    if let Some(_target_id) = self.target_id {
      // TODO: Create a struct type based off the target Struct node.
      // self.ty = cache::get_node(target_id);
    }

    for field in &mut self.fields {
      field.kind.resolve(resolver, cache);
    }
  }
}

impl Resolve for ast::Prototype {
  fn declare(&self, resolver: &mut NameResolver) {
    if let Some(this_parameter) = &self.this_parameter {
      this_parameter.declare(resolver);
    }

    for parameter in &self.parameters {
      parameter.declare(resolver);
    }
  }

  fn resolve(&mut self, resolver: &mut NameResolver, cache: &mut cache::Cache) {
    for parameter in &mut self.parameters {
      parameter.resolve(resolver, cache);
    }

    if self.accepts_instance {
      self.instance_type_id = Some(resolver.current_struct_type_id.unwrap());

      self
        .this_parameter
        .as_mut()
        .unwrap()
        .resolve(resolver, cache);
    }

    // Resolve return type regardless, in case of an extern.
    if let Some(return_type) = &mut self.return_type_annotation {
      return_type.resolve(resolver, cache);
    }
  }
}

impl Resolve for ast::StructType {
  fn declare(&self, resolver: &mut NameResolver) {
    resolver.declare_symbol(
      Symbol {
        base_name: self.name.clone(),
        sub_name: None,
        kind: SymbolKind::Type,
      },
      self.binding_id,
    );
  }

  fn resolve(&mut self, resolver: &mut NameResolver, cache: &mut cache::Cache) {
    for field in &mut self.fields {
      field.1.resolve(resolver, cache);
    }

    cache
      .symbols
      .insert(self.binding_id, ast::NodeKind::StructType(self.clone()));
  }
}

impl Resolve for ast::FunctionType {
  fn resolve(&mut self, resolver: &mut NameResolver, cache: &mut cache::Cache) {
    self.return_type.resolve(resolver, cache);

    for parameter_type in &mut self.parameter_types {
      parameter_type.resolve(resolver, cache);
    }
  }
}

impl Resolve for ast::UnaryExpr {
  fn resolve(&mut self, resolver: &mut NameResolver, cache: &mut cache::Cache) {
    self.expr.kind.resolve(resolver, cache);
  }
}

impl Resolve for ast::Enum {
  fn declare(&self, resolver: &mut NameResolver) {
    resolver.declare_symbol(
      Symbol {
        base_name: self.name.clone(),
        sub_name: None,
        kind: SymbolKind::Type,
      },
      self.binding_id,
    );

    for variant in &self.variants {
      resolver.declare_symbol(
        Symbol {
          base_name: self.name.clone(),
          sub_name: Some(variant.0.clone()),
          kind: SymbolKind::Definition,
        },
        variant.1.clone(),
      );
    }
  }

  fn resolve(&mut self, _resolver: &mut NameResolver, cache: &mut cache::Cache) {
    cache
      .symbols
      .insert(self.binding_id, ast::NodeKind::Enum(self.clone()));
  }
}

impl Resolve for ast::AssignStmt {
  fn resolve(&mut self, resolver: &mut NameResolver, cache: &mut cache::Cache) {
    self.assignee_expr.kind.resolve(resolver, cache);
    self.value.kind.resolve(resolver, cache);
  }
}

impl Resolve for ast::ContinueStmt {
  //
}

impl Resolve for ast::IndexingExpr {
  fn declare(&self, resolver: &mut NameResolver) {
    self.index_expr.kind.declare(resolver);
  }

  fn resolve(&mut self, resolver: &mut NameResolver, cache: &mut cache::Cache) {
    self.index_expr.kind.resolve(resolver, cache);

    self.target_id = resolver.local_lookup_or_error(&Symbol {
      base_name: self.name.clone(),
      sub_name: None,
      kind: SymbolKind::Definition,
    });
  }
}

impl Resolve for ast::StaticArrayValue {
  // TODO: Do we need to declare the struct value's expressions?

  fn resolve(&mut self, resolver: &mut NameResolver, cache: &mut cache::Cache) {
    for element in &mut self.elements {
      element.kind.resolve(resolver, cache);
    }
  }
}

impl Resolve for ast::UnsafeExpr {
  fn declare(&self, resolver: &mut NameResolver) {
    self.0.kind.declare(resolver);
  }

  fn resolve(&mut self, resolver: &mut NameResolver, cache: &mut cache::Cache) {
    self.0.kind.resolve(resolver, cache);
  }
}

// BUG: Parameters with the same name on other functions are being resolved elsewhere.
// ... This is likely because of the current design of relative scopes. Fix this issue.
// ... (May be related to when memoizing or retrieving other functions, then not
// ... virtualizing their environment).
impl Resolve for ast::Parameter {
  fn declare(&self, resolver: &mut NameResolver) {
    resolver.declare_symbol(
      Symbol {
        base_name: self.name.clone(),
        sub_name: None,
        kind: SymbolKind::Definition,
      },
      self.binding_id,
    );
  }

  fn resolve(&mut self, resolver: &mut NameResolver, cache: &mut cache::Cache) {
    self.ty.resolve(resolver, cache);

    cache
      .symbols
      .insert(self.binding_id, ast::NodeKind::Parameter(self.clone()));
  }
}

impl Resolve for ast::Reference {
  fn resolve(&mut self, resolver: &mut NameResolver, cache: &mut cache::Cache) {
    self.pattern.resolve(resolver, cache);
  }
}

impl Resolve for ast::BreakStmt {
  //
}

impl Resolve for ast::LoopStmt {
  fn declare(&self, resolver: &mut NameResolver) {
    if let Some(condition) = &self.condition {
      condition.kind.declare(resolver);
    }

    self.body.declare(resolver);
  }

  fn resolve(&mut self, resolver: &mut NameResolver, cache: &mut cache::Cache) {
    if let Some(condition) = &mut self.condition {
      condition.kind.resolve(resolver, cache);
    }

    self.body.resolve(resolver, cache);
  }
}

impl Resolve for ast::IfExpr {
  fn declare(&self, resolver: &mut NameResolver) {
    self.condition.kind.declare(resolver);
    self.then_value.kind.declare(resolver);

    if let Some(else_block) = &self.else_value {
      else_block.kind.declare(resolver);
    }
  }

  fn resolve(&mut self, resolver: &mut NameResolver, cache: &mut cache::Cache) {
    self.condition.kind.resolve(resolver, cache);
    self.then_value.kind.resolve(resolver, cache);

    if let Some(else_block) = &mut self.else_value {
      else_block.kind.resolve(resolver, cache);
    }
  }
}

impl Resolve for ast::LetStmt {
  fn declare(&self, resolver: &mut NameResolver) {
    resolver.declare_symbol(
      Symbol {
        base_name: self.name.clone(),
        sub_name: None,
        kind: SymbolKind::Definition,
      },
      self.binding_id,
    );

    self.value.kind.declare(resolver);
  }

  fn resolve(&mut self, resolver: &mut NameResolver, cache: &mut cache::Cache) {
    // BUG: The problem seems to be occurring only when using let-statements. Investigate.
    // ... On the second iteration of the resolve step only! During cached nodes resolution.

    self.value.kind.resolve(resolver, cache);

    // REVIEW: Annotated type is not being resolved.

    cache
      .symbols
      .insert(self.binding_id, ast::NodeKind::LetStmt(self.clone()));
  }
}

impl Resolve for ast::ReturnStmt {
  fn declare(&self, resolver: &mut NameResolver) {
    if let Some(value) = &self.value {
      value.kind.declare(resolver);
    }
  }

  fn resolve(&mut self, resolver: &mut NameResolver, cache: &mut cache::Cache) {
    if let Some(value) = &mut self.value {
      value.kind.resolve(resolver, cache);
    }
  }
}

impl Resolve for ast::BlockExpr {
  fn declare(&self, resolver: &mut NameResolver) {
    resolver.push_scope();

    for statement in &self.statements {
      statement.kind.declare(resolver);
    }

    resolver.close_scope_tree(self.binding_id);
  }

  fn resolve(&mut self, resolver: &mut NameResolver, cache: &mut cache::Cache) {
    // BUG: Something's wrong when an if-expression is present, with a block as its `then` value. It won't resolve declarations.
    // BUG: Will this work as expected, or we might need to use a stack?
    let previous_block_binding_id = resolver.current_block_binding_id;

    resolver.current_block_binding_id = Some(self.binding_id);

    for statement in &mut self.statements {
      statement.kind.resolve(resolver, cache);
    }

    resolver.current_block_binding_id = previous_block_binding_id;
  }
}

impl Resolve for ast::Literal {
  //
}

impl Resolve for ast::Function {
  fn declare(&self, resolver: &mut NameResolver) {
    // BUG: Something is wrong with this scope tree. Consider adding tests for this API.
    // ... The `push_scope()` and `close_scope_tree()` lines where commented out. Once uncommented,
    // ... everything SEEMS to be working fine, but still need tests if there aren't any already, and
    // ... review of the code, and possibly integration tests.
    // Parameter scope.
    resolver.push_scope();

    self.prototype.declare(resolver);
    self.body.declare(resolver);

    // NOTE: The scope tree won't be overwritten by the block's, nor the
    // prototype's scope tree, instead they will be merged, as expected.
    resolver.close_scope_tree(self.binding_id);

    resolver.declare_symbol(
      // TODO: Cleanup.
      Symbol {
        base_name: if let Some(static_owner_name) = &self.static_owner_name {
          static_owner_name.clone()
        } else {
          self.name.clone()
        },
        sub_name: if self.static_owner_name.is_some() {
          Some(self.name.clone())
        } else {
          None
        },
        kind: SymbolKind::Definition,
      },
      self.binding_id,
    );
  }

  // REVIEW: This resolve step may need to be repeated for closure.
  fn resolve(&mut self, resolver: &mut NameResolver, cache: &mut cache::Cache) {
    // REVIEW: Do we need scope management here, for the prototype's parameters?
    self.prototype.resolve(resolver, cache);

    // Finally, after both the prototype and its return type have been resolved,
    // proceed to resolve the body.
    self.body.resolve(resolver, cache);

    cache
      .symbols
      .insert(self.binding_id, ast::NodeKind::Function(self.clone()));

    // BUG: This must be checked only within the initial package. Currently, the main function
    // ... can be defined elsewhere on its dependencies (even if they're libraries).
    // REVIEW: Should we have this check positioned here? Or should it be placed elsewhere?
    // ... Also, should the main function binding id be located under the cache?
    if self.name == llvm_lowering::MAIN_FUNCTION_NAME {
      if cache.main_function_id.is_some() {
        resolver.diagnostics.push(
          codespan_reporting::diagnostic::Diagnostic::error()
            .with_message("multiple main functions defined"),
        );
      } else {
        cache.main_function_id = Some(self.binding_id);
      }
    }
  }
}

impl Resolve for ast::ExternFunction {
  fn declare(&self, resolver: &mut NameResolver) {
    resolver.declare_symbol(
      Symbol {
        base_name: self.name.clone(),
        sub_name: None,
        kind: SymbolKind::Definition,
      },
      self.binding_id,
    );
  }

  fn resolve(&mut self, resolver: &mut NameResolver, cache: &mut cache::Cache) {
    self.prototype.resolve(resolver, cache);

    cache
      .symbols
      .insert(self.binding_id, ast::NodeKind::ExternFunction(self.clone()));
  }
}

impl Resolve for ast::CallExpr {
  fn declare(&self, resolver: &mut NameResolver) {
    // Declare any possible `Definition` nodes in the arguments
    // (such as inline closures, etc.).
    for argument in &self.arguments {
      argument.kind.declare(resolver);
    }
  }

  fn resolve(&mut self, resolver: &mut NameResolver, cache: &mut cache::Cache) {
    self.callee_expr.kind.resolve(resolver, cache);

    for argument in &mut self.arguments {
      argument.kind.resolve(resolver, cache);
    }
  }
}

impl Resolve for ast::InlineExprStmt {
  fn declare(&self, resolver: &mut NameResolver) {
    self.expr.kind.declare(resolver);
  }

  fn resolve(&mut self, resolver: &mut NameResolver, cache: &mut cache::Cache) {
    self.expr.kind.resolve(resolver, cache);
  }
}

impl Resolve for ast::BinaryExpr {
  fn declare(&self, resolver: &mut NameResolver) {
    self.left.kind.declare(resolver);
    self.right.kind.declare(resolver);
  }

  fn resolve(&mut self, resolver: &mut NameResolver, cache: &mut cache::Cache) {
    self.left.kind.resolve(resolver, cache);
    self.right.kind.resolve(resolver, cache);
  }
}

#[derive(Clone, PartialEq, Eq, Hash, Debug, PartialOrd, Ord)]
pub struct Qualifier {
  pub package_name: String,
  pub module_name: String,
}

#[derive(Clone)]
pub struct NameResolver {
  diagnostics: Vec<codespan_reporting::diagnostic::Diagnostic<usize>>,
  current_scope_qualifier: Option<Qualifier>,
  /// Contains the modules with their respective top-level definitions.
  global_scopes: std::collections::HashMap<Qualifier, Scope>,
  /// Contains volatile, relative scopes.
  ///
  /// Only used during the declare step. This is reset when the module changes,
  /// although by that time, all the relative scopes should have been popped automatically.
  relative_scopes: Vec<Scope>,
  /// A mapping of a scope's unique key to its own scope, and all visible parent
  /// relative scopes, excluding the global scope.
  scope_map: std::collections::HashMap<cache::BindingId, Vec<Scope>>,
  /// The unique id of the current block's scope. Used in the resolve step.
  current_block_binding_id: Option<cache::BindingId>,
  current_struct_type_id: Option<cache::BindingId>,
}

impl NameResolver {
  pub fn new(initial_module_qualifier: Qualifier) -> Self {
    let mut result = Self {
      diagnostics: Vec::new(),
      current_scope_qualifier: None,
      global_scopes: std::collections::HashMap::new(),
      relative_scopes: Vec::new(),
      scope_map: std::collections::HashMap::new(),
      current_block_binding_id: None,
      current_struct_type_id: None,
    };

    result.create_module(initial_module_qualifier);

    result
  }

  pub fn run(
    &mut self,
    ast_map: &mut std::collections::BTreeMap<Qualifier, Vec<ast::Node>>,
    cache: &mut cache::Cache,
  ) -> Vec<codespan_reporting::diagnostic::Diagnostic<usize>> {
    if ast_map.is_empty() {
      return Vec::new();
    }

    let mut name_resolver = NameResolver::new(ast_map.keys().next().unwrap().clone());

    // BUG: Cannot be processed linearly. Once an import is used, the whole corresponding
    // ... module must be recursively processed first!

    for (qualifier, ast) in ast_map.iter() {
      // REVIEW: Shouldn't the module be created before, on the Driver?
      name_resolver.create_module(qualifier.clone());
      name_resolver.current_scope_qualifier = Some(qualifier.clone());

      for node in ast.iter() {
        node.kind.declare(&mut name_resolver);
      }
    }

    for (qualifier, ast) in ast_map {
      name_resolver.current_scope_qualifier = Some(qualifier.clone());

      for node in ast {
        // FIXME: Need to set active module here. Since the ASTs are jumbled-up together,
        // ... an auxiliary map must be accepted in the parameters.
        node.kind.resolve(&mut name_resolver, cache);
      }
    }

    name_resolver.diagnostics
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
  fn declare_symbol(&mut self, symbol: Symbol, binding_id: cache::BindingId) -> bool {
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
    self.bind(symbol.clone(), binding_id);

    true
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
  fn close_scope_tree(&mut self, binding_id: cache::BindingId) {
    let mut scope_tree = vec![self.force_pop_scope()];

    // Clone the relative scope tree.
    scope_tree.extend(self.relative_scopes.iter().rev().cloned());

    // Append to the existing definition, if applicable.
    if self.scope_map.contains_key(&binding_id) {
      scope_tree.extend(self.scope_map.remove(&binding_id).unwrap());
    }

    self.scope_map.insert(binding_id, scope_tree);
  }

  /// Register a name on the last scope for name resolution lookups.
  ///
  /// If there are no relative scopes, the symbol is registered in the global scope.
  fn bind(&mut self, symbol: Symbol, binding_id: cache::BindingId) {
    self.get_current_scope().insert(symbol, binding_id);
  }

  /// Lookup a symbol in the global scope of a specific package and module.
  fn lookup(&mut self, qualifier: Qualifier, symbol: &Symbol) -> Option<cache::BindingId> {
    if !self.global_scopes.contains_key(&qualifier) {
      return None;
    }

    let global_scope = self.global_scopes.get(&qualifier).unwrap();

    if let Some(binding_id) = global_scope.get(&symbol) {
      return Some(binding_id.clone());
    }

    None
  }

  /// Lookup a symbol starting from the nearest scope, all the way to the global scope
  /// of the current module.
  fn local_lookup(&mut self, symbol: &Symbol) -> Option<cache::BindingId> {
    // If applicable, lookup on the relative scopes. This may not
    // be the case for when resolving global entities such as struct
    // types that reference other structs in their fields (in such case,
    // the relative scopes will be empty and the `current_block_binding_id`
    // buffer would be `None`).
    if let Some(current_block_binding_id) = self.current_block_binding_id {
      let scope_tree = self.scope_map.get(&current_block_binding_id).unwrap();

      // First, attempt to find the symbol in the relative scopes.
      for scope in scope_tree {
        if let Some(binding_id) = scope.get(&symbol) {
          return Some(binding_id.clone());
        }
      }
    }

    // REVISE: Unsafe unwrap.
    // Otherwise, attempt to find the symbol in the current module's global scope.
    self.lookup(self.current_scope_qualifier.clone().unwrap(), symbol)
  }

  fn local_lookup_or_error(&mut self, symbol: &Symbol) -> Option<cache::BindingId> {
    if let Some(binding_id) = self.local_lookup(symbol) {
      return Some(binding_id.clone());
    }

    // TODO: Include sub-name if available.
    self.diagnostics.push(
      codespan_reporting::diagnostic::Diagnostic::error()
        .with_message(format!("undefined reference to `{}`", symbol.base_name)),
    );

    None
  }

  fn current_scope_contains(&mut self, key: &Symbol) -> bool {
    self.get_current_scope().contains_key(key)
  }
}

// TODO: Add essential tests.
#[cfg(test)]
mod tests {
  use super::*;

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

  #[test]
  fn proper_initial_values() {
    let name_resolver = NameResolver::new(mock_qualifier());

    // TODO: Awaiting fix of module system.
    // assert!(name_resolver.current_scope_qualifier.is_none());
    assert!(name_resolver.relative_scopes.is_empty());
    assert!(name_resolver.scope_map.is_empty());
    // assert!(name_resolver.global_scopes.is_empty());
  }

  #[test]
  fn push_pop_scope() {
    let mut name_resolver = NameResolver::new(mock_qualifier());

    assert!(name_resolver.relative_scopes.is_empty());
    name_resolver.push_scope();
    assert_eq!(1, name_resolver.relative_scopes.len());
    name_resolver.force_pop_scope();
    assert!(name_resolver.relative_scopes.is_empty());
  }

  #[test]
  fn get_current_scope() {
    let mut name_resolver = NameResolver::new(mock_qualifier());

    name_resolver.get_current_scope();
  }

  #[test]
  fn current_scope_contains() {
    let mut name_resolver = NameResolver::new(mock_qualifier());
    let symbol = mock_symbol();

    assert!(!name_resolver.current_scope_contains(&symbol));
    name_resolver.bind(symbol.clone(), 0);
    assert!(name_resolver.current_scope_contains(&symbol));
  }

  #[test]
  fn declare_symbol() {
    let binding_id: cache::BindingId = 0;
    let symbol = mock_symbol();
    let mut name_resolver = NameResolver::new(mock_qualifier());

    assert!(name_resolver.declare_symbol(symbol.clone(), binding_id.clone()));
    assert!(name_resolver.diagnostics.is_empty());
    assert!(!name_resolver.declare_symbol(symbol.clone(), binding_id));
    assert_eq!(1, name_resolver.diagnostics.len());
    assert!(name_resolver.current_scope_contains(&symbol));
  }

  #[test]
  fn create_module() {
    let mut name_resolver = NameResolver::new(mock_qualifier());

    assert!(!name_resolver.create_module(mock_qualifier()));
    assert!(name_resolver.current_scope_qualifier.is_some());
  }

  #[test]
  fn lookup() {
    let mut name_resolver = NameResolver::new(mock_qualifier());
    let symbol = mock_symbol();

    assert!(name_resolver.lookup(mock_qualifier(), &symbol).is_none());
    name_resolver.bind(symbol.clone(), 0);
    assert!(name_resolver.lookup(mock_qualifier(), &symbol).is_some());
  }

  #[test]
  fn local_lookup() {
    let mut name_resolver = NameResolver::new(mock_qualifier());
    let symbol = mock_symbol();
    let binding_id: cache::BindingId = 0;

    // REVIEW: Ensure this is test is well-formed.
    assert!(name_resolver.local_lookup(&symbol).is_none());
    name_resolver.push_scope();
    name_resolver.bind(symbol.clone(), binding_id.clone());
    name_resolver.close_scope_tree(binding_id);
    name_resolver.current_block_binding_id = Some(binding_id);
    assert!(name_resolver.local_lookup(&symbol).is_some());
  }

  #[test]
  fn local_lookup_or_error() {
    let mut name_resolver = NameResolver::new(mock_qualifier());

    // REVIEW: Consider testing behavior of the function as well?
    assert!(name_resolver.diagnostics.is_empty());

    assert!(name_resolver
      .local_lookup_or_error(&mock_symbol())
      .is_none());

    assert_eq!(1, name_resolver.diagnostics.len());
  }

  #[test]
  fn close_scope_tree() {
    let mut name_resolver = NameResolver::new(mock_qualifier());

    name_resolver.push_scope();
    name_resolver.close_scope_tree(0);
    assert!(name_resolver.relative_scopes.is_empty());
    assert_eq!(1, name_resolver.scope_map.len());
  }
}
