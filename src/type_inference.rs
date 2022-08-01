use crate::{ast, cache, visitor::AnalysisVisitor};

pub type TypeVariableId = usize;
pub type TypeCache = std::collections::HashMap<cache::Id, ast::Type>;
type TypeConstraint = (ast::Type, ast::Type);

pub fn run(ast_map: &ast::AstMap, cache: &cache::Cache) -> (Vec<ast::Diagnostic>, TypeCache) {
  let mut type_inference_ctx = TypeInferenceContext::new(cache);

  for inner_ast in ast_map.values() {
    for top_level_node in inner_ast {
      let initial_expected_type = type_inference_ctx.create_type_variable();

      // visitor::traverse(&top_level_node, &mut type_inference_ctx);
      type_inference_ctx.infer_and_constrain(initial_expected_type, &top_level_node);
    }
  }

  type_inference_ctx.solve_constraints();

  (
    type_inference_ctx.diagnostics,
    type_inference_ctx.type_cache,
  )
}

pub struct TypeInferenceContext<'a> {
  pub diagnostics: Vec<ast::Diagnostic>,
  // TODO: This doc. is outdated. It is not created during parsing. Instead,
  // ... it is created during type inference. If the type hint of something is `None`,
  // ... a fresh variable type is created for it here.
  /// A map from a type variable's (TODO: Or actually, a node's id) id to a type.
  ///
  /// This serves as a buffer for type inference to occur. It is
  /// populated during parsing, when type variables are created, and
  /// it also is scope-less/context-free.
  substitutions: std::collections::HashMap<cache::Id, ast::Type>,
  cache: &'a cache::Cache,
  // REVIEW: We can only store types of nodes that have ids (e.g. bindings, parameters, etc.).
  /// A mapping from a node to its most recent type.
  ///
  /// This is needed because analysis passes cannot mutate nodes.
  type_cache: TypeCache,
  /// Constraints are expectations, or hints, of equality between a pair of types.
  ///
  /// They are first gathered, then the unification algorithm is performed to solve types, at
  /// the last step of type inference.
  constraints: Vec<TypeConstraint>,
  current_function_id: Option<cache::Id>,
}

impl<'a> TypeInferenceContext<'a> {
  pub fn new(cache: &'a cache::Cache) -> Self {
    Self {
      diagnostics: Vec::new(),
      substitutions: std::collections::HashMap::new(),
      cache,
      type_cache: TypeCache::new(),
      constraints: Vec::new(),
      current_function_id: None,
    }
  }

  // REVISE: Avoid excessive cloning.
  /// Solves the constraints by performing a unification algorithm.
  ///
  /// This serves as one step of the type inference substitution algorithm.
  /// This also checks for compatibility among types. If they are compatible,
  /// they will be inserted into the substitution map.
  fn unify(&mut self, type_a: &ast::Type, type_b: &ast::Type) {
    // TODO: Cleanup code. Perhaps expand it to not be a big match statement?
    match (type_a, type_b) {
      // TODO: Missing type constructor support.
      // If both sides are the same type variable, do nothing.
      (ast::Type::Variable(id_a), ast::Type::Variable(id_b)) if id_a == id_b => {}
      // If one of the types is a type variable that’s bound in the substitution,
      // use unify with that type instead.
      (ast::Type::Variable(id), _)
        if {
          let access = self.substitutions.get(id);

          // REVIEW: Here we manually added the `.is_some()` check. Verify this is as expected.
          access.is_some() && access != Some(&ast::Type::Variable(*id))
        } =>
      {
        self.unify(&self.substitutions.get(id).unwrap().clone(), type_b)
      }
      (_, ast::Type::Variable(id))
        if {
          let access = self.substitutions.get(id);

          // REVIEW: Here we manually added the `.is_some()` check. Verify this is as expected.
          access.is_some() && access != Some(&ast::Type::Variable(*id))
        } =>
      {
        self.unify(type_a, &self.substitutions.get(id).unwrap().clone())
      }
      // Otherwise, if one of the types is an unbound type variable, bind it to the
      // other type. Remember to do an occurs check to avoid constructing infinite types.
      (ast::Type::Variable(id_a), _) => {
        // REVISE: Proper error handling.
        assert!(!self.occurs_in(id_a.to_owned(), &type_b));

        self.substitutions.insert(*id_a, type_b.clone());
      }
      (_, ast::Type::Variable(id_b)) => {
        // REVISE: Proper error handling.
        assert!(!self.occurs_in(id_b.to_owned(), &type_a));

        self.substitutions.insert(*id_b, type_a.clone());
      }
      // (constructor_a, constructor_b) if constructor_a.is_a_constructor() && constructor_b.is_a_constructor() => {

      // }
      (ast::Type::Constructor(kind_a, generics_a), ast::Type::Constructor(kind_b, generics_b)) => {
        // Technical info: https://en.wikipedia.org/wiki/Type_inference#Hindley%E2%80%93Milner_type_inference_algorithm
        // See also in the same page: Higher-order types?
        // case (TConstructor(name1, generics1), TConstructor(name2, generics2)) =>
        //       if(name1 != name2 || generics1.size != generics2.size) {
        //           throw TypeError("Type mismatch: " + substitute(t1) + " vs. " + substitute(t2))
        //       }
        //       for((t1, t2) <- generics1.zip(generics2)) unify(t1, t2)

        // REVISE: Proper error handling.

        assert!(match kind_a {
          ast::TypeConstructorKind::StaticIndexable(_) =>
          // REVIEW: Is it okay to skip size comparison?
            matches!(kind_b, ast::TypeConstructorKind::StaticIndexable(_)),
          _ => kind_a == kind_b,
        });

        assert!(generics_a.len() == generics_b.len());

        for (type_a, type_b) in generics_a.iter().zip(generics_b.iter()) {
          self.unify(type_a, type_b);
        }
      }
      _ => {}
    }
  }

  /// Recursively check if a type variable index occurs in
  /// a type.
  ///
  /// For this to be `true`, the type in question must be a type variable.
  /// Any other type will yield `false`.
  fn occurs_in(&self, index_id: usize, ty: &ast::Type) -> bool {
    match ty {
      ast::Type::Variable(id)
        if self.substitutions.get(id).unwrap() != &ast::Type::Variable(id.to_owned()) =>
      {
        self.occurs_in(index_id, &self.substitutions.get(id).unwrap())
      }
      // REVIEW: Will this compare the underlying values or the addresses?
      ast::Type::Variable(id) => id == &index_id,
      ast::Type::Constructor(_, generics) => generics
        .into_iter()
        .any(|generic| self.occurs_in(index_id, generic)),
      _ => false,
    }
  }

  // TODO: This is the same thing as `node.unification`, but it assumed nodes can be mutated as in object-oriented languages.
  /// Solves constraints by invoking and performing the unification
  /// algorithm.
  ///
  /// This occurs after all the constraints have been added,
  /// and is the last step for Hindley-Milner type inference.
  ///
  /// This will also update the type cache with the resulting types
  /// by performing substitution.
  fn solve_constraints(&mut self) {
    // REVIEW: What if we have conflicting constraints? Say, we have different calls with different types to the same function?
    // ... Or if the parameters are constrained to be something, yet the arguments are constrained to be different?
    // REVIEW: Any way to avoid cloning?
    for constrain in self.constraints.clone() {
      self.unify(&constrain.0, &constrain.1);
    }

    // REVISE: Avoid cloning.
    for (id, ty) in &self.type_cache.clone() {
      self.type_cache.insert(*id, self.substitute(ty.clone()));
    }

    dbg!(self.substitutions.clone());

    self.constraints.clear();
  }

  // REVIEW: Isn't this equivalent (or should be) to the `try_downgrade` function?
  // ... Or maybe that might mess up inner workings during unification, and instead leave it
  // ... last to the specialized `try_downgrade` function?
  /// Substitute a type variable with its non-variable type (if defined).
  ///
  /// If the substitution is not defined, the same type is returned. This
  /// function will recursively substitute type variables, until a non-variable
  /// type is found.
  pub fn substitute(&self, ty: ast::Type) -> ast::Type {
    match &ty {
      ast::Type::Variable(id)
        if self.substitutions.get(id) != Some(&ast::Type::Variable(id.to_owned())) =>
      {
        // REVIEW: Unsafe unwrap?
        self.substitute(self.substitutions.get(id).unwrap().clone())
      }
      // ast::Type::Constructor(kind, generics) => ast::Type::Constructor(
      //   kind.clone(),
      //   generics
      //     .into_iter()
      //     .map(|generic| self.substitute(generic.clone()))
      //     .collect(),
      // ),
      // REVIEW: Is this correct? Or is the current one?
      ast::Type::Constructor(kind, generics) => match &kind {
        ast::TypeConstructorKind::StaticIndexable(size) => {
          ast::Type::StaticIndexable(Box::new(self.substitute(generics[0].clone())), *size)
        }
        ast::TypeConstructorKind::Boolean => ast::Type::Basic(ast::BasicType::Bool),
        ast::TypeConstructorKind::Signature => {
          // FIXME: How can we reform the type if we're missing whether it is
          // ... an extern, variadic, and so on?
          // ast::Type::Function(ast::FunctionType {
          //   // parameters: all types except the last one.
          //   parameters: ,
          //   // return type: the last type.
          //   return_type: generics[1].clone(),
          // })

          // REVISE: Too much cloning.

          let parameter_types = if (generics.len() as u32) > 1 {
            &generics[0..generics.len() - 1]
          } else {
            &[]
          }
          .to_vec()
          .into_iter()
          .map(|generic| self.substitute(generic))
          .collect();

          let return_type = Box::new(self.substitute(generics[generics.len() - 1].clone()));

          ast::Type::Signature(ast::SignatureType {
            parameter_types,
            return_type,
            // BUG: Find a way to get rid of this on the signature type's fields.
            // ... It's just used once, and as we can observe is inconvenient.
            // ... If miraculously we end up needed it, then leave it as `false` since only
            // ... non-externs (non-variadic) can have type inference?
            is_variadic: false,
          })
        }
        ast::TypeConstructorKind::Integer => {
          // TODO: Take into account size.
          ast::Type::Basic(ast::BasicType::Int(ast::IntSize::I32))
        }
        // FIXME: Debugging only.
        ast::TypeConstructorKind::Nullptr => ast::Type::Any,
        _ => todo!(),
      },
      _ => ty,
    }
  }

  // REVIEW: Now with the implementation of `node.find_id()`, things like `(expr)` might have issues.
  // ... Perhaps employ the use of node flattening to solve any issues?
  // REVIEW: What is the `expectedType` parameter used for in the type inference reference?
  fn infer_type_of(&mut self, node: &ast::NodeKind) -> ast::Type {
    // REVIEW: This only works for direct declarations. All other nodes will type `Unit`.
    // BUG: (1) The problem is that the element type is taking on the id of the array node.
    // ... This possibly means that when retrieved again, it will be the type of the element and not the node itself.
    // if let Some(id) = node.find_id() {
    //   if let Some(cached_type) = self.type_cache.get(&id) {
    //     return cached_type.clone();
    //   }
    // }

    // REVIEW: Shouldn't we be associating (substitutions is associations) any type variables created with the id of their corresponding nodes?
    // ... Example: Nullptr literal, its id => type variable.
    let ty = match &node {
      ast::NodeKind::BinaryExpr(binary_expr) => match binary_expr.operator {
        ast::OperatorKind::LessThan
        | ast::OperatorKind::GreaterThan
        | ast::OperatorKind::Equality
        | ast::OperatorKind::And
        | ast::OperatorKind::Or
        | ast::OperatorKind::Nand
        | ast::OperatorKind::Nor
        | ast::OperatorKind::Xor
        | ast::OperatorKind::In => ast::Type::Basic(ast::BasicType::Bool),
        _ => self.infer_type_of(&binary_expr.left_operand),
      },
      ast::NodeKind::BindingStmt(binding_stmt) => binding_stmt
        .type_hint
        // REVISE: Cloning regardless.
        .clone()
        .unwrap_or_else(|| self.create_type_variable()),
      ast::NodeKind::Parameter(parameter) => parameter
        .type_hint
        // REVISE: Cloning regardless.
        .clone()
        .unwrap_or_else(|| self.create_type_variable()),
      ast::NodeKind::Literal(literal) => match literal {
        ast::Literal::Bool(_) => ast::Type::Basic(ast::BasicType::Bool),
        ast::Literal::Char(_) => ast::Type::Basic(ast::BasicType::Char),
        ast::Literal::Int(_, size) => ast::Type::Basic(ast::BasicType::Int(size.clone())),
        ast::Literal::String(_) => ast::Type::Basic(ast::BasicType::String),
        ast::Literal::Nullptr(..) => {
          // TODO: Temporary.
          // return ast::Type::Pointer(Box::new(self.create_type_variable()))
          self.create_type_variable()
        }
      },
      ast::NodeKind::Reference(reference) => {
        // BUG: Other instances don't infer types of other things, take a look at the binding
        // ... statement for example, it doesn't look at its value here. Same goes for the static
        // ... array value. So why is this here? If we do make it a simple unknown type variable,
        // ... then what connection will there be? Will it even work? Or, is it that we've just
        // ... made the inference external to use constraints on the visitation methods?
        let target = self
          .cache
          .find_decl_via_link(&reference.pattern.link_id)
          .unwrap();

        self.infer_type_of(&target)
      }
      ast::NodeKind::Array(array) => {
        ast::Type::Constructor(
          // REVIEW: Is this conversion safe?
          ast::TypeConstructorKind::StaticIndexable(array.elements.len() as u32),
          vec![array
            .elements
            .first()
            .and_then(|element| Some(self.infer_type_of(element)))
            // BUG: (2) The problem is that the element type is taking on the id of the array node.
            .unwrap_or_else(|| self.create_type_variable())],
        )
      }
      // ast::NodeKind::StaticArrayValue(array) => {
      //   self.create_type_variable(array.id)
      // }
      // REVIEW: What about other nodes? Say, indexing, array value, etc. Their types shouldn't be `Unit`.
      _ => ast::Type::Unit,
    };

    // REVIEW: Why is this here? We need to report things' types somewhere to fill up the
    // ... type cache, but why here? Also, probably only declaration's types are to be inserted
    // ... on the type cache, because only declarations have ids. Actually, it's here because this
    // ... function performs memoization of the inferred types, but apparently only for declarations.
    // ... Review what can be done, and whether it is efficient to only infer and memoize declaration's types.
    if let Some(node_id) = node.find_id() {
      // Update association of the node's id with the newly inferred type.
      // TODO: Better way to do this. Not all paths may create a type variable. And it's hacky.
      // self.links.insert(node_id, self.substitutions.len() - 1);
    }

    ty
  }

  // TODO: Why not take an id here, and directly associate?
  fn create_type_variable(&mut self) -> ast::Type {
    // REVIEW: Why add have a new id for this type variable? Couldn't we use the node id?
    // let id = self.substitutions.len();

    let type_variable_id = self.substitutions.len();
    let type_variable = ast::Type::Variable(type_variable_id);

    // Update association of the node's id with the new fresh type variable.
    self
      .substitutions
      .insert(type_variable_id, type_variable.clone());

    // Link the type variable's id to the node id, that way they are associated
    // and can be retrieved once type inference is complete.
    // node_id_opt.map(|node_id| self.links.insert(type_variable_id, node_id));

    // REVIEW: Why add have a new id for this type variable? Couldn't we use the node id?
    // self.substitutions.insert(id, result.clone());

    type_variable
  }

  /// Add a constraint stating that both of the provided types are equal.
  fn report_constraint(&mut self, that_type_or_var: ast::Type, is_equal_to: ast::Type) {
    self.constraints.push((that_type_or_var, is_equal_to));
  }

  fn type_hint_or_variable(&mut self, type_hint: &Option<ast::Type>) -> ast::Type {
    type_hint
      .as_ref()
      .and_then(|type_hint| Some(type_hint.clone()))
      .unwrap_or_else(|| self.create_type_variable())
  }

  // BUG: It may be that by using visitor pattern we're not doing what the algorithm
  // ... intends. I think it intends for types to be created and associated, within the
  // ... inference function itself, and have it call itself recursively while so. This is
  // ... technically possible because we can associate nodes with their types as we go along
  // ... by replacing them on the type cache.
  // Changes:
  // 1. no longer returns a type (on the ref. it was the re-constructed expression).
  // 2. caching is done per-node, because some nodes have more than 1 id (array have one for element type, and for themselves.).
  fn infer_and_constrain(&mut self, u_expected_type: ast::Type, node: &ast::NodeKind) {
    // TODO:
    let expected_type = u_expected_type.try_upgrade_constructor();

    match node {
      ast::NodeKind::BlockExpr(block_expr) => {
        for statement in &block_expr.statements {
          // FIXME: What type should be expected for statements? Or should it be Option.none?
          // ... Or is `ast::Type::Unit` (statements don't evaluate) okay?
          // TODO: Why not using type constructor? Yet still works?
          self.infer_and_constrain(ast::Type::Unit, statement);

          if let ast::NodeKind::ReturnStmt(ast::ReturnStmt { value }) = &statement {
            // TODO: Explain this. Not immediately clear.
            return if let Some(value) = value {
              self.infer_and_constrain(expected_type, value);
            } else {
              self.report_constraint(expected_type, ast::Type::Unit);
            };
          }
        }

        // REVIEW:
        if let Some(yields) = &block_expr.yields {
          self.infer_and_constrain(expected_type, yields);
        } else {
          // TODO: Why not using type constructor? Yet still works?
          self.report_constraint(expected_type, ast::Type::Unit);
        }
      }
      ast::NodeKind::Function(function) => {
        //val newReturnType = returnType.getOrElse(freshTypeVariable())
        // val newParameterTypes = parameters.map(_.typeAnnotation.getOrElse(freshTypeVariable()))
        // val newParameters = parameters.zip(newParameterTypes).map { case (p, t) =>
        //     p.copy(typeAnnotation = Some(t))
        // }
        // val newEnvironment = environment ++ newParameters.map { p =>
        //     p.name -> p.typeAnnotation.get
        // }
        // val newBody = infer(newEnvironment, newReturnType, body)
        // typeConstraints += CEquality(expectedType,
        //     TConstructor(s"Function${parameters.size}", newParameterTypes ++ List(newReturnType))
        // )
        // ELambda(newParameters, Some(newReturnType), newBody)
        let return_type = function
          .signature
          .return_type_hint
          .as_ref()
          .map(|type_hint| type_hint.clone())
          .unwrap_or_else(|| self.create_type_variable());

        self
          .type_cache
          .insert(function.signature.return_type_id, return_type.clone());

        let parameter_types = function
          .signature
          .parameters
          .iter()
          .map(|parameter| {
            let ty = parameter
              .type_hint
              .as_ref()
              .map(|parameter_type_hint| parameter_type_hint.clone())
              .unwrap_or_else(|| self.create_type_variable());

            self.type_cache.insert(parameter.id, ty.clone());

            ty
          })
          .collect::<Vec<_>>();

        // BUG: Do we cache the body type here or when during infer of block expr?
        // ... Should we even need to cache the body's type? Is it needed?
        self.infer_and_constrain(
          return_type.clone(),
          &ast::NodeKind::BlockExpr(std::rc::Rc::clone(&function.body)),
        );

        self.type_cache.insert(
          function.id,
          ast::Type::Signature(ast::SignatureType {
            is_variadic: function.signature.is_variadic,
            parameter_types: parameter_types.clone(),
            return_type: Box::new(return_type.clone()),
          })
          .try_upgrade_constructor(),
        );

        let mut constructor_generics = parameter_types;

        constructor_generics.extend(std::iter::once(return_type));

        let constructor_type =
          ast::Type::Constructor(ast::TypeConstructorKind::Signature, constructor_generics);

        self.report_constraint(expected_type, constructor_type);
      }
      // BUG: Temporary to follow the tutorial. Only works for previously
      // ... declared variables (no functions, etc.).
      ast::NodeKind::Reference(reference) => {
        let variable_type = self
          .type_cache
          .get(self.cache.links.get(&reference.pattern.link_id).unwrap())
          .unwrap()
          .clone();

        self.report_constraint(expected_type, variable_type);
      }
      // On literals and known types (unary, binary expressions, etc.),
      // report constraints. On others, pass it down.
      ast::NodeKind::Literal(literal) => {
        let constructor_kind = match literal {
          ast::Literal::Bool(_) => ast::TypeConstructorKind::Boolean,
          // BUG: Integer types should be specific, otherwise we can't re-construct them.
          ast::Literal::Int(..) => ast::TypeConstructorKind::Integer,
          ast::Literal::Nullptr(..) => ast::TypeConstructorKind::Nullptr,
          _ => todo!(),
        };

        let constructor_type = ast::Type::Constructor(constructor_kind, vec![]);

        self.report_constraint(expected_type, constructor_type);
      }
      ast::NodeKind::Array(array) => {
        // val newItemType = itemType.getOrElse(freshTypeVariable())
        // val newItems = items.map(item => infer(environment, newItemType, item))
        // typeConstraints += CEquality(expectedType, TConstructor("Array", List(newItemType)))
        // EArray(Some(newItemType), newItems)
        let element_type = self
          .type_cache
          .get(&array.element_type_id)
          .map(|element_type| element_type.clone())
          .unwrap_or_else(|| self.create_type_variable());

        self
          .type_cache
          .insert(array.element_type_id, element_type.clone());

        for element in &array.elements {
          self.infer_and_constrain(element_type.clone(), element);
        }

        let constructor_type = ast::Type::Constructor(
          // REVIEW: Is this conversion safe?
          ast::TypeConstructorKind::StaticIndexable(array.elements.len() as u32),
          vec![element_type],
        );

        self.type_cache.insert(array.id, constructor_type.clone());
        self.report_constraint(expected_type, constructor_type);
      }
      ast::NodeKind::BindingStmt(binding_stmt) => {
        // val newTypeAnnotation = typeAnnotation.getOrElse(freshTypeVariable())
        // val newValue = infer(environment, newTypeAnnotation, value)
        // val newEnvironment = environment.updated(name, newTypeAnnotation)
        // val newBody = infer(newEnvironment, expectedType, body)
        // ELet(name, Some(newTypeAnnotation), newValue, newBody)
        let ty = binding_stmt
          .type_hint
          .as_ref()
          .map(|type_hint| type_hint.clone())
          .unwrap_or_else(|| self.create_type_variable());

        self.type_cache.insert(binding_stmt.id, ty.clone());

        self.infer_and_constrain(ty, &binding_stmt.value);
      }
      ast::NodeKind::ReturnStmt(return_stmt) => {
        // val newBody = infer(environment, expectedType, body)
        // typeConstraints += CEquality(expectedType, TConstructor("Unit"))
        // EReturn(newBody)
        // self.new_super_infer(expected_type, &ast::NodeKind::Unit);
        // Pass it down.
        if let Some(return_value) = &return_stmt.value {
          self.infer_and_constrain(expected_type, &return_value);
        }
      }
      ast::NodeKind::UnaryExpr(unary_expr) => {
        // val newOperand = infer(environment, expectedType, operand)
        // typeConstraints += CEquality(expectedType, TConstructor("UnaryOp", List(newOperand.getType())))
        // EUnaryOp(op, newOperand)
        let ty = match &unary_expr.operator {
          ast::OperatorKind::Not => ast::Type::Basic(ast::BasicType::Bool),
          // TODO: Add more operators.
          // FIXME: Debugging only.
          ast::OperatorKind::MultiplyOrDereference => ast::Type::Any,
          // FIXME: Debugging only.
          ast::OperatorKind::SubtractOrNegate => ast::Type::AnyInteger,
          ast::OperatorKind::Cast => unary_expr.cast_type.clone().unwrap(),
          _ => unreachable!(),
        };

        self.infer_and_constrain(ty.clone(), &unary_expr.operand);
        self.report_constraint(ty, expected_type);
      }
      // ast::NodeKind::Parameter(parameter) => {
      //   // TODO: What if its type is already set in the type cache/environment?
      //   todo!();

      //   // val newTypeAnnotation = typeAnnotation.getOrElse(freshTypeVariable())
      //   // val newEnvironment = environment.updated(name, newTypeAnnotation)
      //   // val newBody = infer(newEnvironment, expectedType, body)
      //   // EParameter(name, Some(newTypeAnnotation), newBody)
      //   let ty = parameter
      //     .type_hint
      //     .as_ref()
      //     .map(|type_hint| type_hint.clone())
      //     .unwrap_or_else(|| self.create_type_variable());

      //   self.type_cache.insert(parameter.id, ty.clone());
      //   self.report_constraint(expected_type, ty);
      // }
      ast::NodeKind::InlineExprStmt(inline_expr_stmt) => {
        // val newTypeAnnotation = typeAnnotation.getOrElse(freshTypeVariable())
        // val newValue = infer(environment, newTypeAnnotation, value)
        // val newEnvironment = environment.updated(name, newTypeAnnotation)
        // val newBody = infer(newEnvironment, expectedType, body)
        // ELet(name, Some(newTypeAnnotation), newValue, newBody)
        // Pass it down.
        self.infer_and_constrain(expected_type, &inline_expr_stmt.expr);
      }
      ast::NodeKind::IndexingExpr(indexing_expr) => {
        // val newOperand = infer(environment, expectedType, operand)
        // val newIndex = infer(environment, expectedType, index)
        // typeConstraints += CEquality(expectedType, TConstructor("Index", List(newOperand.getType(), newIndex.getType())))
        // EIndex(newOperand, newIndex)
        // FIXME: This is probably incorrect / missing something.
        // ... should we expect the target to be an array of length ? and the index
        // ... to be a u32 integer?

        self.infer_and_constrain(expected_type.clone(), &indexing_expr.target_expr);

        self.infer_and_constrain(
          ast::Type::Constructor(
            // FIXME: Integer size.
            ast::TypeConstructorKind::Integer,
            vec![],
          ),
          &indexing_expr.index_expr,
        );
      }
      ast::NodeKind::IfExpr(if_expr) => {
        // val newCondition = infer(environment, expectedType, condition)
        // val newThen = infer(environment, expectedType, then)
        // val newElse = infer(environment, expectedType, else)
        // typeConstraints += CEquality(expectedType, TConstructor("If", List(newCondition.getType(), newThen.getType(), newElse.getType())))
        // EIf(newCondition, newThen, newElse)

        self.infer_and_constrain(
          ast::Type::Constructor(ast::TypeConstructorKind::Boolean, Vec::new()),
          &if_expr.condition,
        );

        // The if expression will always have a unit type if it is
        // missing its else branch.
        let ty = if if_expr.else_value.is_none() {
          ast::Type::Unit
        } else {
          self.create_type_variable()
        };

        self.type_cache.insert(if_expr.id, ty.clone());
        self.infer_and_constrain(ty.clone(), &if_expr.then_value);

        if let Some(else_value) = &if_expr.else_value {
          self.infer_and_constrain(ty.clone(), else_value);
        }

        for (condition, alternative_branch) in &if_expr.alternative_branches {
          self.infer_and_constrain(
            ast::Type::Constructor(ast::TypeConstructorKind::Boolean, Vec::new()),
            condition,
          );

          self.infer_and_constrain(ty.clone(), alternative_branch);
        }
      }
      ast::NodeKind::BinaryExpr(binary_expr) => {
        let ty = match binary_expr.operator {
          ast::OperatorKind::Add
          | ast::OperatorKind::SubtractOrNegate
          | ast::OperatorKind::MultiplyOrDereference
          | ast::OperatorKind::Divide => {
            ast::Type::Constructor(ast::TypeConstructorKind::Integer, Vec::new())
          }
          // TODO: Any missing?
          ast::OperatorKind::Equality
          | ast::OperatorKind::And
          | ast::OperatorKind::Or
          | ast::OperatorKind::Nor
          | ast::OperatorKind::GreaterThan
          | ast::OperatorKind::GreaterThanOrEqual
          | ast::OperatorKind::LessThan
          | ast::OperatorKind::LessThanOrEqual
          | ast::OperatorKind::Xor => {
            ast::Type::Constructor(ast::TypeConstructorKind::Boolean, Vec::new())
          }
          // TODO:
          _ => todo!(),
        };

        self.infer_and_constrain(ty.clone(), &binary_expr.left_operand);
        self.infer_and_constrain(ty.clone(), &binary_expr.right_operand);
      }
      _ => {
        dbg!(node);
        todo!()
      }
    }
  }
}

impl<'a> AnalysisVisitor for TypeInferenceContext<'a> {
  fn visit_literal(&mut self, _literal: &ast::Literal) {
    // let id = match literal {
    //   ast::Literal::Nullptr(id, _) => id,
    //   // Only the nullptr literal has the juicy stuff.
    //   _ => return,
    // };

    // REVIEW: Isn't this a type constructor?
    // self.create_type_variable();

    // self.substitutions.insert(id.to_owned(), ty);
  }

  fn visit_binding_stmt(&mut self, binding_stmt: &ast::BindingStmt) {
    // FIXME: Added this constraint because it makes sense, but it was not part of the reference code.
    let value_type = self.infer_type_of(&binding_stmt.value);

    // self
    //   .links
    //   .insert(self.substitutions.len() - 1, binding_stmt.id);

    // REVIEW: Isn't this being repeated in the `infer_type_of` function? If so,
    // ... abstract common code or merge.
    let binding_type = self.type_hint_or_variable(&binding_stmt.type_hint);

    self.report_constraint(binding_type, value_type);

    // REVIEW: Don't we have to specify that the type of the binding must equal its value's type,
    // ... regardless of the type hint's presence?
    // if binding_stmt.type_hint.is_some() {
    //   return;
    // }

    // Associate the fresh type variable with the binding.
    // self
    //   .substitutions
    //   .insert(binding_stmt.id, fresh_type_variable.clone());
  }

  fn visit_unary_expr(&mut self, unary_expr: &ast::UnaryExpr) {
    // REVIEW: We need to insert a substitution? Otherwise what links the constraint back to this?
    let expr_type = self.infer_type_of(&unary_expr.operand);

    match unary_expr.operator {
      ast::OperatorKind::Add | ast::OperatorKind::SubtractOrNegate => {
        // Report that the type of the unary expression's operand must
        // be an integer.
        self.report_constraint(expr_type, ast::Type::AnyInteger);
      }
      // TODO: Add support for missing hints; logical not, and address-of?
      ast::OperatorKind::MultiplyOrDereference => {
        // let _pointer_inner_type = self.create_type_variable();

        // TODO: How to specify/deal with type constructor? Currently just defaulting to creating a type variable.
        // self.report_constraint(expr_type, ast::Type::Pointer(Box::new(pointer_inner_type)))

        // TODO: Temporary.
        self.report_constraint(expr_type, ast::Type::Any);
      }
      ast::OperatorKind::Not => {
        self.report_constraint(expr_type, ast::Type::Basic(ast::BasicType::Bool));
      }
      // NOTE: All cases for unary operators are covered above.
      _ => {}
    }
  }

  fn visit_binary_expr(&mut self, binary_expr: &ast::BinaryExpr) {
    let left_expr_type = self.infer_type_of(&binary_expr.left_operand);
    let right_expr_type = self.infer_type_of(&binary_expr.right_operand);

    // REVIEW: Should we report a constraint that both operands must be of the same type?
    // ... Or is it implied by both being expected to equal the same type? Will it affect the
    // ... algorithm if we don't report them to be equal to each other specifically?

    // REVIEW: What about the right expression?
    let ty = match binary_expr.operator {
      ast::OperatorKind::Add
      | ast::OperatorKind::SubtractOrNegate
      | ast::OperatorKind::MultiplyOrDereference
      | ast::OperatorKind::Divide
      | ast::OperatorKind::LessThan
      | ast::OperatorKind::LessThanOrEqual
      | ast::OperatorKind::Equality
      | ast::OperatorKind::GreaterThan
      | ast::OperatorKind::GreaterThanOrEqual => ast::Type::AnyInteger,
      ast::OperatorKind::And
      | ast::OperatorKind::Or
      | ast::OperatorKind::Nand
      | ast::OperatorKind::Nor
      | ast::OperatorKind::Xor => ast::Type::Basic(ast::BasicType::Bool),
      // TODO: Implement.
      ast::OperatorKind::Cast | ast::OperatorKind::In => todo!(),
      // NOTE: All cases for binary operators are covered above.
      _ => return,
    };

    self.report_constraint(left_expr_type, ty.clone());
    self.report_constraint(right_expr_type, ty);
  }

  fn visit_return_stmt(&mut self, return_stmt: &ast::ReturnStmt) {
    // REVIEW: What about block yielding as the return value of a function?

    let return_value_type = if let Some(return_value) = &return_stmt.value {
      self.infer_type_of(&return_value)
    } else {
      ast::Type::Unit
    };

    let current_function_sig = self
      .cache
      // NOTE: No need to follow declaration link because it is directly set.
      .declarations
      .get(&self.current_function_id.unwrap())
      .unwrap()
      .find_signature()
      .unwrap();

    // FIXME: Here, we are manually doing the job of `infer_type_of`, which does automatic caching as well.
    // ... Prone to bugs. Consider abstracting the common functionality, or merge into `infer_type_of`.
    let current_function_return_type = current_function_sig
      .return_type_hint
      // REVISE: Cloning regardless.
      .clone()
      .unwrap_or_else(|| self.create_type_variable());

    self.report_constraint(return_value_type, current_function_return_type);
  }

  fn visit_signature(&mut self, _signature: &ast::Signature) {
    // TODO: For the return type, we might need to add the `id` field to `ast::Signature`, and then
    // ... make it an `rc::Rc<>`, and possibly use a buffer id to be able to retrieve it during visitation.
  }

  fn visit_array(&mut self, _array_value: &ast::Array) {
    // REVIEW: This check is here to prevent overriding previous type inference results.
    // ... (Say from binding invoking `infer_type_of` upon this array value).
    // if self.substitutions.contains_key(&array_value.id) {
    //   return;
    // }

    // let array_element_type = if let Some(first_element) = array_value.elements.first() {
    //   self.infer_type_of(first_element)
    // } else {
    //   self.create_type_variable()
    // };

    // // Create the static indexable type.
    // let array_type = ast::Type::StaticIndexable(
    //   // With our unknown element type.
    //   Box::new(array_element_type.clone()),
    //   // REVIEW: Is this conversion safe?
    //   array_value.elements.len() as u32,
    // );

    // FIXME: Abstract common functionality from `infer_type_of`.
    // Upgrade it and insert it into substitutions.
    // self
    //   .substitutions
    //   .insert(array_value.id, array_type.clone().try_upgrade());

    // TODO: If we constrain the element types to all match the same as the array's element type,
    // ... we basically get a type checking advantage, since it verifies that all elements are of the same type.
    // for element in &array_value.elements {
    //   let actual_element_type = self.infer_type_of(element);

    //   self.report_constraint(actual_element_type, array_element_type.clone());
    // }
    // let array_type = self.create_type_variable(array_value.id);

    // let element_types
  }

  fn visit_call_expr(&mut self, _call_expr: &ast::CallExpr) {
    // TODO: The arguments of the call constrain the types of the target's parameters.
  }

  fn enter_function(&mut self, function: &ast::Function) {
    self.current_function_id = Some(function.id);
  }

  fn exit_function(&mut self, _function: &ast::Function) -> () {
    self.current_function_id = None;
  }
}

#[cfg(test)]
mod tests {
  use super::*;
  use crate::mock::tests::Mock;

  #[test]
  fn occurs_in() {
    let cache = cache::Cache::new();
    let mut type_inference_ctx = TypeInferenceContext::new(&cache);
    let first_index_id = 0;
    let second_index_id = first_index_id + 1;

    type_inference_ctx
      .substitutions
      .insert(first_index_id, ast::Type::Variable(first_index_id));

    type_inference_ctx
      .substitutions
      .insert(second_index_id, ast::Type::Unit);

    let subject_type_variable = ast::Type::Variable(first_index_id);

    assert!(type_inference_ctx.occurs_in(first_index_id, &subject_type_variable));
    assert!(!type_inference_ctx.occurs_in(second_index_id, &subject_type_variable));
    assert!(!type_inference_ctx.occurs_in(first_index_id, &ast::Type::Unit));
  }

  #[test]
  fn create_type_variable() {
    let cache = cache::Cache::new();
    let mut type_context = TypeInferenceContext::new(&cache);

    assert_eq!(type_context.create_type_variable(), ast::Type::Variable(0));
    assert_eq!(1, type_context.substitutions.len());
  }

  #[test]
  fn solve_constraints() {
    let cache = cache::Cache::new();
    let mut type_context = TypeInferenceContext::new(&cache);

    // TODO: Add actual constraints to complete this test.

    type_context.solve_constraints();
    assert!(type_context.constraints.is_empty());
  }

  #[test]
  fn substitute() {
    let cache = cache::Cache::new();
    let mut type_context = TypeInferenceContext::new(&cache);

    assert_eq!(ast::Type::Unit, type_context.substitute(ast::Type::Unit));

    let type_variable_id = 0;
    let non_type_variable = ast::Type::Basic(ast::BasicType::Bool);

    type_context
      .substitutions
      .insert(type_variable_id, non_type_variable.clone());

    assert_eq!(
      non_type_variable,
      type_context.substitute(ast::Type::Variable(type_variable_id))
    );
  }

  #[test]
  fn infer_binding_from_literal() {
    let mut cache = cache::Cache::new();
    let binding_stmt_id = cache.next_id();
    let mut type_inference_ctx = TypeInferenceContext::new(&cache);

    let binding_stmt = Mock::free_binding(
      binding_stmt_id,
      "a",
      ast::NodeKind::Literal(ast::Literal::Bool(true)),
      None,
    );

    let initial_expected_type = type_inference_ctx.create_type_variable();

    type_inference_ctx.infer_and_constrain(initial_expected_type, &binding_stmt);
    type_inference_ctx.solve_constraints();

    assert_eq!(
      Some(&ast::Type::Basic(ast::BasicType::Bool)),
      type_inference_ctx.type_cache.get(&binding_stmt_id)
    );
  }

  #[test]
  fn infer_binding_from_reference() {
    let mut cache = cache::Cache::new();
    let binding_stmt_a_id = cache.next_id();
    let binding_stmt_b_id = cache.next_id();

    let binding_stmt_a = Mock::free_binding(
      binding_stmt_a_id,
      "a",
      ast::NodeKind::Literal(ast::Literal::Bool(true)),
      None,
    );

    cache.links.insert(binding_stmt_a_id, binding_stmt_a_id);

    cache
      .declarations
      .insert(binding_stmt_a_id, binding_stmt_a.clone());

    let binding_stmt_b = Mock::free_binding(
      binding_stmt_b_id,
      "b",
      ast::NodeKind::Reference(Mock::reference(binding_stmt_a_id)),
      None,
    );

    let mut type_inference_ctx = TypeInferenceContext::new(&cache);
    let initial_expected_type_a = type_inference_ctx.create_type_variable();
    let initial_expected_type_b = type_inference_ctx.create_type_variable();

    type_inference_ctx.infer_and_constrain(initial_expected_type_a, &binding_stmt_a);
    type_inference_ctx.infer_and_constrain(initial_expected_type_b, &binding_stmt_b);
    type_inference_ctx.solve_constraints();

    // REVIEW: Why is this failing?
    assert_eq!(
      &ast::Type::Basic(ast::BasicType::Bool),
      type_inference_ctx
        .type_cache
        .get(&binding_stmt_b_id)
        .unwrap(),
    );
  }

  #[test]
  fn infer_binding_from_nullptr() {
    let mut cache = cache::Cache::new();
    let binding_id = 0;

    let binding_stmt = Mock::free_binding(
      binding_id.clone(),
      "a",
      ast::NodeKind::Literal(ast::Literal::Nullptr(1, None)),
      None,
    );

    cache.links.insert(binding_id, binding_id);
    cache.declarations.insert(binding_id, binding_stmt.clone());

    let mut type_inference_ctx = TypeInferenceContext::new(&cache);

    let deref_expr = ast::NodeKind::UnaryExpr(ast::UnaryExpr {
      cast_type: None,
      operand: std::rc::Rc::new(ast::NodeKind::Reference(Mock::reference(binding_id))),
      operator: ast::OperatorKind::MultiplyOrDereference,
    });

    let initial_expected_type_a = type_inference_ctx.create_type_variable();
    let initial_expected_type_b = type_inference_ctx.create_type_variable();

    type_inference_ctx.infer_and_constrain(initial_expected_type_a, &binding_stmt);
    type_inference_ctx.infer_and_constrain(initial_expected_type_b, &deref_expr);
    type_inference_ctx.solve_constraints();

    assert_eq!(
      // FIXME: Awaiting type constructor inference.
      Some(&ast::Type::Any),
      type_inference_ctx.type_cache.get(&binding_id)
    );
  }

  #[test]
  fn infer_parameter_from_unary_expr() {
    let mut cache = cache::Cache::new();

    let parameter = ast::Parameter {
      id: cache.next_id(),
      name: "a".to_string(),
      position: 0,
      type_hint: None,
    };

    cache.links.insert(parameter.id, parameter.id);

    let negation_expr = ast::NodeKind::UnaryExpr(ast::UnaryExpr {
      cast_type: None,
      operand: std::rc::Rc::new(ast::NodeKind::Reference(Mock::reference(parameter.id))),
      operator: ast::OperatorKind::Not,
    });

    let function_id = cache.next_id();

    let function = Mock::free_function(
      function_id,
      vec![parameter],
      vec![ast::NodeKind::InlineExprStmt(ast::InlineExprStmt {
        expr: Box::new(negation_expr),
      })],
    );

    let mut type_inference_ctx = TypeInferenceContext::new(&cache);
    let initial_expected_type = type_inference_ctx.create_type_variable();

    type_inference_ctx.infer_and_constrain(initial_expected_type, &function);
    type_inference_ctx.solve_constraints();

    let test_expected_type = ast::Type::Signature(ast::SignatureType {
      is_variadic: false,
      parameter_types: vec![ast::Type::Basic(ast::BasicType::Bool)],
      return_type: Box::new(ast::Type::Unit),
    });

    assert_eq!(
      Some(&test_expected_type),
      type_inference_ctx.type_cache.get(&function_id)
    );
  }

  // #[test]
  // fn infer_parameter_from_call_expr() {
  //   let mut cache = cache::Cache::new();
  //   let id = 0;

  //   let mut function = Mock::free_function(Vec::new());

  //   let mut sig = function.find_signature().unwrap();

  //   let inner_param = ast::Parameter {
  //     id,
  //     name: "a".to_string(),
  //     position: 0,
  //     type_hint: None,
  //   };

  //   let parameter = ast::NodeKind::Parameter(std::rc::Rc::new(inner_param));

  //   sig.parameters.push(std::rc::Rc::new(inner_param));
  //   cache.links.insert(id, id);
  //   cache.declarations.insert(id, parameter.clone());

  //   let mut type_inference_ctx = TypeInferenceContext::new(&cache);

  //   let negation_expr = ast::NodeKind::UnaryExpr(ast::UnaryExpr {
  //     cast_type: None,
  //     expr: std::rc::Rc::new(ast::NodeKind::Reference(Mock::reference(id))),
  //     operator: ast::OperatorKind::SubtractOrNegate,
  //   });

  //   let bool_literal = ast::NodeKind::Literal(ast::Literal::Bool(true));

  //   let call_expr = ast::NodeKind::CallExpr(ast::CallExpr {
  //     arguments: vec![bool_literal],
  //     callee_expr:
  //   });

  //   type_inference_ctx.dispatch(&parameter);
  //   visitor::traverse(&negation_expr, &mut type_inference_ctx);
  //   type_inference_ctx.solve_constraints();

  //   assert_eq!(
  //     type_inference_ctx.substitute(ast::Type::Variable(id)),
  //     // FIXME: Awaiting type constructor inference.
  //     ast::Type::AnyInteger
  //   );

  //   assert_eq!(
  //     type_inference_ctx.type_cache.get(&id).unwrap(),
  //     &ast::Type::AnyInteger
  //   );
  // }

  #[test]
  fn infer_return_type_from_literal() {
    let mut cache = cache::Cache::new();

    let return_stmt = ast::NodeKind::ReturnStmt(ast::ReturnStmt {
      value: Some(Box::new(ast::NodeKind::Literal(ast::Literal::Bool(true)))),
    });

    let function_id = cache.next_id();
    let function = Mock::free_function(function_id, Vec::new(), vec![return_stmt]);

    cache.declarations.insert(function_id, function.clone());

    let mut type_inference_ctx = TypeInferenceContext::new(&cache);
    let initial_expected_type = type_inference_ctx.create_type_variable();

    type_inference_ctx.infer_and_constrain(initial_expected_type, &function);
    type_inference_ctx.solve_constraints();

    assert_eq!(
      Some(&ast::Type::Signature(ast::SignatureType {
        parameter_types: Vec::new(),
        return_type: Box::new(ast::Type::Basic(ast::BasicType::Bool)),
        is_variadic: false,
      })),
      type_inference_ctx.type_cache.get(&function_id)
    );
  }

  #[test]
  fn infer_array_type_from_binding_hint() {
    let mut cache = cache::Cache::new();
    let binding_stmt_id = cache.next_id();
    let array_id = cache.next_id();
    let array_element_id = cache.next_id();
    let mut type_inference_ctx = TypeInferenceContext::new(&cache);

    let array = ast::NodeKind::Array(ast::Array {
      elements: Vec::new(),
      id: array_id,
      element_type_id: array_element_id,
    });

    let binding_type =
      ast::Type::StaticIndexable(Box::new(ast::Type::Basic(ast::BasicType::Bool)), 0);

    let binding_stmt = Mock::free_binding(binding_stmt_id, "a", array, Some(binding_type.clone()));
    let initial_expected_type = type_inference_ctx.create_type_variable();

    type_inference_ctx.infer_and_constrain(initial_expected_type, &binding_stmt);
    type_inference_ctx.solve_constraints();

    assert_eq!(
      Some(&binding_type),
      type_inference_ctx.type_cache.get(&array_id)
    )
  }

  #[test]
  fn infer_array_type_from_element() {
    let mut cache = cache::Cache::new();
    let array_id = cache.next_id();
    let element_type_id = cache.next_id();
    let mut type_inference_ctx = TypeInferenceContext::new(&cache);

    let array = ast::NodeKind::Array(ast::Array {
      elements: vec![ast::NodeKind::Literal(ast::Literal::Bool(true))],
      id: array_id,
      element_type_id,
    });

    let initial_expected_type = type_inference_ctx.create_type_variable();

    type_inference_ctx.infer_and_constrain(initial_expected_type, &array);
    type_inference_ctx.solve_constraints();

    let test_expected_type =
      ast::Type::StaticIndexable(Box::new(ast::Type::Basic(ast::BasicType::Bool)), 1);

    let test_actual_type = type_inference_ctx
      .type_cache
      .get(&array_id)
      .unwrap()
      .to_owned();

    assert_eq!(test_expected_type, test_actual_type)
  }

  // TODO: Use the empty array type test.
  // TODO: Also, create a second test for inferring of parameter types.
}
