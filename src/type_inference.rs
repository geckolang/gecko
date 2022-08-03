use crate::{ast, symbol_table, visitor::AnalysisVisitor};

pub type TypeVariableId = usize;
pub type TypeCache = std::collections::HashMap<symbol_table::NodeId, ast::Type>;
type TypeConstraint = (ast::Type, ast::Type);

// REVIEW: Consider using a trait-visitor pattern here.

pub fn run(
  ast_map: &ast::AstMap,
  symbol_table: &symbol_table::SymbolTable,
) -> (Vec<ast::Diagnostic>, TypeCache) {
  let mut type_inference_ctx = TypeInferenceContext::new(symbol_table);

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
  substitutions: std::collections::HashMap<symbol_table::NodeId, ast::Type>,
  cache: &'a symbol_table::SymbolTable,
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
}

impl<'a> TypeInferenceContext<'a> {
  pub fn new(cache: &'a symbol_table::SymbolTable) -> Self {
    Self {
      diagnostics: Vec::new(),
      substitutions: std::collections::HashMap::new(),
      cache,
      type_cache: TypeCache::new(),
      constraints: Vec::new(),
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
      ast::Type::Variable(id) => {
        println!("=======================");
        dbg!(self.constraints.clone());
        dbg!(self.substitutions.clone());
        println!("=======================");

        panic!(
          "Pending proper error handling; Type variable {} could not be resolved!",
          id
        );
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
        ast::TypeConstructorKind::Function => {
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
        ast::TypeConstructorKind::String => ast::Type::Basic(ast::BasicType::String),
        _ => {
          dbg!(ty);
          todo!()
        }
      },
      _ => ty,
    }
  }

  // TODO: Why not take an id here, and directly associate?
  fn create_type_variable(&mut self) -> ast::Type {
    let type_variable_id = self.substitutions.len();
    let type_variable = ast::Type::Variable(type_variable_id);

    self
      .substitutions
      .insert(type_variable_id, type_variable.clone());

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
  fn infer_and_constrain(&mut self, initial_expected_type: ast::Type, node: &ast::NodeKind) {
    // TODO:
    let expected_type = initial_expected_type.try_upgrade_constructor();

    match node {
      // The expected type for blocks is the function signature's return type.
      ast::NodeKind::BlockExpr(block_expr) => {
        for statement in &block_expr.statements {
          // If the statement is a return statement, pass down the expected
          // type for it to constrain the given signature's return type.
          if let ast::NodeKind::ReturnStmt(_) = &statement {
            self.infer_and_constrain(expected_type.clone(), statement);
          } else {
            // TODO: Why not using type constructor? Yet still works?
            // All statements should evaluate unit type.
            self.infer_and_constrain(ast::Type::Unit, statement);
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

        // Cache the function type before inferring the body to allow
        // for recursion, otherwise they may try to retrieve the function type
        // when it hasn't been set yet.
        self.type_cache.insert(
          function.id,
          ast::Type::Signature(ast::SignatureType {
            is_variadic: function.signature.is_variadic,
            parameter_types: parameter_types.clone(),
            return_type: Box::new(return_type.clone()),
          })
          .try_upgrade_constructor(),
        );

        // BUG: Do we cache the body type here or when during infer of block expr?
        // ... Should we even need to cache the body's type? Is it needed?
        self.infer_and_constrain(
          return_type.clone(),
          &ast::NodeKind::BlockExpr(std::rc::Rc::clone(&function.body)),
        );

        let mut constructor_generics = parameter_types;

        constructor_generics.extend(std::iter::once(return_type));

        let constructor_type =
          ast::Type::Constructor(ast::TypeConstructorKind::Function, constructor_generics);

        self.report_constraint(expected_type, constructor_type);
      }
      // BUG: Temporary to follow the tutorial. Only works for previously
      // ... declared variables (no functions, etc.).
      ast::NodeKind::Reference(reference) => {
        let target_type = self
          .type_cache
          .get(self.cache.links.get(&reference.pattern.link_id).unwrap())
          .unwrap()
          .clone();

        self.report_constraint(expected_type, target_type);
      }
      // On literals and known types (unary, binary expressions, etc.),
      // report constraints. On others, pass it down.
      ast::NodeKind::Literal(literal) => {
        let constructor_kind = match literal {
          ast::Literal::Bool(_) => ast::TypeConstructorKind::Boolean,
          // BUG: Integer types should be specific, otherwise we can't re-construct them.
          ast::Literal::Int(..) => ast::TypeConstructorKind::Integer,
          ast::Literal::Nullptr(..) => ast::TypeConstructorKind::Nullptr,
          ast::Literal::String(..) => ast::TypeConstructorKind::String,
          _ => todo!(),
        };

        let constructor_type = ast::Type::Constructor(constructor_kind, vec![]);

        self.report_constraint(constructor_type, expected_type);
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
        let ty = binding_stmt
          .type_hint
          .as_ref()
          .map(|type_hint| type_hint.clone())
          .unwrap_or_else(|| self.create_type_variable());

        self.type_cache.insert(binding_stmt.id, ty.clone());
        self.infer_and_constrain(ty, &binding_stmt.value);
      }
      // The expected type for return statement is the function signature's return type.
      ast::NodeKind::ReturnStmt(return_stmt) => {
        // TODO: Switch the comment around to make it more clear.
        // If there is no return value, constrain the expected type to
        // be unit, otherwise pass the expected type down to the value
        // for it to constrain to.
        if let Some(return_value) = &return_stmt.value {
          self.infer_and_constrain(expected_type, return_value);
        } else {
          self.report_constraint(ast::Type::Unit, expected_type);
        };
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
          _ => unreachable!(),
        };

        self.infer_and_constrain(ty.clone(), &unary_expr.operand);
        self.report_constraint(ty, expected_type);
      }
      ast::NodeKind::CastExpr(cast_expr) => {
        let ty = cast_expr.cast_type.clone();

        self.infer_and_constrain(ty.clone(), &cast_expr.operand);
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

        // BUG: Type getting inferred as an array instead of an element. We have no mechanism
        // ... to handle the size of array constructor type yet. (We could constrain the target to
        // ... be an array of ? element type, but the size requirement for type is impeding that.).

        // We don't know and/or don't have direct access to the element type.
        let element_type = self.create_type_variable();

        let array_type = ast::Type::Constructor(
          // FIXME: How can we determine the array size? Currently we rely on the magic special
          // ... case on the unify function that ignores the size of array constructors. In other words,
          // ... since this array type is just used for comparison and not registered on the type cache,
          // ... the size here doesn't really matter -- it's just a dummy value.
          ast::TypeConstructorKind::StaticIndexable(0),
          vec![element_type.clone()],
        );

        self.report_constraint(element_type, expected_type);
        self.infer_and_constrain(array_type, &indexing_expr.target_expr);

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
        // Constrain the condition to be a boolean.
        self.infer_and_constrain(
          ast::Type::Constructor(ast::TypeConstructorKind::Boolean, Vec::new()),
          &if_expr.condition,
        );

        // The if expression will always have a unit type if it is
        // missing its else branch.
        let ty = if if_expr.else_branch.is_none() {
          ast::Type::Unit
        } else {
          self.create_type_variable()
        };

        self.report_constraint(ty.clone(), expected_type);
        self.type_cache.insert(if_expr.id, ty.clone());
        self.infer_and_constrain(ty.clone(), &if_expr.then_branch);

        for (condition, alternative_branch) in &if_expr.alternative_branches {
          self.infer_and_constrain(
            ast::Type::Constructor(ast::TypeConstructorKind::Boolean, Vec::new()),
            condition,
          );

          self.infer_and_constrain(ty.clone(), alternative_branch);
        }

        if let Some(else_value) = &if_expr.else_branch {
          self.infer_and_constrain(ty.clone(), else_value);
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

        self.report_constraint(ty.clone(), expected_type);
        self.infer_and_constrain(ty.clone(), &binary_expr.left_operand);
        self.infer_and_constrain(ty, &binary_expr.right_operand);
      }
      ast::NodeKind::UnsafeExpr(unsafe_expr) => {
        // Nothing to do on unsafe expr. It is a transient node.
        self.infer_and_constrain(expected_type, &unsafe_expr.0);
      }
      ast::NodeKind::CallExpr(call_expr) => {
        // self type = callee's type (return type, that is)
        // initially, we don't know our own type. => type variable?
        let return_type = self.create_type_variable();

        let argument_types = call_expr.arguments.iter().map(|argument| {
          // We don't have access to the argument type.
          let argument_type = self.create_type_variable();

          // Constrain it to be our new type variable.
          self.infer_and_constrain(argument_type.clone(), argument);

          argument_type
        });

        let callee_type = ast::Type::Constructor(
          ast::TypeConstructorKind::Function,
          argument_types
            .into_iter()
            .chain(std::iter::once(return_type.clone()))
            .collect(),
        );

        // BUG: Save the type on the cache? Currently Call expr doesn't have an id.

        self.infer_and_constrain(callee_type, &call_expr.callee_expr);
        self.report_constraint(return_type, expected_type);
      }
      ast::NodeKind::ExternFunction(extern_) => {
        let ty = ast::Type::Signature(ast::SignatureType {
          return_type: Box::new(extern_.signature.return_type_hint.as_ref().unwrap().clone()),
          is_variadic: extern_.signature.is_variadic,
          parameter_types: extern_
            .signature
            .parameters
            .iter()
            .map(|parameter| parameter.type_hint.as_ref().unwrap().clone())
            .collect(),
        });

        for parameter in &extern_.signature.parameters {
          self
            .type_cache
            .insert(parameter.id, parameter.type_hint.as_ref().unwrap().clone());
        }

        self.type_cache.insert(extern_.id, ty);
      }
      ast::NodeKind::IntrinsicCall(intrinsic_call) => {
        let constructor_kind = match intrinsic_call.kind {
          ast::IntrinsicKind::LengthOf => ast::TypeConstructorKind::Integer,
        };

        let ty = ast::Type::Constructor(constructor_kind, Vec::new());

        // TODO: What about arguments?

        self.report_constraint(ty, expected_type);
      }
      ast::NodeKind::SizeofIntrinsic(_) => {
        // TODO: What about the argument? Ignore it since we don't care about its type?

        self.report_constraint(
          ast::Type::Constructor(ast::TypeConstructorKind::Integer, Vec::new()),
          expected_type,
        );
      }
      ast::NodeKind::Struct(_) | ast::NodeKind::StructValue(_) | ast::NodeKind::StructImpl(_) => {
        // TODO: Implement.
      }
      ast::NodeKind::MemberAccess(member_access) => {
        // The overall type is the type of the field.

        // TODO: Generics?
        // Let the base expression be a struct type constructor.
        let base_expr_type = ast::Type::Constructor(ast::TypeConstructorKind::Struct, vec![]);

        // Infer it and constrain it to our expectation.
        self.infer_and_constrain(base_expr_type, &member_access.base_expr);

        // TODO: Finish implementation.
      }
      ast::NodeKind::Closure(closure) => {
        // REVISE: Repeated logic from function. Abstract common code and simplify.

        let return_type = closure
          .signature
          .return_type_hint
          .as_ref()
          .map(|type_hint| type_hint.clone())
          .unwrap_or_else(|| self.create_type_variable());

        self
          .type_cache
          .insert(closure.signature.return_type_id, return_type.clone());

        let parameter_types = closure
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

        // Cache the function type before inferring the body to allow
        // for recursion, otherwise they may try to retrieve the function type
        // when it hasn't been set yet.
        self.type_cache.insert(
          closure.id,
          ast::Type::Signature(ast::SignatureType {
            // Closures are never variadic.
            is_variadic: false,
            parameter_types: parameter_types.clone(),
            return_type: Box::new(return_type.clone()),
          })
          .try_upgrade_constructor(),
        );

        // BUG: Do we cache the body type here or when during infer of block expr?
        // ... Should we even need to cache the body's type? Is it needed?
        self.infer_and_constrain(
          return_type.clone(),
          &ast::NodeKind::BlockExpr(std::rc::Rc::clone(&closure.body)),
        );

        let mut constructor_generics = parameter_types;

        constructor_generics.extend(std::iter::once(return_type));

        let constructor_type =
          ast::Type::Constructor(ast::TypeConstructorKind::Function, constructor_generics);

        self.report_constraint(expected_type, constructor_type);
      }
      _ => {
        dbg!(node);
        todo!()
      }
    }
  }
}

impl<'a> AnalysisVisitor for TypeInferenceContext<'a> {
  //
}

#[cfg(test)]
mod tests {
  use super::*;
  use crate::test::tests::Test;

  #[test]
  fn occurs_in() {
    let cache = symbol_table::SymbolTable::new();
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
    let cache = symbol_table::SymbolTable::new();
    let mut type_context = TypeInferenceContext::new(&cache);

    assert_eq!(type_context.create_type_variable(), ast::Type::Variable(0));
    assert_eq!(1, type_context.substitutions.len());
  }

  #[test]
  fn solve_constraints() {
    let cache = symbol_table::SymbolTable::new();
    let mut type_context = TypeInferenceContext::new(&cache);

    // TODO: Add actual constraints to complete this test.

    type_context.solve_constraints();
    assert!(type_context.constraints.is_empty());
  }

  #[test]
  fn substitute() {
    let cache = symbol_table::SymbolTable::new();
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
    let mut cache = symbol_table::SymbolTable::new();
    let binding_stmt_id = cache.next_id();
    let mut type_inference_ctx = TypeInferenceContext::new(&cache);

    let binding_stmt = Test::free_binding(
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
    let mut cache = symbol_table::SymbolTable::new();
    let binding_stmt_a_id = cache.next_id();
    let binding_stmt_b_id = cache.next_id();

    let binding_stmt_a = Test::free_binding(
      binding_stmt_a_id,
      "a",
      ast::NodeKind::Literal(ast::Literal::Bool(true)),
      None,
    );

    cache.links.insert(binding_stmt_a_id, binding_stmt_a_id);

    cache
      .declarations
      .insert(binding_stmt_a_id, binding_stmt_a.clone());

    let binding_stmt_b = Test::free_binding(
      binding_stmt_b_id,
      "b",
      ast::NodeKind::Reference(Test::reference(binding_stmt_a_id)),
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
    let mut cache = symbol_table::SymbolTable::new();
    let binding_id = cache.next_id();

    let binding_stmt = Test::free_binding(
      binding_id.clone(),
      "a",
      ast::NodeKind::Literal(ast::Literal::Nullptr(cache.next_id())),
      None,
    );

    cache.links.insert(binding_id, binding_id);
    cache.declarations.insert(binding_id, binding_stmt.clone());

    let mut type_inference_ctx = TypeInferenceContext::new(&cache);

    let deref_expr = ast::NodeKind::UnaryExpr(ast::UnaryExpr {
      operand: std::rc::Rc::new(ast::NodeKind::Reference(Test::reference(binding_id))),
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
    let mut cache = symbol_table::SymbolTable::new();

    let parameter = ast::Parameter {
      id: cache.next_id(),
      name: "a".to_string(),
      position: 0,
      type_hint: None,
    };

    cache.links.insert(parameter.id, parameter.id);

    let negation_expr = ast::NodeKind::UnaryExpr(ast::UnaryExpr {
      operand: std::rc::Rc::new(ast::NodeKind::Reference(Test::reference(parameter.id))),
      operator: ast::OperatorKind::Not,
    });

    let function_id = cache.next_id();

    let function = Test::free_function(
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

  #[test]
  fn infer_return_type_from_literal() {
    let mut cache = symbol_table::SymbolTable::new();

    let return_stmt = ast::NodeKind::ReturnStmt(ast::ReturnStmt {
      value: Some(Box::new(ast::NodeKind::Literal(ast::Literal::Bool(true)))),
    });

    let function_id = cache.next_id();
    let function = Test::free_function(function_id, Vec::new(), vec![return_stmt]);

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
    let mut cache = symbol_table::SymbolTable::new();
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

    let binding_stmt = Test::free_binding(binding_stmt_id, "a", array, Some(binding_type.clone()));
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
    let mut cache = symbol_table::SymbolTable::new();
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
}
