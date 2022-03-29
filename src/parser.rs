use crate::{ast, cache, diagnostic, lexer, name_resolution};

pub const THIS_IDENTIFIER: &str = "this";

// TODO: Add more test cases for larger numbers than `0`. Also, is there a need for a panic here? If so, consider using `unreachable!()`. Additionally, should `unreachabe!()` panics even be reported on the documentation?
/// Determine the minimum bit-size in which a number can fit.
fn minimum_int_size_of(number: &u64) -> ast::IntSize {
  let log2_result = f64::log2(*number as f64 + 1_f64);
  let minimum_bit_size = f64::floor(log2_result) as u64;

  if minimum_bit_size <= 8 {
    ast::IntSize::I8
  } else if minimum_bit_size <= 16 {
    ast::IntSize::I16
  } else if minimum_bit_size <= 32 {
    ast::IntSize::I32
  } else if minimum_bit_size <= 64 {
    ast::IntSize::I64
  } else {
    panic!("expected minimum bit-size to be smaller than 64");
  }
}

// REVIEW: Can't we use this to determine whether a token is an operator or not?
fn get_token_precedence(token: &lexer::TokenKind) -> usize {
  // FIXME: What about the `not` operator, and others?
  match token {
    lexer::TokenKind::Plus
    | lexer::TokenKind::Minus
    | lexer::TokenKind::Equal
    | lexer::TokenKind::LessThan
    | lexer::TokenKind::GreaterThan
    | lexer::TokenKind::And
    | lexer::TokenKind::Or
    | lexer::TokenKind::Nand
    | lexer::TokenKind::Nor
    | lexer::TokenKind::Xor => 1,
    lexer::TokenKind::Asterisk | lexer::TokenKind::Slash => 2,
    _ => 0,
  }
}

type ParserResult<T> = Result<T, diagnostic::Diagnostic>;

pub struct Parser<'a> {
  tokens: Vec<lexer::Token>,
  index: usize,
  cache: &'a mut cache::Cache,
}

impl<'a> Parser<'a> {
  pub fn new(tokens: Vec<lexer::Token>, cache: &'a mut cache::Cache) -> Self {
    Self {
      tokens,
      index: 0,
      cache,
    }
  }

  pub fn from_tokens(token_kinds: Vec<lexer::TokenKind>, cache: &'a mut cache::Cache) -> Self {
    let tokens = token_kinds
      .iter()
      .map(|kind| (kind.to_owned(), 0 as usize))
      .collect::<Vec<_>>();

    Self::new(tokens, cache)
  }

  // REVIEW: Consider removing the `token` parameter, and adjust the binary expression parsing function accordingly. Or is a better decision to have it the other way around?
  /// Determine whether the given token is considered a valid binary
  /// operator.
  fn is_binary_operator(token_kind: &lexer::TokenKind) -> bool {
    // TODO: Support for GTOE and LTOE operators.
    matches!(
      token_kind,
      lexer::TokenKind::Plus
        | lexer::TokenKind::Minus
        | lexer::TokenKind::Asterisk
        | lexer::TokenKind::Slash
        | lexer::TokenKind::LessThan
        | lexer::TokenKind::GreaterThan
        | lexer::TokenKind::And
        | lexer::TokenKind::Or
        | lexer::TokenKind::Nand
        | lexer::TokenKind::Nor
        | lexer::TokenKind::Xor
        | lexer::TokenKind::Equal
    )
  }

  /// Parse all top-level definitions.
  pub fn parse_all(&mut self) -> ParserResult<Vec<ast::Node>> {
    let mut result = Vec::new();

    while !self.is_eof() {
      result.push(self.parse_root_node()?);

      // FIXME: Nothing being done to the parsed top-level node.
    }

    Ok(result)
  }

  fn close_span(&self, span_start: usize) -> diagnostic::Span {
    span_start..self.index
  }

  fn skip_past(&mut self, token_kind: &lexer::TokenKind) -> ParserResult<()> {
    if !self.is(token_kind) {
      return Err(self.expected(format!("token `{}`", token_kind).as_str()));
    }

    self.try_skip();

    Ok(())
  }

  fn expected(&self, expected: &str) -> diagnostic::Diagnostic {
    diagnostic::Diagnostic {
      // TODO: Unsafe access. Default to `EOF` if there isn't a current token.
      message: format!("expected {}, but got `{}`", expected, self.force_get()),
      severity: diagnostic::Severity::Error,
      span: self.get_span(),
    }
  }

  fn is_unary_operator(&self) -> bool {
    matches!(
      self.force_get(),
      lexer::TokenKind::Minus
        | lexer::TokenKind::Bang
        | lexer::TokenKind::Ampersand
        | lexer::TokenKind::Asterisk
        | lexer::TokenKind::Backtick
    )
  }

  fn until(&self, token: &lexer::TokenKind) -> ParserResult<bool> {
    // TODO: Handle `EOF` case here.
    return Ok(!self.is_eof() && !self.is(token));
  }

  fn force_get(&self) -> &lexer::TokenKind {
    // REVIEW: Accessing index unsafely. How about using `ParserResult<&lexer::TokenKind>`?
    &self.tokens.get(self.index).unwrap().0
  }

  fn get_span(&self) -> Option<diagnostic::Span> {
    // REVIEW: Safety check?
    let position = self.tokens[self.index].1;

    Some(position..position)
  }

  /// Compare the current token to the given token.
  ///
  /// If `EOF` has been reached, `false` will always be returned. This is
  /// to avoid infinite loops that rely on this check as their condition.
  fn is(&self, token: &lexer::TokenKind) -> bool {
    if self.index >= self.tokens.len() {
      return false;
    }

    self.force_get() == token
  }

  // REVIEW: Consider returning a `ParserResult<bool>` instead.
  // TODO: Need testing for this function.
  /// Attempt to reposition the index to the next token (if any).
  ///
  /// Returns whether the index was repositioned. If false, it indicates
  /// that the end of file was reached (`EOF`), and the current character
  /// cannot be skipped-over.
  fn try_skip(&mut self) -> bool {
    if self.is_eof() {
      return false;
    }

    self.index += 1;

    true
  }

  /// Retrieve the upcoming token (if any).
  fn peek(&self) -> Option<&lexer::TokenKind> {
    match self.tokens.get(self.index + 1) {
      Some(value) => Some(&value.0),
      None => None,
    }
  }

  /// Compare the upcoming token to the given token.
  ///
  /// Will always return false if `EOF` has been reached.
  fn peek_is(&self, token: &lexer::TokenKind) -> bool {
    let next_token = self.peek();

    if next_token.is_none() {
      return false;
    }

    token == next_token.unwrap()
  }

  /// Whether the parser has reached the end of the input.
  ///
  /// Will return true if the tokens vector provided is empty,
  /// or if the index is at the end of the tokens vector.
  pub fn is_eof(&self) -> bool {
    self.tokens.is_empty() || self.index == self.tokens.len() - 1
  }

  fn parse_pattern(
    &mut self,
    symbol_kind: name_resolution::SymbolKind,
  ) -> ParserResult<ast::Pattern> {
    let starting_name = self.parse_name()?;

    let module_name = if self.is(&lexer::TokenKind::Colon) {
      self.try_skip();

      Some(starting_name.clone())
    } else {
      None
    };

    let base_name = if module_name.is_none() {
      starting_name
    } else {
      self.parse_name()?
    };

    Ok(ast::Pattern {
      module_name,
      base_name,
      symbol_kind,
      target_id: None,
    })
  }

  fn parse_name(&mut self) -> ParserResult<String> {
    // REVIEW: Illegal/unrecognized tokens MAY also be represented under 'Identifier'? Is this a problem?

    let name = match self.force_get() {
      lexer::TokenKind::Identifier(value) => value.clone(),
      _ => return Err(self.expected("identifier")),
    };

    self.try_skip();

    Ok(name)
  }

  fn parse_statement(&mut self) -> ParserResult<ast::Node> {
    let span_start = self.index;

    let kind = match self.force_get() {
      lexer::TokenKind::Return => ast::NodeKind::ReturnStmt(self.parse_return_stmt()?),
      lexer::TokenKind::Let => ast::NodeKind::LetStmt(self.parse_let_stmt()?),
      lexer::TokenKind::Loop => ast::NodeKind::LoopStmt(self.parse_loop_stmt()?),
      lexer::TokenKind::Break => ast::NodeKind::BreakStmt(self.parse_break_stmt()?),
      lexer::TokenKind::Continue => ast::NodeKind::ContinueStmt(self.parse_continue_stmt()?),
      lexer::TokenKind::Unsafe => ast::NodeKind::UnsafeBlock(self.parse_unsafe_block_stmt()?),
      _ => {
        let expr = self.parse_expr()?;

        // Promote the inline expression to an assignment statement, if applicable.
        if self.is(&lexer::TokenKind::Equal) {
          ast::NodeKind::AssignStmt(self.parse_assign_stmt(expr)?)
        } else {
          ast::NodeKind::InlineExprStmt(ast::InlineExprStmt {
            expr: Box::new(expr),
          })
        }
      }
    };

    Ok(ast::Node {
      kind,
      span: self.close_span(span_start),
    })
  }

  /// {'{' (%statement+) '}' | '=' {%statement | %expr}}
  fn parse_block_expr(&mut self) -> ParserResult<ast::BlockExpr> {
    // TODO: Simplify.
    // Support for short syntax.
    if self.is(&lexer::TokenKind::Equal) {
      self.try_skip();

      let statement = self.parse_statement()?;

      // REVIEW: Is this correct for all cases?
      let yield_last_expr = if self.is(&lexer::TokenKind::SemiColon) {
        self.try_skip();

        false
      } else {
        true
      };

      // TODO: Must ensure a semi-colon always follows (for if statements, loops, etc.)?
      return Ok(ast::BlockExpr {
        statements: vec![statement],
        yields_last_expr: yield_last_expr,
        unique_id: self.cache.create_unique_id(),
      });
    }

    self.skip_past(&lexer::TokenKind::BraceL)?;

    let mut statements = Vec::new();
    let mut yield_last_expr = true;

    // REVIEW: What if the last statement is a let-statement? Let-statements have inferrable types, used internally.
    // REVIEW: This may be the reason why let-statements shouldn't have inferrable types (only internally).
    while self.until(&lexer::TokenKind::BraceR)? {
      let statement = self.parse_statement()?;

      if self.peek_is(&lexer::TokenKind::BraceR) && self.is(&lexer::TokenKind::SemiColon) {
        self.try_skip();
        yield_last_expr = false;
      }

      statements.push(statement);
    }

    self.skip_past(&lexer::TokenKind::BraceR)?;

    Ok(ast::BlockExpr {
      statements,
      yields_last_expr: yield_last_expr,
      unique_id: self.cache.create_unique_id(),
    })
  }

  /// {u8 | u16 | u32 | u64 | i8 | i16 | i32 | i64}
  fn parse_int_type(&mut self) -> ParserResult<ast::Type> {
    let current_token = self.force_get();

    let size = match current_token {
      lexer::TokenKind::TypeInt8 => ast::IntSize::I8,
      lexer::TokenKind::TypeInt16 => ast::IntSize::I16,
      lexer::TokenKind::TypeInt32 => ast::IntSize::I32,
      lexer::TokenKind::TypeInt64 => ast::IntSize::I64,
      lexer::TokenKind::TypeUint8 => ast::IntSize::U8,
      lexer::TokenKind::TypeUint16 => ast::IntSize::U16,
      lexer::TokenKind::TypeUint32 => ast::IntSize::U32,
      lexer::TokenKind::TypeUint64 => ast::IntSize::U64,
      // TODO: Add unsigned type tokens.
      _ => return Err(self.expected("integer type")),
    };

    self.try_skip();

    Ok(ast::Type::Basic(ast::BasicType::Int(size)))
  }

  // REVIEW: Merge with the `parse_type` function (too small)?
  /// bool
  fn parse_bool_type(&mut self) -> ParserResult<ast::Type> {
    self.skip_past(&lexer::TokenKind::TypeBool)?;

    Ok(ast::Type::Basic(ast::BasicType::Bool))
  }

  // TODO: Specialize return types of the `parse_type_x` functions to their actual types instead of the `ast::Type` enum wrapper?
  fn parse_this_type(&mut self) -> ParserResult<ast::Type> {
    self.skip_past(&lexer::TokenKind::TypeThis)?;

    Ok(ast::Type::This(ast::ThisType {
      target_id: None,
      ty: None,
    }))
  }

  /// '[' %type, 0-9+ ']'
  fn parse_array_type(&mut self) -> ParserResult<ast::Type> {
    self.skip_past(&lexer::TokenKind::BracketL)?;

    let element_type = self.parse_type()?;

    self.skip_past(&lexer::TokenKind::Comma)?;

    let size = match self.force_get() {
      lexer::TokenKind::Int(value) => value.clone() as u32,
      _ => return Err(self.expected("array size")),
    };

    self.try_skip();
    self.skip_past(&lexer::TokenKind::BracketR)?;

    Ok(ast::Type::Array(Box::new(element_type), size))
  }

  fn parse_type(&mut self) -> ParserResult<ast::Type> {
    // REVISE: Unsafe access.
    // TODO: Support for more types.
    let mut ty = match self.force_get() {
      // TODO: Other types as well.
      // TODO: Parse function types.
      // lexer::TokenKind::Fn => self.parse_callable_type(),
      lexer::TokenKind::TypeInt8
      | lexer::TokenKind::TypeInt16
      | lexer::TokenKind::TypeInt32
      | lexer::TokenKind::TypeInt64
      | lexer::TokenKind::TypeUint8
      | lexer::TokenKind::TypeUint16
      | lexer::TokenKind::TypeUint32
      | lexer::TokenKind::TypeUint64 => self.parse_int_type(),
      lexer::TokenKind::TypeBool => self.parse_bool_type(),
      lexer::TokenKind::Identifier(_) => self.parse_stub_type(),
      lexer::TokenKind::BracketL => self.parse_array_type(),
      lexer::TokenKind::TypeUnit => self.parse_unit_type(),
      lexer::TokenKind::Asterisk => {
        self.try_skip();

        Ok(ast::Type::Pointer(Box::new(self.parse_type()?)))
      }
      lexer::TokenKind::TypeString => {
        self.try_skip();

        Ok(ast::Type::Basic(ast::BasicType::String))
      }
      _ => return Err(self.expected("type")),
    }?;

    // Upgrade to a function type, if applicable.
    if self.is(&lexer::TokenKind::Arrow) {
      ty = self.parse_function_type(ty)?;
    }

    Ok(ty)
  }

  fn parse_unit_type(&mut self) -> ParserResult<ast::Type> {
    // REVIEW: Do we need to ensure that nothing is applied to the unit type?

    self.skip_past(&lexer::TokenKind::TypeUnit)?;

    Ok(ast::Type::Unit)
  }

  /// fn '(' (%type (','))* ')' ('[' %type ']')
  fn parse_function_type(&mut self, first_type: ast::Type) -> ParserResult<ast::Type> {
    self.skip_past(&lexer::TokenKind::Arrow)?;

    let mut parameter_types = vec![first_type, self.parse_type()?];

    while self.is(&lexer::TokenKind::Arrow) {
      self.try_skip();
      parameter_types.push(self.parse_type()?);
    }

    let return_type = parameter_types.remove(parameter_types.len() - 1);

    // Support for no parameters.
    if parameter_types.len() == 1 && parameter_types[0] == ast::Type::Unit {
      parameter_types.clear();
    }

    Ok(ast::Type::Function(ast::FunctionType {
      parameter_types,
      return_type: Box::new(return_type),
      // TODO: Support for variadic functions types? Such as a reference to an extern that is variadic? Think/investigate. Remember that externs may only be invoked from unsafe blocks.
      is_variadic: false,
    }))
  }

  /// %name
  fn parse_stub_type(&mut self) -> ParserResult<ast::Type> {
    let name = self.parse_name()?;

    Ok(ast::Type::Stub(ast::StubType {
      name,
      target_id: None,
    }))
  }

  /// %name ':' %type
  fn parse_parameter(&mut self, position: u32) -> ParserResult<ast::Parameter> {
    let name = self.parse_name()?;

    self.skip_past(&lexer::TokenKind::Colon)?;

    let ty = self.parse_type()?;

    Ok(ast::Parameter {
      name,
      ty,
      position,
      unique_id: self.cache.create_unique_id(),
    })
  }

  /// '(' {%parameter* (,)} (+) ')' ':' %type
  fn parse_prototype(&mut self) -> ParserResult<ast::Prototype> {
    self.skip_past(&lexer::TokenKind::ParenthesesL)?;

    let mut parameters = vec![];
    let mut is_variadic = false;
    let mut parameter_index_counter = 0;
    let mut accepts_instance = false;
    let mut this_parameter = None;

    if self.is(&lexer::TokenKind::Identifier(THIS_IDENTIFIER.to_string())) {
      self.try_skip();
      parameter_index_counter += 1;
      accepts_instance = true;

      this_parameter = Some(ast::Parameter {
        name: THIS_IDENTIFIER.to_string(),
        ty: ast::Type::This(ast::ThisType {
          target_id: None,
          ty: None,
        }),
        position: 0,
        unique_id: self.cache.create_unique_id(),
      });

      if !self.is(&lexer::TokenKind::ParenthesesR) {
        self.skip_past(&lexer::TokenKind::Comma)?;
      }
    }

    // REVISE: Analyze, and remove possibility of lonely comma.
    while self.until(&lexer::TokenKind::ParenthesesR)? {
      if self.is(&lexer::TokenKind::Plus) {
        is_variadic = true;
        self.try_skip();

        break;
      }

      parameters.push(self.parse_parameter(parameter_index_counter)?);
      parameter_index_counter += 1;

      if !self.is(&lexer::TokenKind::Comma) {
        break;
      }

      self.try_skip();
    }

    self.skip_past(&lexer::TokenKind::ParenthesesR)?;
    self.skip_past(&lexer::TokenKind::Colon)?;

    let return_type = self.parse_type()?;

    Ok(ast::Prototype {
      parameters,
      return_type,
      is_variadic,
      accepts_instance,
      instance_type_id: None,
      this_parameter,
    })
  }

  /// fn %prototype %block
  fn parse_function(&mut self, attributes: Vec<ast::Attribute>) -> ParserResult<ast::Function> {
    // TODO: Support for visibility.
    self.skip_past(&lexer::TokenKind::Fn)?;

    let name = self.parse_name()?;
    let prototype = self.parse_prototype()?;

    self.skip_past(&lexer::TokenKind::Equal)?;

    let value = self.parse_expr()?;

    Ok(ast::Function {
      name,
      prototype,
      body_value: Box::new(value),
      attributes,
      unique_id: self.cache.create_unique_id(),
    })
  }

  /// extern fn %prototype
  fn parse_extern_function(
    &mut self,
    attributes: Vec<ast::Attribute>,
  ) -> ParserResult<ast::ExternFunction> {
    // REVIEW: Support for visibility?

    self.skip_past(&lexer::TokenKind::Extern)?;
    self.skip_past(&lexer::TokenKind::Fn)?;

    let name = self.parse_name()?;
    let prototype = self.parse_prototype()?;

    Ok(ast::ExternFunction {
      name,
      prototype,
      attributes,
      unique_id: self.cache.create_unique_id(),
    })
  }

  /// extern static %name ':' %type
  fn parse_extern_static(&mut self) -> ParserResult<ast::ExternStatic> {
    self.skip_past(&lexer::TokenKind::Extern)?;
    self.skip_past(&lexer::TokenKind::Static)?;

    let name = self.parse_name()?;

    self.skip_past(&lexer::TokenKind::Colon)?;

    let ty = self.parse_type()?;

    Ok(ast::ExternStatic {
      name,
      ty,
      unique_id: self.cache.create_unique_id(),
    })
  }

  /// '@' %name ('(' (%literal (','))* ')')
  fn parse_attribute(&mut self) -> ParserResult<ast::Attribute> {
    self.skip_past(&lexer::TokenKind::At)?;

    let name = self.parse_name()?;
    let mut values = Vec::new();

    if self.is(&lexer::TokenKind::ParenthesesL) {
      self.try_skip();

      while self.until(&lexer::TokenKind::ParenthesesR)? {
        values.push(self.parse_literal()?);

        // REVIEW: Review this comma-skipping system.
        if !self.is(&lexer::TokenKind::Comma) {
          break;
        }

        self.try_skip();
      }

      self.skip_past(&lexer::TokenKind::ParenthesesR)?;
    }

    Ok(ast::Attribute { name, values })
  }

  /// {%function | %extern_function | %extern_static | %type_alias | %enum | %struct_type}
  fn parse_root_node(&mut self) -> ParserResult<ast::Node> {
    // REVIEW: Why not move this check into the `get()` method?
    if self.is_eof() {
      return Err(diagnostic::Diagnostic {
        message: "expected top-level construct but got end of file".to_string(),
        severity: diagnostic::Severity::Error,
        span: self.get_span(),
      });
    }

    let node_span_start = self.index;
    let mut attributes: Vec<ast::Attribute> = Vec::new();

    while self.is(&lexer::TokenKind::At) {
      let attribute_span_start = self.index;
      let attribute = self.parse_attribute()?;

      if attributes
        .iter()
        .any(|attribute| attribute.name == attribute.name)
      {
        return Err(diagnostic::Diagnostic {
          message: format!("duplicate attribute `{}`", attribute.name),
          severity: diagnostic::Severity::Error,
          span: Some(self.close_span(attribute_span_start)),
        });
      }

      attributes.push(attribute);
    }

    let is_attributable = matches!(
      self.force_get(),
      lexer::TokenKind::Fn | lexer::TokenKind::Extern
    );

    if !attributes.is_empty() && !is_attributable {
      return Err(diagnostic::Diagnostic {
        message: "attributes may only be attached to functions or externs".to_string(),
        severity: diagnostic::Severity::Error,
        span: self.get_span(),
      });
    }

    let token = self.force_get();

    let kind = match token {
      lexer::TokenKind::Fn => ast::NodeKind::Function(self.parse_function(attributes)?),
      lexer::TokenKind::Extern if self.peek_is(&lexer::TokenKind::Fn) => {
        ast::NodeKind::ExternFunction(self.parse_extern_function(attributes)?)
      }
      lexer::TokenKind::Extern if self.peek_is(&lexer::TokenKind::Static) => {
        ast::NodeKind::ExternStatic(self.parse_extern_static()?)
      }
      lexer::TokenKind::Enum => ast::NodeKind::Enum(self.parse_enum()?),
      lexer::TokenKind::Struct => ast::NodeKind::StructType(self.parse_struct_type()?),
      lexer::TokenKind::Type => ast::NodeKind::TypeAlias(self.parse_type_alias()?),
      lexer::TokenKind::Impl => ast::NodeKind::StructImpl(self.parse_struct_impl()?),
      lexer::TokenKind::Trait => ast::NodeKind::Trait(self.parse_trait()?),
      _ => return Err(self.expected("top-level construct")),
    };

    Ok(ast::Node {
      kind,
      span: self.close_span(node_span_start),
    })
  }

  /// type %name = %type
  fn parse_type_alias(&mut self) -> ParserResult<ast::TypeAlias> {
    self.skip_past(&lexer::TokenKind::Type)?;

    let name = self.parse_name()?;

    self.skip_past(&lexer::TokenKind::Equal)?;

    // BUG: Recursive type aliases are possible, and cause stack-overflow.
    // ... Fix this bug. This bug possibly exist for any other thing that uses deferred-resolved types.
    let ty = self.parse_type()?;

    Ok(ast::TypeAlias {
      name,
      ty,
      unique_id: self.cache.create_unique_id(),
    })
  }

  /// return (%expr)
  fn parse_return_stmt(&mut self) -> ParserResult<ast::ReturnStmt> {
    self.skip_past(&lexer::TokenKind::Return)?;

    let value = if !self.is(&lexer::TokenKind::SemiColon) {
      Some(Box::new(self.parse_expr()?))
    } else {
      self.try_skip();

      None
    };

    Ok(ast::ReturnStmt { value })
  }

  /// let (mut) %name (':' %type) '=' %expr
  fn parse_let_stmt(&mut self) -> ParserResult<ast::LetStmt> {
    self.skip_past(&lexer::TokenKind::Let)?;

    let is_mutable = if self.is(&lexer::TokenKind::Mut) {
      self.try_skip();

      true
    } else {
      false
    };

    let name = self.parse_name()?;

    self.skip_past(&lexer::TokenKind::Colon)?;

    let ty = self.parse_type()?;

    self.skip_past(&lexer::TokenKind::Equal)?;

    let value = self.parse_expr()?;

    // TODO: Value should be treated as rvalue, unless its using an address-of operator. Find out how to translate this to logic.

    Ok(ast::LetStmt {
      name,
      ty,
      value: Box::new(value),
      is_mutable,
      unique_id: self.cache.create_unique_id(),
    })
  }

  /// if %expr %block (else %block)
  fn parse_if_expr(&mut self) -> ParserResult<ast::IfExpr> {
    self.skip_past(&lexer::TokenKind::If)?;

    let condition = self.parse_expr()?;

    self.skip_past(&lexer::TokenKind::Then)?;

    let then_value = self.parse_expr()?;

    let else_value = if self.is(&lexer::TokenKind::Else) {
      self.try_skip();

      Some(Box::new(self.parse_expr()?))
    } else {
      None
    };

    Ok(ast::IfExpr {
      condition: Box::new(condition),
      then_value: Box::new(then_value),
      else_value,
    })
  }

  /// loop (%expr) %block
  fn parse_loop_stmt(&mut self) -> ParserResult<ast::LoopStmt> {
    self.skip_past(&lexer::TokenKind::Loop)?;

    let condition = if self.is(&lexer::TokenKind::BraceL) {
      None
    } else {
      Some(Box::new(self.parse_expr()?))
    };

    let body = self.parse_block_expr()?;

    Ok(ast::LoopStmt { condition, body })
  }

  /// break
  fn parse_break_stmt(&mut self) -> ParserResult<ast::BreakStmt> {
    self.skip_past(&lexer::TokenKind::Break)?;

    Ok(ast::BreakStmt {})
  }

  /// continue
  fn parse_continue_stmt(&mut self) -> ParserResult<ast::ContinueStmt> {
    self.skip_past(&lexer::TokenKind::Continue)?;

    Ok(ast::ContinueStmt)
  }

  // unsafe %block
  fn parse_unsafe_block_stmt(&mut self) -> ParserResult<ast::UnsafeBlockStmt> {
    // REVIEW: Why not merge this with the normal block, and just have a flag?

    self.skip_past(&lexer::TokenKind::Unsafe)?;

    Ok(ast::UnsafeBlockStmt(self.parse_block_expr()?))
  }

  /// {true | false}
  fn parse_bool_literal(&mut self) -> ParserResult<ast::Literal> {
    Ok(match self.force_get().clone() {
      lexer::TokenKind::Bool(value) => {
        self.try_skip();

        ast::Literal::Bool(value.clone())
      }
      _ => return Err(self.expected("boolean literal")),
    })
  }

  /// 0-9+
  fn parse_int_literal(&mut self) -> ParserResult<ast::Literal> {
    Ok(match self.force_get().clone() {
      lexer::TokenKind::Int(value) => {
        self.try_skip();

        let minimum_size = minimum_int_size_of(&value);

        // REVIEW: Deal with unsigned integers here?
        // Default size to 32 bit-width.
        let size = if minimum_size < ast::IntSize::I32 {
          ast::IntSize::I32
        } else {
          minimum_size
        };

        ast::Literal::Int(value, size)
      }
      _ => return Err(self.expected("integer literal")),
    })
  }

  /// '"' [^"]* '"'
  fn parse_string_literal(&mut self) -> ParserResult<ast::Literal> {
    let result = match self.force_get() {
      lexer::TokenKind::String(value) => ast::Literal::String(value.clone()),
      _ => return Err(self.expected("string literal")),
    };

    self.try_skip();

    Ok(result)
  }

  /// '[' (%expr (','))* ']'
  fn parse_array_value(&mut self) -> ParserResult<ast::ArrayValue> {
    let mut elements = Vec::new();

    self.skip_past(&lexer::TokenKind::BracketL)?;

    while self.until(&lexer::TokenKind::BracketR)? {
      elements.push(self.parse_expr()?);

      // REVIEW: What if the comma isn't provided?
      if self.is(&lexer::TokenKind::Comma) {
        self.try_skip();
      }
    }

    // Skip the closing bracket.
    self.try_skip();

    // TODO: In the future, the type of an empty array will be inferred.
    let explicit_type = if elements.is_empty() {
      Some(self.parse_type()?)
    } else {
      None
    };

    Ok(ast::ArrayValue {
      elements,
      explicit_type,
    })
  }

  /// %name '[' %expr ']'
  fn parse_array_indexing(&mut self) -> ParserResult<ast::ArrayIndexing> {
    // TODO: Work with a pattern instead.
    let name = self.parse_name()?;

    self.skip_past(&lexer::TokenKind::BracketL)?;

    let index = Box::new(self.parse_expr()?);

    self.skip_past(&lexer::TokenKind::BracketR)?;

    Ok(ast::ArrayIndexing {
      name,
      index_expr: index,
      target_id: None,
    })
  }

  fn parse_literal(&mut self) -> ParserResult<ast::Literal> {
    let current_token = self.force_get();

    Ok(match current_token {
      lexer::TokenKind::Bool(_) => self.parse_bool_literal()?,
      lexer::TokenKind::Int(_) => self.parse_int_literal()?,
      lexer::TokenKind::String(_) => self.parse_string_literal()?,
      lexer::TokenKind::Nullptr => {
        self.try_skip();

        ast::Literal::Nullptr
      }
      _ => return Err(self.expected("literal")),
    })
  }

  // TODO: Retire this function. Remove it once its no longer needed.
  fn after_pattern_is(&self, token: &lexer::TokenKind) -> bool {
    let mut index = self.index;
    let mut delimiter_switch = false;
    let is_past_eof_at = |index: usize| index >= self.tokens.len();

    // REVISE: This is hacky code. Fix up.
    // REVIEW: Ensure this works as expected with the modifications done.
    while !is_past_eof_at(index)
      && match self.tokens.get(index).unwrap().0 {
        lexer::TokenKind::Dot | lexer::TokenKind::Colon if delimiter_switch => true,
        lexer::TokenKind::Identifier(_) if !delimiter_switch => true,
        _ => false,
      }
    {
      delimiter_switch = !delimiter_switch;
      index += 1;
    }

    if let Some(token_at_index) = self.tokens.get(index) {
      &token_at_index.0 == token
    } else {
      false
    }
  }

  // TODO: Make use-of, or dispose.
  fn is_chain(&self) -> bool {
    if self.is_eof() {
      return false;
    }

    matches!(
      self.force_get(),
      lexer::TokenKind::Dot | lexer::TokenKind::ParenthesesL
    )
  }

  fn parse_primary_expr(&mut self) -> ParserResult<ast::Node> {
    let span_start = self.index;

    let kind = match self.force_get() {
      // REVIEW: Possible redundant check after the fn keyword. But how do we know we're still not on a block and accidentally parse a function as a closure?
      lexer::TokenKind::Fn
        if (self.peek_is(&lexer::TokenKind::BracketL)
          || self.peek_is(&lexer::TokenKind::ParenthesesL)) =>
      {
        ast::NodeKind::Closure(self.parse_closure()?)
      }
      lexer::TokenKind::If => ast::NodeKind::IfStmt(self.parse_if_expr()?),
      lexer::TokenKind::Tilde => ast::NodeKind::IntrinsicCall(self.parse_intrinsic_call()?),
      // REVISE: Change this syntax to the same treatment as call expressions (check afterwards).
      lexer::TokenKind::Identifier(_) if self.after_pattern_is(&lexer::TokenKind::BracketL) => {
        ast::NodeKind::ArrayIndexing(self.parse_array_indexing()?)
      }
      lexer::TokenKind::Identifier(_) => ast::NodeKind::Reference(self.parse_reference()?),
      lexer::TokenKind::BracketL => ast::NodeKind::ArrayValue(self.parse_array_value()?),
      lexer::TokenKind::New => ast::NodeKind::StructValue(self.parse_struct_value()?),
      lexer::TokenKind::BraceL => ast::NodeKind::BlockExpr(self.parse_block_expr()?),
      _ if self.is_unary_operator() => ast::NodeKind::UnaryExpr(self.parse_unary_expr()?),
      // Default to a literal if nothing else matched.
      _ => ast::NodeKind::Literal(self.parse_literal()?),
    };

    let mut node = ast::Node {
      kind,
      span: self.close_span(span_start),
    };

    // Promote the node to a chain, if applicable.
    while self.is_chain() {
      let kind = match self.force_get() {
        lexer::TokenKind::ParenthesesL => ast::NodeKind::CallExpr(self.parse_call_expr(node)?),
        lexer::TokenKind::Dot => ast::NodeKind::MemberAccess(self.parse_member_access(node)?),
        _ => unreachable!(),
      };

      // REVIEW: Simplify (DRY)?
      node = ast::Node {
        kind,
        span: self.close_span(span_start),
      };
    }

    Ok(node)
  }

  /// %expr '.' %name
  fn parse_member_access(&mut self, base_expr: ast::Node) -> ParserResult<ast::MemberAccess> {
    self.skip_past(&lexer::TokenKind::Dot)?;

    Ok(ast::MemberAccess {
      base_expr: Box::new(base_expr),
      member_name: self.parse_name()?,
    })
  }

  /// {'+' | '-' | '*' | '/'}
  fn parse_operator(&mut self) -> ParserResult<ast::OperatorKind> {
    // TODO: Unsafe access. Also, cloning token.
    let current_token = self.force_get().clone();

    let operator = match current_token {
      lexer::TokenKind::And => ast::OperatorKind::And,
      lexer::TokenKind::Or => ast::OperatorKind::Or,
      lexer::TokenKind::Nand => ast::OperatorKind::Nand,
      lexer::TokenKind::Nor => ast::OperatorKind::Nor,
      lexer::TokenKind::Xor => ast::OperatorKind::Xor,
      lexer::TokenKind::Bang => ast::OperatorKind::Not,
      lexer::TokenKind::Plus => ast::OperatorKind::Add,
      lexer::TokenKind::Minus => ast::OperatorKind::SubtractOrNegate,
      lexer::TokenKind::Asterisk => ast::OperatorKind::MultiplyOrDereference,
      lexer::TokenKind::Slash => ast::OperatorKind::Divide,
      lexer::TokenKind::LessThan => ast::OperatorKind::LessThan,
      lexer::TokenKind::GreaterThan => ast::OperatorKind::GreaterThan,
      lexer::TokenKind::Ampersand => ast::OperatorKind::AddressOf,
      lexer::TokenKind::Backtick => ast::OperatorKind::Cast,
      lexer::TokenKind::Equal => ast::OperatorKind::Equality,
      // TODO: Implement logic for GTE & LTE.
      _ => return Err(self.expected("operator")),
    };

    self.try_skip();

    Ok(operator)
  }

  // REVISE: Move to use the Pratt-parsing technique instead to replace the non-tail recursive method.
  /// %expr %operator %expr
  fn parse_binary_expr_or_default(
    &mut self,
    left: ast::Node,
    min_precedence: usize,
  ) -> ParserResult<ast::Node> {
    let span_start = self.index;
    let mut token_buffer = self.force_get();
    let precedence = get_token_precedence(&token_buffer);
    let mut buffer = left;

    while Parser::is_binary_operator(token_buffer) && (precedence > min_precedence) {
      let operator = self.parse_operator()?;
      let mut right = self.parse_primary_expr()?;

      token_buffer = self.force_get();

      while Parser::is_binary_operator(&token_buffer)
        && get_token_precedence(&token_buffer) > precedence
      {
        // REVIEW: Are we adding the correct amount of precedence here? Shouldn't there be a higher difference in precedence?
        // REVIEW: This isn't tail-recursive?
        right = self.parse_binary_expr_or_default(right, precedence + 1)?;
        token_buffer = self.force_get();
      }

      let kind = ast::NodeKind::BinaryExpr(ast::BinaryExpr {
        left: Box::new(buffer),
        operator,
        right: Box::new(right),
      });

      buffer = ast::Node {
        kind,
        span: self.close_span(span_start),
      };
    }

    Ok(buffer)
  }

  /// %operator %expr
  fn parse_unary_expr(&mut self) -> ParserResult<ast::UnaryExpr> {
    if !self.is_unary_operator() {
      return Err(self.expected("unary operator"));
    }

    let operator = self.parse_operator()?;

    let cast_type = if operator == ast::OperatorKind::Cast {
      Some(self.parse_type()?)
    } else {
      None
    };

    let expr = Box::new(self.parse_expr()?);

    // TODO:
    // if !matches!(operator, ast::OperatorKind::AddressOf) {
    //   expr.as_rvalue = true;
    // }

    Ok(ast::UnaryExpr {
      operator,
      expr,
      cast_type,
    })
  }

  // REVISE: Better naming and/or positioning for logic.
  fn parse_expr(&mut self) -> ParserResult<ast::Node> {
    // TODO: Add support for unary expressions (such as `!`, '-', etc.).

    let starting_expr = self.parse_primary_expr()?;

    // REVIEW: Should the precedence be zero here?
    Ok(self.parse_binary_expr_or_default(starting_expr, 0)?)
  }

  /// %expr '(' (%expr (,))* ')'
  fn parse_call_expr(&mut self, callee_expr: ast::Node) -> ParserResult<ast::CallExpr> {
    // REVIEW: On the expressions being parsed, the pattern may not be parsed as a pattern linking to a function or extern. Ensure this is actually the case.
    // let callee_pattern = self.parse_pattern(name_resolution::SymbolKind::FunctionOrExtern)?;

    self.skip_past(&lexer::TokenKind::ParenthesesL)?;

    let mut arguments = vec![];

    while self.until(&lexer::TokenKind::ParenthesesR)? {
      arguments.push(self.parse_expr()?);

      // REVIEW: What if the comma is omitted?
      if self.is(&lexer::TokenKind::Comma) {
        self.try_skip();
      }
    }

    self.skip_past(&lexer::TokenKind::ParenthesesR)?;

    Ok(ast::CallExpr {
      callee_expr: Box::new(callee_expr),
      arguments,
    })
  }

  /// '~' %name '(' (%expr (,))* ')'
  fn parse_intrinsic_call(&mut self) -> ParserResult<ast::IntrinsicCall> {
    self.skip_past(&lexer::TokenKind::Tilde)?;

    let kind = match self.parse_name()?.as_str() {
      "panic" => ast::IntrinsicKind::Panic,
      _ => return Err(self.expected("valid intrinsic name")),
    };

    self.skip_past(&lexer::TokenKind::ParenthesesL)?;

    let mut arguments = vec![];

    while self.until(&lexer::TokenKind::ParenthesesR)? {
      arguments.push(self.parse_expr()?);

      // REVIEW: What if the comma is omitted?
      if self.is(&lexer::TokenKind::Comma) {
        self.try_skip();
      }
    }

    self.skip_past(&lexer::TokenKind::ParenthesesR)?;

    Ok(ast::IntrinsicCall { kind, arguments })
  }

  /// %pattern
  fn parse_reference(&mut self) -> ParserResult<ast::Reference> {
    let pattern = self.parse_pattern(name_resolution::SymbolKind::Definition)?;

    Ok(ast::Reference { pattern })
  }

  /// %expr '=' %expr
  fn parse_assign_stmt(&mut self, assignee_expr: ast::Node) -> ParserResult<ast::AssignStmt> {
    self.skip_past(&lexer::TokenKind::Equal)?;

    let value = Box::new(self.parse_expr()?);

    Ok(ast::AssignStmt {
      assignee_expr: Box::new(assignee_expr),
      value,
    })
  }

  /// enum %name '{' (%name (','))* '}'
  fn parse_enum(&mut self) -> ParserResult<ast::Enum> {
    self.skip_past(&lexer::TokenKind::Enum)?;

    let name = self.parse_name()?;

    self.skip_past(&lexer::TokenKind::BraceL)?;

    let mut variants = vec![];

    while self.until(&lexer::TokenKind::BraceR)? {
      variants.push(self.parse_name()?);

      // TODO: Iron out case for lonely comma.
      if self.is(&lexer::TokenKind::Comma) {
        self.try_skip();
      }
    }

    // REVIEW: What if we reach `EOF` before closing brace?

    // Skip the closing brace symbol.
    self.try_skip();

    Ok(ast::Enum {
      name,
      variants,
      unique_id: self.cache.create_unique_id(),
    })
  }

  /// struct %name '{' (%name ':' %type ';')* '}'
  fn parse_struct_type(&mut self) -> ParserResult<ast::StructType> {
    self.skip_past(&lexer::TokenKind::Struct)?;

    let name = self.parse_name()?;

    self.skip_past(&lexer::TokenKind::BraceL)?;

    let mut fields = Vec::new();

    while self.until(&lexer::TokenKind::BraceR)? {
      let field_name = self.parse_name()?;

      self.skip_past(&lexer::TokenKind::Colon)?;

      let field_type = self.parse_type()?;

      self.skip_past(&lexer::TokenKind::Comma)?;
      fields.push((field_name, field_type));
    }

    // Skip the closing brace symbol.
    self.try_skip();

    Ok(ast::StructType {
      name,
      fields,
      unique_id: self.cache.create_unique_id(),
    })
  }

  /// new %name '{' (%name ':' %expr ',')* '}'
  fn parse_struct_value(&mut self) -> ParserResult<ast::StructValue> {
    self.skip_past(&lexer::TokenKind::New)?;

    // REVIEW: Shouldn't it be `ScopeQualifier`?
    let struct_name = self.parse_name()?;

    self.skip_past(&lexer::TokenKind::BraceL)?;

    let mut fields = Vec::new();

    while self.until(&lexer::TokenKind::BraceR)? {
      let field = self.parse_expr()?;

      fields.push(field);

      // TODO: Disallow trailing comma.
      if self.is(&lexer::TokenKind::Comma) {
        self.try_skip();
      }
    }

    // Skip the closing brace symbol.
    self.try_skip();

    Ok(ast::StructValue {
      struct_name,
      fields,
      target_id: None,
    })
  }

  /// fn '[' (%name (','))* ']' %prototype %block
  fn parse_closure(&mut self) -> ParserResult<ast::Closure> {
    self.skip_past(&lexer::TokenKind::Fn)?;

    let mut captures = Vec::new();

    if self.is(&lexer::TokenKind::BracketL) {
      self.try_skip();

      while self.until(&lexer::TokenKind::BracketR)? {
        captures.push((self.parse_name()?, None));
      }

      self.try_skip();
    }

    let prototype = self.parse_prototype()?;
    let body = self.parse_block_expr()?;

    Ok(ast::Closure {
      captures,
      prototype,
      body,
    })
  }

  fn parse_struct_impl(&mut self) -> ParserResult<ast::StructImpl> {
    self.skip_past(&lexer::TokenKind::Impl)?;

    let mut struct_pattern = self.parse_pattern(name_resolution::SymbolKind::Type)?;
    let mut trait_pattern = None;

    if self.is(&lexer::TokenKind::For) {
      trait_pattern = Some(struct_pattern);
      self.try_skip();
      struct_pattern = self.parse_pattern(name_resolution::SymbolKind::Type)?;
    }

    self.skip_past(&lexer::TokenKind::BraceL)?;

    // TODO: Support for trait specifications/implementations.

    let mut methods = Vec::new();

    while self.until(&lexer::TokenKind::BraceR)? {
      // TODO: Support for attributes.
      let method = self.parse_function(Vec::new())?;

      methods.push(method);
    }

    self.skip_past(&lexer::TokenKind::BraceR)?;

    Ok(ast::StructImpl {
      // TODO: Support for trait specialization.
      is_default: false,
      target_struct_pattern: struct_pattern,
      trait_pattern,
      methods,
    })
  }

  fn parse_trait(&mut self) -> ParserResult<ast::Trait> {
    self.skip_past(&lexer::TokenKind::Trait)?;

    let name = self.parse_name()?;

    self.skip_past(&lexer::TokenKind::BraceL)?;

    let mut methods = Vec::new();

    while self.until(&lexer::TokenKind::BraceR)? {
      self.skip_past(&lexer::TokenKind::Fn)?;

      let method_name = self.parse_name()?;
      let prototype = self.parse_prototype()?;

      methods.push((method_name, prototype));
    }

    self.skip_past(&lexer::TokenKind::BraceR)?;

    Ok(ast::Trait {
      name,
      methods,
      unique_id: self.cache.create_unique_id(),
    })
  }
}

#[cfg(test)]
mod tests {
  use super::*;

  #[test]
  fn proper_initial_values() {
    let mut context = cache::Cache::new();
    let parser = Parser::new(vec![], &mut context);

    assert_eq!(0, parser.index);
  }

  #[test]
  fn is() {
    let mut context = cache::Cache::new();
    let mut parser = Parser::new(Vec::new(), &mut context);

    assert!(!parser.is(&lexer::TokenKind::EOF));
    parser.index = 1;
    assert!(!parser.is(&lexer::TokenKind::EOF));
    parser.index = 0;
    parser.tokens.push((lexer::TokenKind::Fn, 0));
    assert!(parser.is(&lexer::TokenKind::Fn));
  }

  #[test]
  fn is_empty() {
    let mut context = cache::Cache::new();
    let parser = Parser::new(vec![], &mut context);

    assert_eq!(false, parser.is(&lexer::TokenKind::Fn));
  }

  #[test]
  fn skip() {
    let mut context = cache::Cache::new();

    let mut parser = Parser::from_tokens(
      vec![lexer::TokenKind::Fn, lexer::TokenKind::Fn],
      &mut context,
    );

    parser.try_skip();
    assert_eq!(1, parser.index);
  }

  #[test]
  fn skip_out_of_bounds() {
    let mut context = cache::Cache::new();
    let mut parser = Parser::from_tokens(vec![lexer::TokenKind::Fn], &mut context);

    parser.try_skip();
    assert_eq!(0, parser.index);
  }

  #[test]
  fn is_eof() {
    let mut context = cache::Cache::new();
    let mut parser = Parser::new(vec![], &mut context);

    assert!(parser.is_eof());
    parser.tokens.push((lexer::TokenKind::Fn, 0));
    assert!(parser.is_eof());
    parser.tokens.push((lexer::TokenKind::Fn, 0));
    assert!(!parser.is_eof());
    parser.try_skip();
    assert!(parser.is_eof());
  }

  #[test]
  fn after_pattern_is() {
    let mut context = cache::Cache::new();

    let mut parser = Parser::new(
      vec![(lexer::TokenKind::Identifier("test".to_string()), 0)],
      &mut context,
    );

    parser.tokens.push((lexer::TokenKind::BraceL, 0));
    assert!(parser.after_pattern_is(&lexer::TokenKind::BraceL));
    parser.tokens.pop();
    parser.tokens.push((lexer::TokenKind::Dot, 0));

    parser
      .tokens
      .push((lexer::TokenKind::Identifier("foo".to_string()), 0));

    parser.tokens.push((lexer::TokenKind::BraceL, 0));
    assert!(parser.after_pattern_is(&lexer::TokenKind::BraceL));
  }

  #[test]
  fn is_binary_operator() {
    assert!(!Parser::is_binary_operator(&lexer::TokenKind::BraceL));
    assert!(Parser::is_binary_operator(&lexer::TokenKind::Plus));
    assert!(Parser::is_binary_operator(&lexer::TokenKind::Equal));
    assert!(Parser::is_binary_operator(&lexer::TokenKind::And));
    assert!(!Parser::is_binary_operator(&lexer::TokenKind::EOF));

    // TODO: More.
  }

  #[test]
  fn peek_is() {
    let mut context = cache::Cache::new();
    let mut parser = Parser::new(Vec::new(), &mut context);

    assert!(!parser.peek_is(&lexer::TokenKind::BraceL));
    parser.tokens.push((lexer::TokenKind::BraceL, 0));
    assert!(!parser.peek_is(&lexer::TokenKind::BraceL));
    parser.tokens.push((lexer::TokenKind::BraceL, 0));
    assert!(parser.peek_is(&lexer::TokenKind::BraceL));
  }

  // TODO: Add more tests.
}
