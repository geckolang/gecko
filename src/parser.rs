use crate::{ast, cache, diagnostic, lexer, name_resolution, type_check::TypeCheck};

macro_rules! skip_past {
  ($self:expr, $token:expr) => {
    if !$self.is($token) {
      return Err(diagnostic::Diagnostic {
        message: format!(
          "expected token `{}`, but got `{}`",
          $token, $self.tokens[$self.index].0
        ),
        severity: diagnostic::Severity::Error,
        location: $self.get_location(),
      });
    }

    $self.skip();
  };
}

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

// TODO: Can't we use this to determine whether a token is an operator or not?
fn get_token_precedence(token: &lexer::TokenKind) -> usize {
  // FIXME: What about the `not` operator, and others?
  match token {
    lexer::TokenKind::SymbolPlus
    | lexer::TokenKind::SymbolMinus
    | lexer::TokenKind::SymbolEqual
    | lexer::TokenKind::SymbolLessThan
    | lexer::TokenKind::SymbolGreaterThan
    | lexer::TokenKind::KeywordAnd
    | lexer::TokenKind::KeywordOr
    | lexer::TokenKind::KeywordNand
    | lexer::TokenKind::KeywordNor
    | lexer::TokenKind::KeywordXor => 1,
    lexer::TokenKind::SymbolAsterisk | lexer::TokenKind::SymbolSlash => 2,
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

  /// Parse all top-level definitions.
  pub fn parse_all(&mut self) -> ParserResult<Vec<ast::Node>> {
    let mut result = Vec::new();

    while !self.is_eof() {
      result.push(self.parse_top_level_node()?);

      // FIXME: Nothing being done to the parsed top-level node.
    }

    Ok(result)
  }

  fn expected(&self, expected: &str) -> diagnostic::Diagnostic {
    diagnostic::Diagnostic {
      message: format!("expected {}, but got `{}`", expected, self.get()),
      severity: diagnostic::Severity::Error,
      location: self.get_location(),
    }
  }

  // TODO: Consider removing the `token` parameter, and adjust the binary expression parsing function accordingly.
  /// Determine whether the given token is considered a valid binary
  /// operator.
  fn is_binary_operator(&self, token: &lexer::TokenKind) -> bool {
    // TODO: Attempt to simplify.
    // TODO: Support for GTOE and LTOE operators.
    matches!(
      token,
      lexer::TokenKind::SymbolPlus
        | lexer::TokenKind::SymbolMinus
        | lexer::TokenKind::SymbolAsterisk
        | lexer::TokenKind::SymbolSlash
        | lexer::TokenKind::SymbolLessThan
        | lexer::TokenKind::SymbolGreaterThan
        | lexer::TokenKind::KeywordAnd
        | lexer::TokenKind::KeywordOr
        | lexer::TokenKind::KeywordNand
        | lexer::TokenKind::KeywordNor
        | lexer::TokenKind::KeywordXor
    ) || matches!(token, lexer::TokenKind::SymbolEqual if self.peek_is(&lexer::TokenKind::SymbolEqual))
  }

  fn until(&self, token: &lexer::TokenKind) -> ParserResult<bool> {
    // TODO: Handle `EOF` case here.
    return Ok(!self.is_eof() && !self.is(token));
  }

  fn get(&self) -> &lexer::TokenKind {
    // TODO: Accessing index unsafely.
    &self.tokens[self.index].0
  }

  fn get_location(&self) -> Option<diagnostic::Location> {
    let position = self.tokens[self.index].1;

    Some(position..position)
  }

  /// Compare the current token to the given token.
  ///
  /// If `EOF` has been reached, `false` will always be returned.
  fn is(&self, token: &lexer::TokenKind) -> bool {
    if self.index >= self.tokens.len() {
      return false;
    }

    self.get() == token
  }

  // TODO: Need testing for this function.
  /// Attempt to reposition the index to the next token (if any).
  ///
  /// Returns whether the index was repositioned. If false, it indicates
  /// that the end of file was reached (`EOF`), and the current character
  /// cannot be skipped-over.
  fn skip(&mut self) -> bool {
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

    let module_name = if self.is(&lexer::TokenKind::SymbolColon) {
      self.skip();

      Some(starting_name.clone())
    } else {
      None
    };

    let base_name = if module_name.is_none() {
      starting_name
    } else {
      self.parse_name()?
    };

    let mut member_path = Vec::new();

    while self.is(&lexer::TokenKind::SymbolDot) {
      self.skip();
      member_path.push(self.parse_name()?);
    }

    Ok(ast::Pattern {
      module_name,
      base_name,
      member_path,
      symbol_kind,
      target_key: None,
    })
  }

  /// %identifier
  fn parse_name(&mut self) -> ParserResult<String> {
    // TODO: Illegal/unrecognized tokens MAY also be represented under 'Identifier'? Is this a problem?

    // TODO: Wrong error message. Create an `expect` method.
    assert!(matches!(self.get(), lexer::TokenKind::Identifier(_)));

    let name = match self.get() {
      lexer::TokenKind::Identifier(value) => Some(value.clone()),
      _ => None,
    };

    crate::diagnostic_assert!(name.is_some());
    self.skip();

    Ok(name.unwrap())
  }

  fn parse_statement(&mut self) -> ParserResult<ast::Node> {
    Ok(match self.get() {
      lexer::TokenKind::KeywordReturn => ast::Node::ReturnStmt(self.parse_return_stmt()?),
      lexer::TokenKind::KeywordLet => ast::Node::Definition(self.parse_let_stmt()?),
      lexer::TokenKind::KeywordLoop => ast::Node::LoopStmt(self.parse_loop_stmt()?),
      lexer::TokenKind::KeywordBreak => ast::Node::BreakStmt(self.parse_break_stmt()?),
      lexer::TokenKind::KeywordContinue => ast::Node::ContinueStmt(self.parse_continue_stmt()?),
      lexer::TokenKind::KeywordUnsafe => ast::Node::UnsafeBlock(self.parse_unsafe_block_stmt()?),
      lexer::TokenKind::Identifier(_) if self.after_pattern_is(&lexer::TokenKind::SymbolEqual) => {
        ast::Node::AssignStmt(self.parse_assign_stmt()?)
      }
      lexer::TokenKind::Identifier(_)
        if !self.after_pattern_is(&lexer::TokenKind::SymbolParenthesesL) =>
      {
        ast::Node::VariableOrMemberRef(self.parse_variable_or_member_ref()?)
      }
      // Otherwise, assume an in-line expression.
      _ => ast::Node::InlineExprStmt(ast::InlineExprStmt {
        expr: Box::new(self.parse_expr()?),
      }),
    })
  }

  /// {'{' (%statement+) '}' | '=' {%statement | %expr}}
  fn parse_block(&mut self) -> ParserResult<ast::Block> {
    // Support for short syntax.
    if self.is(&lexer::TokenKind::SymbolEqual) {
      self.skip();
      skip_past!(self, &lexer::TokenKind::SymbolGreaterThan);

      let statement = self.parse_statement()?;

      // FIXME: Is this correct for all cases?
      let yield_last_expr = if self.is(&lexer::TokenKind::SymbolSemiColon) {
        self.skip();

        false
      } else {
        true
      };

      // TODO: Must ensure a semi-colon always follows (for if statements, loops, etc.)?
      return Ok(ast::Block {
        statements: vec![Box::new(statement)],
        yield_last_expr,
      });
    }

    skip_past!(self, &lexer::TokenKind::SymbolBraceL);

    let mut statements = vec![];
    let mut yield_last_expr = false;

    while self.until(&lexer::TokenKind::SymbolBraceR)? {
      let statement = self.parse_statement()?;

      // TODO: Is this logic correct?
      // We're reached the end of the block without a semi-colon.
      // This means that an expression is to be yielded. Otherwise,
      // simply skip a semi-colon if applicable.
      if self.is(&lexer::TokenKind::SymbolBraceR) {
        yield_last_expr = true;
      }

      statements.push(Box::new(statement));
    }

    skip_past!(self, &lexer::TokenKind::SymbolBraceR);

    Ok(ast::Block {
      statements,
      yield_last_expr,
    })
  }

  /// {u8 | u16 | u32 | u64 | i8 | i16 | i32 | i64}
  fn parse_int_type(&mut self) -> ParserResult<ast::Type> {
    let current_token = self.get();

    let size = match current_token {
      lexer::TokenKind::TypeSignedInt8 => ast::IntSize::I8,
      lexer::TokenKind::TypeSignedInt16 => ast::IntSize::I16,
      lexer::TokenKind::TypeSignedInt32 => ast::IntSize::I32,
      lexer::TokenKind::TypeSignedInt64 => ast::IntSize::I64,
      lexer::TokenKind::TypeUnsignedInt8 => ast::IntSize::U8,
      lexer::TokenKind::TypeUnsignedInt16 => ast::IntSize::U16,
      lexer::TokenKind::TypeUnsignedInt32 => ast::IntSize::U32,
      lexer::TokenKind::TypeUnsignedInt64 => ast::IntSize::U64,
      // TODO: Add unsigned type tokens.
      _ => return Err(self.expected("integer type")),
    };

    self.skip();

    Ok(ast::Type::Primitive(ast::PrimitiveType::Int(size)))
  }

  // TODO: Merge with the `parse_type` function (too small)?
  /// bool
  fn parse_bool_type(&mut self) -> ParserResult<ast::Type> {
    skip_past!(self, &lexer::TokenKind::TypeBool);

    Ok(ast::Type::Primitive(ast::PrimitiveType::Bool))
  }

  /// '[' %type, 0-9+ ']'
  fn parse_array_type(&mut self) -> ParserResult<ast::Type> {
    skip_past!(self, &lexer::TokenKind::SymbolBracketL);

    let element_type = self.parse_type()?;

    skip_past!(self, &lexer::TokenKind::SymbolComma);

    let size = match self.get() {
      lexer::TokenKind::LiteralInt(value) => value.clone() as u32,
      _ => return Err(self.expected("array size")),
    };

    self.skip();
    skip_past!(self, &lexer::TokenKind::SymbolBracketR);

    Ok(ast::Type::Array(Box::new(element_type), size))
  }

  /// %type
  fn parse_type(&mut self) -> ParserResult<ast::Type> {
    // TODO: Check if the index is valid?
    // TODO: Support for more types.
    match self.get() {
      // TODO: Other types as well.
      lexer::TokenKind::TypeSignedInt8
      | lexer::TokenKind::TypeSignedInt16
      | lexer::TokenKind::TypeSignedInt32
      | lexer::TokenKind::TypeSignedInt64
      | lexer::TokenKind::TypeUnsignedInt8
      | lexer::TokenKind::TypeUnsignedInt16
      | lexer::TokenKind::TypeUnsignedInt32
      | lexer::TokenKind::TypeUnsignedInt64 => self.parse_int_type(),
      lexer::TokenKind::TypeBool => self.parse_bool_type(),
      lexer::TokenKind::TypeString => {
        self.skip();

        Ok(ast::Type::Primitive(ast::PrimitiveType::String))
      }
      lexer::TokenKind::SymbolBracketL => self.parse_array_type(),
      lexer::TokenKind::SymbolAsterisk => {
        self.skip();

        Ok(ast::Type::Pointer(Box::new(self.parse_type()?)))
      }
      lexer::TokenKind::Identifier(_) => self.parse_stub_type(),
      _ => {
        return Err(diagnostic::Diagnostic {
          message: format!(
            "unexpected token `{:?}`, expected type",
            // TODO: Check if the index is valid? This may be unsafe.
            self.tokens[self.index]
          ),
          severity: diagnostic::Severity::Error,
          location: self.get_location(),
        });
      }
    }
  }

  fn parse_stub_type(&mut self) -> ParserResult<ast::Type> {
    let name = self.parse_name()?;

    Ok(ast::Type::Stub(ast::StubType {
      name,
      target_key: None,
    }))
  }

  /// %name ':' %type_group
  fn parse_parameter(&mut self, index: u32) -> ParserResult<ast::Parameter> {
    let name = self.parse_name()?;

    skip_past!(self, &lexer::TokenKind::SymbolColon);

    let type_group = self.parse_type()?;

    Ok((name, type_group, index))
  }

  /// '(' {%parameter* (,)} (+) ')' ':' %type_group
  fn parse_prototype(&mut self) -> ParserResult<ast::Prototype> {
    skip_past!(self, &lexer::TokenKind::SymbolParenthesesL);

    // TODO: Parameters must be a `Declaration` node, in order for their references to be resolved.
    let mut parameters = vec![];
    let mut is_variadic = false;
    let mut parameter_index_counter = 0;

    // TODO: Analyze, and remove possibility of lonely comma.
    while self.until(&lexer::TokenKind::SymbolParenthesesR)? {
      if self.is(&lexer::TokenKind::SymbolPlus) {
        is_variadic = true;
        self.skip();

        break;
      }

      parameters.push(self.parse_parameter(parameter_index_counter)?);
      parameter_index_counter += 1;

      if !self.is(&lexer::TokenKind::SymbolComma) {
        break;
      }

      self.skip();
    }

    skip_past!(self, &lexer::TokenKind::SymbolParenthesesR);

    let mut return_type = ast::Type::Unit;

    if self.is(&lexer::TokenKind::SymbolColon) {
      self.skip();
      return_type = self.parse_type()?;
    }

    Ok(ast::Prototype {
      parameters,
      return_type,
      is_variadic,
    })
  }

  /// fn %prototype %block
  fn parse_function(&mut self, attributes: Vec<ast::Attribute>) -> ParserResult<ast::Definition> {
    // TODO: Support for visibility.

    skip_past!(self, &lexer::TokenKind::KeywordFn);

    let name = self.parse_name()?;
    let prototype = self.parse_prototype()?;
    let body = self.parse_block()?;

    let function = ast::Function {
      name: name.clone(),
      prototype,
      body,
      attributes,
    };

    Ok(ast::Definition {
      name,
      symbol_kind: name_resolution::SymbolKind::FunctionOrExtern,
      node_ref_cell: cache::create_cached_node(ast::Node::Function(function)),
      definition_key: self.cache.create_definition_key(),
    })
  }

  /// extern fn %prototype ';'
  fn parse_extern_function(
    &mut self,
    attributes: Vec<ast::Attribute>,
  ) -> ParserResult<ast::Definition> {
    // TODO: Support for visibility?

    skip_past!(self, &lexer::TokenKind::KeywordExtern);
    skip_past!(self, &lexer::TokenKind::KeywordFn);

    let name = self.parse_name()?;
    let prototype = self.parse_prototype()?;

    let extern_function = ast::ExternFunction {
      name: name.clone(),
      prototype,
      attributes,
    };

    Ok(ast::Definition {
      name,
      symbol_kind: name_resolution::SymbolKind::FunctionOrExtern,
      node_ref_cell: cache::create_cached_node(ast::Node::ExternFunction(extern_function)),
      definition_key: self.cache.create_definition_key(),
    })
  }

  fn parse_extern_static(&mut self) -> ParserResult<ast::Definition> {
    skip_past!(self, &lexer::TokenKind::KeywordExtern);
    skip_past!(self, &lexer::TokenKind::KeywordStatic);

    let name = self.parse_name()?;

    skip_past!(self, &lexer::TokenKind::SymbolColon);

    let ty = self.parse_type()?;

    let extern_static = ast::ExternStatic(name.clone(), ty);

    Ok(ast::Definition {
      name,
      symbol_kind: name_resolution::SymbolKind::StaticOrVariableOrParameter,
      node_ref_cell: cache::create_cached_node(ast::Node::ExternStatic(extern_static)),
      definition_key: self.cache.create_definition_key(),
    })
  }

  fn parse_attribute(&mut self) -> ParserResult<ast::Attribute> {
    skip_past!(self, &lexer::TokenKind::SymbolAt);

    let name = self.parse_name()?;
    let mut values = Vec::new();

    if self.is(&lexer::TokenKind::SymbolParenthesesL) {
      self.skip();

      while self.until(&lexer::TokenKind::SymbolParenthesesR)? {
        values.push(self.parse_literal()?);

        // TODO: Review this comma-skipping system.
        if !self.is(&lexer::TokenKind::SymbolComma) {
          break;
        }

        self.skip();
      }

      skip_past!(self, &lexer::TokenKind::SymbolParenthesesR);
    }

    Ok(ast::Attribute { name, values })
  }

  // TODO: Why not build the `Definition` node here? We might require access to the `name` and `symbol_kind`, however.
  /// {%function | %extern | %enum | %struct}
  fn parse_top_level_node(&mut self) -> ParserResult<ast::Node> {
    // TODO: Why not move this check into the `get()` method?
    if self.is_eof() {
      return Err(diagnostic::Diagnostic {
        message: "expected top-level construct but got end of file".to_string(),
        severity: diagnostic::Severity::Error,
        location: self.get_location(),
      });
    }

    let mut attributes: Vec<ast::Attribute> = Vec::new();

    while self.is(&lexer::TokenKind::SymbolAt) {
      let attribute = self.parse_attribute()?;

      if attributes.iter().any(|x| x.name == x.name) {
        return Err(diagnostic::Diagnostic {
          message: format!("duplicate attribute `{}`", attribute.name),
          severity: diagnostic::Severity::Error,
          location: self.get_location(),
        });
      }

      attributes.push(attribute);
    }

    let is_attributable = match self.get() {
      lexer::TokenKind::KeywordFn | lexer::TokenKind::KeywordExtern => true,
      _ => false,
    };

    if !attributes.is_empty() && !is_attributable {
      return Err(diagnostic::Diagnostic {
        message: "attributes may only be attached to functions or externs".to_string(),
        severity: diagnostic::Severity::Error,
        location: self.get_location(),
      });
    }

    let token = self.get();

    let definition = match token {
      // TODO: Why not create the definition here? That way we allow testability (functions actually return what they parse).
      lexer::TokenKind::KeywordFn => ast::Node::Definition(self.parse_function(attributes)?),
      lexer::TokenKind::KeywordExtern if self.peek_is(&lexer::TokenKind::KeywordFn) => {
        ast::Node::Definition(self.parse_extern_function(attributes)?)
      }
      lexer::TokenKind::KeywordExtern if self.peek_is(&lexer::TokenKind::KeywordStatic) => {
        ast::Node::Definition(self.parse_extern_static()?)
      }
      lexer::TokenKind::KeywordEnum => ast::Node::Definition(self.parse_enum()?),
      lexer::TokenKind::KeywordStruct => ast::Node::Definition(self.parse_struct_type()?),
      lexer::TokenKind::KeywordType => ast::Node::Definition(self.parse_type_alias()?),
      _ => {
        return Err(diagnostic::Diagnostic {
          message: format!("unexpected token `{}`, expected top-level construct", token),
          severity: diagnostic::Severity::Error,
          location: self.get_location(),
        })
      }
    };

    Ok(definition)
  }

  fn parse_type_alias(&mut self) -> ParserResult<ast::Definition> {
    skip_past!(self, &lexer::TokenKind::KeywordType);

    let name = self.parse_name()?;

    skip_past!(self, &lexer::TokenKind::SymbolEqual);

    // TODO: Ensure that there can't be recursive type aliases.
    let ty = self.parse_type()?;

    let type_alias = ast::TypeAlias {
      name: name.clone(),
      ty,
    };

    Ok(ast::Definition {
      name,
      symbol_kind: name_resolution::SymbolKind::Type,
      node_ref_cell: cache::create_cached_node(ast::Node::TypeAlias(type_alias)),
      definition_key: self.cache.create_definition_key(),
    })
  }

  /// return (%expr)
  fn parse_return_stmt(&mut self) -> ParserResult<ast::ReturnStmt> {
    skip_past!(self, &lexer::TokenKind::KeywordReturn);

    let mut value = None;

    // TODO: Does this cover all cases?
    if !self.is(&lexer::TokenKind::SymbolSemiColon) {
      value = Some(Box::new(self.parse_expr()?));
    } else {
      self.skip();
    }

    Ok(ast::ReturnStmt { value })
  }

  /// let %name (':' %type_group) '=' %expr ';'
  fn parse_let_stmt(&mut self) -> ParserResult<ast::Definition> {
    skip_past!(self, &lexer::TokenKind::KeywordLet);

    let is_mutable = if self.is(&lexer::TokenKind::KeywordMut) {
      self.skip();

      true
    } else {
      false
    };

    let name = self.parse_name()?;
    let mut ty = None;

    if self.is(&lexer::TokenKind::SymbolColon) {
      self.skip();
      ty = Some(self.parse_type()?);
    }

    skip_past!(self, &lexer::TokenKind::SymbolEqual);

    let value = self.parse_expr()?;

    let let_stmt = ast::LetStmt {
      name: name.clone(),
      ty,
      value: Box::new(value),
      is_mutable,
    };

    Ok(ast::Definition {
      name,
      symbol_kind: name_resolution::SymbolKind::StaticOrVariableOrParameter,
      node_ref_cell: cache::create_cached_node(ast::Node::LetStmt(let_stmt)),
      definition_key: self.cache.create_definition_key(),
    })
  }

  /// if %expr %block (else %block)
  fn parse_if_expr(&mut self) -> ParserResult<ast::IfStmt> {
    skip_past!(self, &lexer::TokenKind::KeywordIf);

    let condition = self.parse_expr()?;
    let then_block = self.parse_block()?;
    let mut else_block = None;

    if self.is(&lexer::TokenKind::KeywordElse) {
      self.skip();
      else_block = Some(self.parse_block()?);
    }

    Ok(ast::IfStmt {
      condition: Box::new(condition),
      then_block,
      else_block,
    })
  }

  /// loop %expr %block
  fn parse_loop_stmt(&mut self) -> ParserResult<ast::LoopStmt> {
    skip_past!(self, &lexer::TokenKind::KeywordLoop);

    let condition = if self.is(&lexer::TokenKind::SymbolBraceL) {
      None
    } else {
      Some(Box::new(self.parse_expr()?))
    };

    let body = self.parse_block()?;

    Ok(ast::LoopStmt { condition, body })
  }

  /// break ';'
  fn parse_break_stmt(&mut self) -> ParserResult<ast::BreakStmt> {
    skip_past!(self, &lexer::TokenKind::KeywordBreak);

    Ok(ast::BreakStmt {})
  }

  /// continue ';'
  fn parse_continue_stmt(&mut self) -> ParserResult<ast::ContinueStmt> {
    skip_past!(self, &lexer::TokenKind::KeywordContinue);

    Ok(ast::ContinueStmt)
  }

  // unsafe %block
  fn parse_unsafe_block_stmt(&mut self) -> ParserResult<ast::UnsafeBlockStmt> {
    // TODO: Why not merge this with the normal block, and just have a flag?

    skip_past!(self, &lexer::TokenKind::KeywordUnsafe);

    Ok(ast::UnsafeBlockStmt(self.parse_block()?))
  }

  /// {true | false}
  fn parse_bool_literal(&mut self) -> ParserResult<ast::Literal> {
    Ok(match self.get().clone() {
      lexer::TokenKind::LiteralBool(value) => {
        self.skip();

        ast::Literal::Bool(value.clone())
      }
      _ => return Err(self.expected("boolean literal")),
    })
  }

  /// 0-9+
  fn parse_int_literal(&mut self) -> ParserResult<ast::Literal> {
    Ok(match self.get().clone() {
      lexer::TokenKind::LiteralInt(value) => {
        self.skip();

        // TODO: Temporary syntax, until casting is implemented.
        let int_type = if self.is(&lexer::TokenKind::SymbolParenthesesL) {
          self.skip();

          let int_type = self.parse_int_type()?;

          skip_past!(self, &lexer::TokenKind::SymbolParenthesesR);

          Some(int_type)
        } else {
          None
        };

        let mut size = minimum_int_size_of(&value);

        // TODO: Deal with unsigned integers here?
        // Default size to 32 bit-width.
        if size < ast::IntSize::I32 {
          size = ast::IntSize::I32;
        }

        // TODO: Temporary.
        if let Some(int_type) = int_type {
          if let ast::Type::Primitive(ast::PrimitiveType::Int(type_size)) = int_type {
            size = type_size;
          } else {
            unreachable!();
          }
        }

        ast::Literal::Int(value, size)
      }
      _ => return Err(self.expected("integer literal")),
    })
  }

  /// '"' [^"]* '"'
  fn parse_string_literal(&mut self) -> ParserResult<ast::Literal> {
    let result = match self.get() {
      lexer::TokenKind::LiteralString(value) => ast::Literal::String(value.clone()),
      _ => return Err(self.expected("string literal")),
    };

    self.skip();

    Ok(result)
  }

  /// '[' (%expr (','))* ']'
  fn parse_array_value(&mut self) -> ParserResult<ast::ArrayValue> {
    let mut elements = Vec::new();

    skip_past!(self, &lexer::TokenKind::SymbolBracketL);

    while self.until(&lexer::TokenKind::SymbolBracketR)? {
      elements.push(self.parse_expr()?);

      // TODO: What if the comma isn't provided?
      if self.is(&lexer::TokenKind::SymbolComma) {
        self.skip();
      }
    }

    // Skip the closing bracket.
    self.skip();

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

  /// %pattern '[' %expr ']'
  fn parse_array_indexing(&mut self) -> ParserResult<ast::ArrayIndexing> {
    // TODO: Work with a pattern instead.
    let name = self.parse_name()?;

    skip_past!(self, &lexer::TokenKind::SymbolBracketL);

    let index = Box::new(self.parse_expr()?);

    skip_past!(self, &lexer::TokenKind::SymbolBracketR);

    Ok(ast::ArrayIndexing {
      name,
      index,
      target_key: None,
    })
  }

  fn parse_literal(&mut self) -> ParserResult<ast::Literal> {
    let current_token = self.get();

    Ok(match current_token {
      lexer::TokenKind::LiteralBool(_) => self.parse_bool_literal()?,
      lexer::TokenKind::LiteralInt(_) => self.parse_int_literal()?,
      lexer::TokenKind::LiteralString(_) => self.parse_string_literal()?,
      lexer::TokenKind::LiteralNullptr => {
        self.skip();

        ast::Literal::Nullptr
      }
      _ => return Err(self.expected("literal")),
    })
  }

  fn after_pattern_is(&self, token: &lexer::TokenKind) -> bool {
    let mut index = self.index + 1;

    // FIXME: This is hacky code. Fix up.
    while match self.tokens.get(index) {
      Some(value) => matches!(
        value.0,
        lexer::TokenKind::Identifier(_)
          | lexer::TokenKind::SymbolDot
          | lexer::TokenKind::SymbolColon
      ),
      None => false,
    } {
      index += 1;
    }

    // TODO: Implement logic to handle this (possible?) edge case.
    if self.is_eof() {
      todo!();
    }

    &self.tokens.get(index).unwrap().0 == token
  }

  fn parse_primary_expr(&mut self) -> ParserResult<ast::Node> {
    Ok(match self.get() {
      lexer::TokenKind::KeywordIf => ast::Node::IfStmt(self.parse_if_expr()?),
      lexer::TokenKind::SymbolTilde => ast::Node::IntrinsicCall(self.parse_intrinsic_call()?),
      lexer::TokenKind::Identifier(_)
        if self.after_pattern_is(&lexer::TokenKind::SymbolParenthesesL) =>
      {
        ast::Node::FunctionCall(self.parse_function_call()?)
      }
      // FIXME: Use `after_pattern_is`.
      lexer::TokenKind::Identifier(_) if self.peek_is(&lexer::TokenKind::SymbolBracketL) => {
        ast::Node::ArrayIndexing(self.parse_array_indexing()?)
      }
      // FIXME: Use `after_pattern_is`.
      lexer::TokenKind::Identifier(_) => {
        ast::Node::VariableOrMemberRef(self.parse_variable_or_member_ref()?)
      }
      lexer::TokenKind::SymbolMinus
      | lexer::TokenKind::SymbolBang
      | lexer::TokenKind::SymbolAmpersand
      | lexer::TokenKind::SymbolAsterisk => ast::Node::UnaryExpr(self.parse_unary_expr()?),
      lexer::TokenKind::SymbolBracketL => ast::Node::ArrayValue(self.parse_array_value()?),
      lexer::TokenKind::KeywordNew => ast::Node::StructValue(self.parse_struct_value()?),
      // Default to a literal if nothing else matched.
      _ => ast::Node::Literal(self.parse_literal()?),
    })
  }

  /// {'+' | '-' | '*' | '/'}
  fn parse_operator(&mut self) -> ParserResult<ast::OperatorKind> {
    // TODO: Unsafe access. Also, cloning token.
    let current_token = self.get().clone();

    let operator = match current_token {
      lexer::TokenKind::KeywordAnd => ast::OperatorKind::And,
      lexer::TokenKind::KeywordOr => ast::OperatorKind::Or,
      lexer::TokenKind::KeywordNand => ast::OperatorKind::Nand,
      lexer::TokenKind::KeywordNor => ast::OperatorKind::Nor,
      lexer::TokenKind::KeywordXor => ast::OperatorKind::Xor,
      lexer::TokenKind::SymbolBang => ast::OperatorKind::Not,
      lexer::TokenKind::SymbolPlus => ast::OperatorKind::Add,
      lexer::TokenKind::SymbolMinus => ast::OperatorKind::SubtractOrNegate,
      lexer::TokenKind::SymbolAsterisk => ast::OperatorKind::MultiplyOrDereference,
      lexer::TokenKind::SymbolSlash => ast::OperatorKind::Divide,
      lexer::TokenKind::SymbolLessThan => ast::OperatorKind::LessThan,
      lexer::TokenKind::SymbolGreaterThan => ast::OperatorKind::GreaterThan,
      lexer::TokenKind::SymbolAmpersand => ast::OperatorKind::AddressOf,
      lexer::TokenKind::SymbolEqual if self.peek_is(&lexer::TokenKind::SymbolEqual) => {
        self.skip();

        ast::OperatorKind::Equality
      }
      // TODO: Implement logic for GTE & LTE.
      _ => return Err(self.expected("operator")),
    };

    self.skip();

    Ok(operator)
  }

  /// %expr %operator %expr
  fn parse_binary_expr(
    &mut self,
    left: ast::Node,
    min_precedence: usize,
  ) -> ParserResult<ast::Node> {
    let mut token = self.get();
    let precedence = get_token_precedence(&token);
    let mut result = left;

    while self.is_binary_operator(token) && (precedence > min_precedence) {
      let operator = self.parse_operator()?;
      let mut right = self.parse_primary_expr()?;

      token = self.get();

      while self.is_binary_operator(&token) && get_token_precedence(&token) > precedence {
        // TODO: This isn't tail-recursive?
        right = self.parse_binary_expr(right, precedence + 1)?;
        token = self.get();
      }

      result = ast::Node::BinaryExpr(ast::BinaryExpr {
        left: Box::new(result),
        operator,
        right: Box::new(right),
      });
    }

    Ok(result)
  }

  /// %operator %expr
  fn parse_unary_expr(&mut self) -> ParserResult<ast::UnaryExpr> {
    let operator = self.parse_operator()?;
    let expr = Box::new(self.parse_primary_expr()?);

    Ok(ast::UnaryExpr { operator, expr })
  }

  // TODO: Better naming and/or positioning for logic.
  fn parse_expr(&mut self) -> ParserResult<ast::Node> {
    // TODO: Add support for unary expressions (such as `!`, '-', etc.).

    let starting_expr = self.parse_primary_expr()?;

    // TODO: Should the precedence be zero here?
    Ok(self.parse_binary_expr(starting_expr, 0)?)
  }

  /// %pattern '(' (%expr (,))* ')'
  fn parse_function_call(&mut self) -> ParserResult<ast::FunctionCall> {
    let callee_pattern = self.parse_pattern(name_resolution::SymbolKind::FunctionOrExtern)?;

    skip_past!(self, &lexer::TokenKind::SymbolParenthesesL);

    let mut arguments = vec![];

    while self.until(&lexer::TokenKind::SymbolParenthesesR)? {
      arguments.push(self.parse_expr()?);

      // TODO: What if the comma is omitted?
      if self.is(&lexer::TokenKind::SymbolComma) {
        self.skip();
      }
    }

    skip_past!(self, &lexer::TokenKind::SymbolParenthesesR);

    Ok(ast::FunctionCall {
      callee_pattern,
      arguments,
    })
  }

  fn parse_intrinsic_call(&mut self) -> ParserResult<ast::IntrinsicCall> {
    skip_past!(self, &lexer::TokenKind::SymbolTilde);

    let kind = match self.parse_name()?.as_str() {
      "panic" => ast::IntrinsicKind::Panic,
      _ => return Err(self.expected("valid intrinsic name")),
    };

    skip_past!(self, &lexer::TokenKind::SymbolParenthesesL);

    let mut arguments = vec![];

    while self.until(&lexer::TokenKind::SymbolParenthesesR)? {
      arguments.push(self.parse_expr()?);

      // TODO: What if the comma is omitted?
      if self.is(&lexer::TokenKind::SymbolComma) {
        self.skip();
      }
    }

    skip_past!(self, &lexer::TokenKind::SymbolParenthesesR);

    Ok(ast::IntrinsicCall { kind, arguments })
  }

  /// %name
  fn parse_variable_or_member_ref(&mut self) -> ParserResult<ast::VariableOrMemberRef> {
    let pattern = self.parse_pattern(name_resolution::SymbolKind::StaticOrVariableOrParameter)?;

    Ok(ast::VariableOrMemberRef(pattern))
  }

  /// %name '=' %expr ';'
  fn parse_assign_stmt(&mut self) -> ParserResult<ast::AssignStmt> {
    let assignee_expr = Box::new(self.parse_expr()?);

    skip_past!(self, &lexer::TokenKind::SymbolEqual);

    let value = Box::new(self.parse_expr()?);

    Ok(ast::AssignStmt {
      assignee_expr,
      value,
    })
  }

  /// enum %name '{' (%name (','))* '}'
  fn parse_enum(&mut self) -> ParserResult<ast::Definition> {
    skip_past!(self, &lexer::TokenKind::KeywordEnum);

    let name = self.parse_name()?;

    skip_past!(self, &lexer::TokenKind::SymbolBraceL);

    let mut variants = vec![];

    while self.until(&lexer::TokenKind::SymbolBraceR)? {
      variants.push(self.parse_name()?);

      // TODO: Iron out case for lonely comma.
      if self.is(&lexer::TokenKind::SymbolComma) {
        self.skip();
      }
    }

    // TODO: What if we reach `EOF` before closing brace?

    // Skip the closing brace symbol.
    self.skip();

    let enum_ = ast::Enum {
      name: name.clone(),
      variants,
    };

    Ok(ast::Definition {
      name,
      symbol_kind: name_resolution::SymbolKind::Type,
      node_ref_cell: cache::create_cached_node(ast::Node::Enum(enum_)),
      definition_key: self.cache.create_definition_key(),
    })
  }

  /// struct %name '{' (%name ':' %type ';')* '}'
  fn parse_struct_type(&mut self) -> ParserResult<ast::Definition> {
    skip_past!(self, &lexer::TokenKind::KeywordStruct);

    let name = self.parse_name()?;

    skip_past!(self, &lexer::TokenKind::SymbolBraceL);

    let mut fields = std::collections::HashMap::new();

    while self.until(&lexer::TokenKind::SymbolBraceR)? {
      let field_name = self.parse_name()?;

      skip_past!(self, &lexer::TokenKind::SymbolColon);

      let field_type = self.parse_type()?;

      skip_past!(self, &lexer::TokenKind::SymbolComma);
      fields.insert(field_name, field_type);
    }

    // Skip the closing brace symbol.
    self.skip();

    let struct_type = ast::StructType {
      name: name.clone(),
      fields,
    };

    Ok(ast::Definition {
      name,
      symbol_kind: name_resolution::SymbolKind::Type,
      node_ref_cell: cache::create_cached_node(ast::Node::StructType(struct_type)),
      definition_key: self.cache.create_definition_key(),
    })
  }

  fn parse_struct_value(&mut self) -> ParserResult<ast::StructValue> {
    skip_past!(self, &lexer::TokenKind::KeywordNew);

    // TODO: Shouldn't it be `ScopeQualifier`?
    let name = self.parse_name()?;

    skip_past!(self, &lexer::TokenKind::SymbolBraceL);

    let mut fields = Vec::new();

    while self.until(&lexer::TokenKind::SymbolBraceR)? {
      let field = self.parse_expr()?;

      fields.push(field);

      // TODO: Disallow trailing comma.
      if self.is(&lexer::TokenKind::SymbolComma) {
        self.skip();
      }
    }

    // Skip the closing brace symbol.
    self.skip();

    Ok(ast::StructValue {
      name,
      fields,
      target_key: None,
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
    let parser = Parser::from_tokens(vec![lexer::TokenKind::KeywordFn], &mut context);

    assert_eq!(true, parser.is(&lexer::TokenKind::KeywordFn));
  }

  #[test]
  fn is_empty() {
    let mut context = cache::Cache::new();
    let parser = Parser::new(vec![], &mut context);

    assert_eq!(false, parser.is(&lexer::TokenKind::KeywordFn));
  }

  #[test]
  fn skip() {
    let mut context = cache::Cache::new();

    let mut parser = Parser::from_tokens(
      vec![lexer::TokenKind::KeywordFn, lexer::TokenKind::KeywordFn],
      &mut context,
    );

    parser.skip();
    assert_eq!(1, parser.index);
  }

  #[test]
  fn skip_out_of_bounds() {
    let mut context = cache::Cache::new();
    let mut parser = Parser::from_tokens(vec![lexer::TokenKind::KeywordFn], &mut context);

    parser.skip();
    assert_eq!(0, parser.index);
  }

  #[test]
  fn is_eof() {
    let mut context = cache::Cache::new();
    let mut parser = Parser::new(vec![], &mut context);

    assert_eq!(true, parser.is_eof());
    parser.tokens.push((lexer::TokenKind::KeywordFn, 0));
    assert_eq!(true, parser.is_eof());
    parser.tokens.push((lexer::TokenKind::KeywordFn, 0));
    assert_eq!(false, parser.is_eof());
    parser.skip();
    assert_eq!(true, parser.is_eof());
  }

  // TODO: Add more tests.
}
