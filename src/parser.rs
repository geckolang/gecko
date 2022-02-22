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

  fn close_span(&self, span_start: usize) -> diagnostic::Span {
    span_start..self.index
  }

  fn skip_past(&mut self, token_kind: &lexer::TokenKind) -> ParserResult<()> {
    if !self.is(token_kind) {
      return Err(self.expected(format!("token `{}`", token_kind).as_str()));
    }

    self.skip();

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

  fn is_unary_operator(&self) -> bool {
    matches!(
      self.force_get(),
      lexer::TokenKind::SymbolMinus
        | lexer::TokenKind::SymbolBang
        | lexer::TokenKind::SymbolAmpersand
        | lexer::TokenKind::SymbolAsterisk
        | lexer::TokenKind::SymbolBacktick
    )
  }

  fn until(&self, token: &lexer::TokenKind) -> ParserResult<bool> {
    // TODO: Handle `EOF` case here.
    return Ok(!self.is_eof() && !self.is(token));
  }

  fn force_get(&self) -> &lexer::TokenKind {
    // TODO: Accessing index unsafely. How about using `ParserResult<&lexer::TokenKind>`?
    &self.tokens.get(self.index).unwrap().0
  }

  fn get_span(&self) -> Option<diagnostic::Span> {
    // TODO: Safety check?
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

  // TODO: Consider returning a `ParserResult<bool>` instead.
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

    Ok(ast::Pattern {
      module_name,
      base_name,
      symbol_kind,
      target_id: None,
    })
  }

  fn parse_name(&mut self) -> ParserResult<String> {
    // TODO: Illegal/unrecognized tokens MAY also be represented under 'Identifier'? Is this a problem?

    let name = match self.force_get() {
      lexer::TokenKind::Identifier(value) => value.clone(),
      _ => return Err(self.expected("identifier")),
    };

    self.skip();

    Ok(name)
  }

  fn parse_statement(&mut self) -> ParserResult<ast::Node> {
    let span_start = self.index;

    let kind = match self.force_get() {
      lexer::TokenKind::KeywordReturn => ast::NodeKind::ReturnStmt(self.parse_return_stmt()?),
      lexer::TokenKind::KeywordLet => ast::NodeKind::LetStmt(self.parse_let_stmt()?),
      lexer::TokenKind::KeywordLoop => ast::NodeKind::LoopStmt(self.parse_loop_stmt()?),
      lexer::TokenKind::KeywordBreak => ast::NodeKind::BreakStmt(self.parse_break_stmt()?),
      lexer::TokenKind::KeywordContinue => ast::NodeKind::ContinueStmt(self.parse_continue_stmt()?),
      lexer::TokenKind::KeywordUnsafe => {
        ast::NodeKind::UnsafeBlock(self.parse_unsafe_block_stmt()?)
      }
      _ => {
        let expr = self.parse_expr()?;

        // Promote the inline expression to an assignment statement, if applicable.
        if self.is(&lexer::TokenKind::SymbolEqual) {
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
      unique_id: self.cache.create_unique_id(),
    })
  }

  /// {'{' (%statement+) '}' | '=' {%statement | %expr}}
  fn parse_block(&mut self) -> ParserResult<ast::Block> {
    // TODO: Simplify.
    // Support for short syntax.
    if self.is(&lexer::TokenKind::SymbolEqual) {
      self.skip();
      self.skip_past(&lexer::TokenKind::SymbolGreaterThan)?;

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
        statements: vec![statement],
        yield_last_expr,
        unique_id: self.cache.create_unique_id(),
      });
    }

    self.skip_past(&lexer::TokenKind::SymbolBraceL)?;

    let mut statements = Vec::new();
    let mut yield_last_expr = true;

    // FIXME: What if the last statement is a let-statement? Let-statements have inferrable types, used internally. Review this.
    // FIXME: This may be the reason why let-statements shouldn't have inferrable types (only internally).
    while self.until(&lexer::TokenKind::SymbolBraceR)? {
      let statement = self.parse_statement()?;

      if self.peek_is(&lexer::TokenKind::SymbolBraceR)
        && self.is(&lexer::TokenKind::SymbolSemiColon)
      {
        self.skip();
        yield_last_expr = false;
      }

      statements.push(statement);
    }

    self.skip_past(&lexer::TokenKind::SymbolBraceR)?;

    Ok(ast::Block {
      statements,
      yield_last_expr,
      unique_id: self.cache.create_unique_id(),
    })
  }

  /// {u8 | u16 | u32 | u64 | i8 | i16 | i32 | i64}
  fn parse_int_type(&mut self) -> ParserResult<ast::Type> {
    let current_token = self.force_get();

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
    self.skip_past(&lexer::TokenKind::TypeBool)?;

    Ok(ast::Type::Primitive(ast::PrimitiveType::Bool))
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
    self.skip_past(&lexer::TokenKind::SymbolBracketL)?;

    let element_type = self.parse_type()?;

    self.skip_past(&lexer::TokenKind::SymbolComma)?;

    let size = match self.force_get() {
      lexer::TokenKind::LiteralInt(value) => value.clone() as u32,
      _ => return Err(self.expected("array size")),
    };

    self.skip();
    self.skip_past(&lexer::TokenKind::SymbolBracketR)?;

    Ok(ast::Type::Array(Box::new(element_type), size))
  }

  fn parse_type(&mut self) -> ParserResult<ast::Type> {
    // TODO: Check if the index is valid?
    // TODO: Support for more types.
    match self.force_get() {
      // TODO: Other types as well.
      lexer::TokenKind::KeywordFn => self.parse_callable_type(),
      lexer::TokenKind::TypeSignedInt8
      | lexer::TokenKind::TypeSignedInt16
      | lexer::TokenKind::TypeSignedInt32
      | lexer::TokenKind::TypeSignedInt64
      | lexer::TokenKind::TypeUnsignedInt8
      | lexer::TokenKind::TypeUnsignedInt16
      | lexer::TokenKind::TypeUnsignedInt32
      | lexer::TokenKind::TypeUnsignedInt64 => self.parse_int_type(),
      lexer::TokenKind::TypeBool => self.parse_bool_type(),
      lexer::TokenKind::Identifier(_) => self.parse_stub_type(),
      lexer::TokenKind::SymbolBracketL => self.parse_array_type(),
      lexer::TokenKind::TypeThis => self.parse_this_type(),
      lexer::TokenKind::SymbolAsterisk => {
        self.skip();

        Ok(ast::Type::Pointer(Box::new(self.parse_type()?)))
      }
      lexer::TokenKind::TypeString => {
        self.skip();

        Ok(ast::Type::Primitive(ast::PrimitiveType::String))
      }
      _ => return Err(self.expected("type")),
    }
  }

  /// fn '(' (%type (','))* ')' ('[' %type ']')
  fn parse_callable_type(&mut self) -> ParserResult<ast::Type> {
    self.skip_past(&lexer::TokenKind::KeywordFn)?;
    self.skip_past(&lexer::TokenKind::SymbolParenthesesL)?;

    let mut parameter_types = Vec::new();

    while self.until(&lexer::TokenKind::SymbolParenthesesR)? {
      parameter_types.push(self.parse_type()?);

      if !self.peek_is(&lexer::TokenKind::SymbolParenthesesR) {
        self.skip_past(&lexer::TokenKind::SymbolComma)?;
      }
    }

    self.skip_past(&lexer::TokenKind::SymbolParenthesesR)?;
    self.skip_past(&lexer::TokenKind::SymbolBracketL)?;

    let return_type = if self.is(&lexer::TokenKind::SymbolBracketR) {
      ast::Type::Unit
    } else {
      self.parse_type()?
    };

    self.skip_past(&lexer::TokenKind::SymbolBracketR)?;

    Ok(ast::Type::Callable(ast::CallableType {
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
      ty: None,
    }))
  }

  /// %name ':' %type
  fn parse_parameter(&mut self, index: u32) -> ParserResult<ast::Parameter> {
    let name = self.parse_name()?;

    self.skip_past(&lexer::TokenKind::SymbolColon)?;

    let ty = self.parse_type()?;

    Ok((name, ty, index))
  }

  /// '(' {%parameter* (,)} (+) ')' ':' %type
  fn parse_prototype(&mut self) -> ParserResult<ast::Prototype> {
    self.skip_past(&lexer::TokenKind::SymbolParenthesesL)?;

    // TODO: Parameters must be a `Declaration` node, in order for their references to be resolved.
    let mut parameters = vec![];
    let mut is_variadic = false;
    let mut parameter_index_counter = 0;
    let mut accepts_instance = false;
    let mut this_parameter = None;

    if self.is(&lexer::TokenKind::Identifier(THIS_IDENTIFIER.to_string())) {
      self.skip();
      parameter_index_counter += 1;
      accepts_instance = true;

      this_parameter = Some((
        THIS_IDENTIFIER.to_string(),
        ast::Type::This(ast::ThisType {
          target_id: None,
          ty: None,
        }),
        0,
      ));

      if !self.is(&lexer::TokenKind::SymbolParenthesesR) {
        self.skip_past(&lexer::TokenKind::SymbolComma)?;
      }
    }

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

    self.skip_past(&lexer::TokenKind::SymbolParenthesesR)?;

    let return_type = if self.is(&lexer::TokenKind::SymbolColon) {
      self.skip();

      Some(self.parse_type()?)
    } else {
      None
    };

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
    self.skip_past(&lexer::TokenKind::KeywordFn)?;

    let name = self.parse_name()?;
    let prototype = self.parse_prototype()?;
    let body = self.parse_block()?;

    Ok(ast::Function {
      name: name.clone(),
      prototype,
      body,
      attributes,
    })
  }

  /// extern fn %prototype
  fn parse_extern_function(
    &mut self,
    attributes: Vec<ast::Attribute>,
  ) -> ParserResult<ast::ExternFunction> {
    // TODO: Support for visibility?

    self.skip_past(&lexer::TokenKind::KeywordExtern)?;
    self.skip_past(&lexer::TokenKind::KeywordFn)?;

    let name = self.parse_name()?;
    let prototype = self.parse_prototype()?;

    // Extern functions must provide an explicit return type for their
    // prototypes.
    if prototype.return_type.is_none() {
      return Err(self.expected("explicit return type"));
    }

    Ok(ast::ExternFunction {
      name: name.clone(),
      prototype,
      attributes,
    })
  }

  /// extern static %name ':' %type
  fn parse_extern_static(&mut self) -> ParserResult<ast::ExternStatic> {
    self.skip_past(&lexer::TokenKind::KeywordExtern)?;
    self.skip_past(&lexer::TokenKind::KeywordStatic)?;

    let name = self.parse_name()?;

    self.skip_past(&lexer::TokenKind::SymbolColon)?;

    let ty = self.parse_type()?;

    Ok(ast::ExternStatic(name.clone(), ty))
  }

  /// '@' %name ('(' (%literal (','))* ')')
  fn parse_attribute(&mut self) -> ParserResult<ast::Attribute> {
    self.skip_past(&lexer::TokenKind::SymbolAt)?;

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

      self.skip_past(&lexer::TokenKind::SymbolParenthesesR)?;
    }

    Ok(ast::Attribute { name, values })
  }

  // TODO: Why not build the `Definition` node here? We might require access to the `name` and `symbol_kind`, however.
  /// {%function | %extern_function | %extern_static | %type_alias | %enum | %struct_type}
  fn parse_top_level_node(&mut self) -> ParserResult<ast::Node> {
    let span_start = self.index;

    // TODO: Why not move this check into the `get()` method?
    if self.is_eof() {
      return Err(diagnostic::Diagnostic {
        message: "expected top-level construct but got end of file".to_string(),
        severity: diagnostic::Severity::Error,
        span: self.get_span(),
      });
    }

    let mut attributes: Vec<ast::Attribute> = Vec::new();

    while self.is(&lexer::TokenKind::SymbolAt) {
      let span_start = self.index;

      let attribute = self.parse_attribute()?;

      if attributes.iter().any(|x| x.name == x.name) {
        return Err(diagnostic::Diagnostic {
          message: format!("duplicate attribute `{}`", attribute.name),
          severity: diagnostic::Severity::Error,
          span: Some(self.close_span(span_start)),
        });
      }

      attributes.push(attribute);
    }

    let is_attributable = matches!(
      self.force_get(),
      lexer::TokenKind::KeywordFn | lexer::TokenKind::KeywordExtern
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
      // TODO: Why not create the definition here? That way we allow testability (functions actually return what they parse).
      lexer::TokenKind::KeywordFn => ast::NodeKind::Function(self.parse_function(attributes)?),
      lexer::TokenKind::KeywordExtern if self.peek_is(&lexer::TokenKind::KeywordFn) => {
        ast::NodeKind::ExternFunction(self.parse_extern_function(attributes)?)
      }
      lexer::TokenKind::KeywordExtern if self.peek_is(&lexer::TokenKind::KeywordStatic) => {
        ast::NodeKind::ExternStatic(self.parse_extern_static()?)
      }
      lexer::TokenKind::KeywordEnum => ast::NodeKind::Enum(self.parse_enum()?),
      lexer::TokenKind::KeywordStruct => ast::NodeKind::StructType(self.parse_struct_type()?),
      lexer::TokenKind::KeywordType => ast::NodeKind::TypeAlias(self.parse_type_alias()?),
      lexer::TokenKind::KeywordImpl => ast::NodeKind::StructImpl(self.parse_struct_impl()?),
      lexer::TokenKind::KeywordTrait => ast::NodeKind::Trait(self.parse_trait()?),
      _ => return Err(self.expected("top-level construct")),
    };

    Ok(ast::Node {
      kind,
      span: self.close_span(span_start),
      unique_id: self.cache.create_unique_id(),
    })
  }

  /// type %name = %type
  fn parse_type_alias(&mut self) -> ParserResult<ast::TypeAlias> {
    self.skip_past(&lexer::TokenKind::KeywordType)?;

    let name = self.parse_name()?;

    self.skip_past(&lexer::TokenKind::SymbolEqual)?;

    // FIXME: Recursive type aliases are possible, and cause stack-overflow. Fix this bug. This bug possibly exist for any other thing that uses deferred-resolved types.
    let ty = self.parse_type()?;

    Ok(ast::TypeAlias {
      name: name.clone(),
      ty,
    })
  }

  /// return (%expr)
  fn parse_return_stmt(&mut self) -> ParserResult<ast::ReturnStmt> {
    self.skip_past(&lexer::TokenKind::KeywordReturn)?;

    let mut value = None;

    // TODO: Does this cover all cases?
    if !self.is(&lexer::TokenKind::SymbolSemiColon) {
      value = Some(Box::new(self.parse_expr()?));
    } else {
      self.skip();
    }

    Ok(ast::ReturnStmt { value })
  }

  /// let (mut) %name (':' %type) '=' %expr
  fn parse_let_stmt(&mut self) -> ParserResult<ast::LetStmt> {
    self.skip_past(&lexer::TokenKind::KeywordLet)?;

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

    self.skip_past(&lexer::TokenKind::SymbolEqual)?;

    let value = self.parse_expr()?;

    // TODO: Value should be treated as rvalue, unless its using an address-of operator. Find out how to translate this to logic.

    Ok(ast::LetStmt {
      name: name.clone(),
      ty,
      value: Box::new(value),
      is_mutable,
    })
  }

  /// if %expr %block (else %block)
  fn parse_if_expr(&mut self) -> ParserResult<ast::IfStmt> {
    self.skip_past(&lexer::TokenKind::KeywordIf)?;

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

  /// loop (%expr) %block
  fn parse_loop_stmt(&mut self) -> ParserResult<ast::LoopStmt> {
    self.skip_past(&lexer::TokenKind::KeywordLoop)?;

    let condition = if self.is(&lexer::TokenKind::SymbolBraceL) {
      None
    } else {
      Some(Box::new(self.parse_expr()?))
    };

    let body = self.parse_block()?;

    Ok(ast::LoopStmt { condition, body })
  }

  /// break
  fn parse_break_stmt(&mut self) -> ParserResult<ast::BreakStmt> {
    self.skip_past(&lexer::TokenKind::KeywordBreak)?;

    Ok(ast::BreakStmt {})
  }

  /// continue
  fn parse_continue_stmt(&mut self) -> ParserResult<ast::ContinueStmt> {
    self.skip_past(&lexer::TokenKind::KeywordContinue)?;

    Ok(ast::ContinueStmt)
  }

  // unsafe %block
  fn parse_unsafe_block_stmt(&mut self) -> ParserResult<ast::UnsafeBlockStmt> {
    // TODO: Why not merge this with the normal block, and just have a flag?

    self.skip_past(&lexer::TokenKind::KeywordUnsafe)?;

    Ok(ast::UnsafeBlockStmt(self.parse_block()?))
  }

  /// {true | false}
  fn parse_bool_literal(&mut self) -> ParserResult<ast::Literal> {
    Ok(match self.force_get().clone() {
      lexer::TokenKind::LiteralBool(value) => {
        self.skip();

        ast::Literal::Bool(value.clone())
      }
      _ => return Err(self.expected("boolean literal")),
    })
  }

  /// 0-9+
  fn parse_int_literal(&mut self) -> ParserResult<ast::Literal> {
    Ok(match self.force_get().clone() {
      lexer::TokenKind::LiteralInt(value) => {
        self.skip();

        // TODO: Temporary syntax, until casting is implemented.
        let int_type = if self.is(&lexer::TokenKind::SymbolParenthesesL) {
          self.skip();

          let int_type = self.parse_int_type()?;

          self.skip_past(&lexer::TokenKind::SymbolParenthesesR)?;

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
    let result = match self.force_get() {
      lexer::TokenKind::LiteralString(value) => ast::Literal::String(value.clone()),
      _ => return Err(self.expected("string literal")),
    };

    self.skip();

    Ok(result)
  }

  /// '[' (%expr (','))* ']'
  fn parse_array_value(&mut self) -> ParserResult<ast::ArrayValue> {
    let mut elements = Vec::new();

    self.skip_past(&lexer::TokenKind::SymbolBracketL)?;

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

  /// %name '[' %expr ']'
  fn parse_array_indexing(&mut self) -> ParserResult<ast::ArrayIndexing> {
    // TODO: Work with a pattern instead.
    let name = self.parse_name()?;

    self.skip_past(&lexer::TokenKind::SymbolBracketL)?;

    let index = Box::new(self.parse_expr()?);

    self.skip_past(&lexer::TokenKind::SymbolBracketR)?;

    Ok(ast::ArrayIndexing {
      name,
      index_expr: index,
      target_id: None,
    })
  }

  fn parse_literal(&mut self) -> ParserResult<ast::Literal> {
    let current_token = self.force_get();

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

  // TODO: Retire this function. Remove it once its no longer needed.
  fn after_pattern_is(&self, token: &lexer::TokenKind) -> bool {
    let mut index = self.index;
    let mut delimiter_switch = false;
    let is_past_eof_at = |index: usize| index >= self.tokens.len();

    // FIXME: This is hacky code. Fix up.
    // FIXME: Ensure this works as expected with the modifications done.
    while !is_past_eof_at(index)
      && match self.tokens.get(index).unwrap().0 {
        lexer::TokenKind::SymbolDot | lexer::TokenKind::SymbolColon if delimiter_switch => true,
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
      lexer::TokenKind::SymbolDot | lexer::TokenKind::SymbolParenthesesL
    )
  }

  fn parse_primary_expr(&mut self) -> ParserResult<ast::Node> {
    let span_start = self.index;

    let kind = match self.force_get() {
      // FIXME: Possible redundant check after the fn keyword. But how do we know we're still not on a block and accidentally parse a function as a closure?
      lexer::TokenKind::KeywordFn
        if (self.peek_is(&lexer::TokenKind::SymbolBracketL)
          || self.peek_is(&lexer::TokenKind::SymbolParenthesesL)) =>
      {
        ast::NodeKind::Closure(self.parse_closure()?)
      }
      lexer::TokenKind::KeywordIf => ast::NodeKind::IfStmt(self.parse_if_expr()?),
      lexer::TokenKind::SymbolTilde => ast::NodeKind::IntrinsicCall(self.parse_intrinsic_call()?),
      // TODO: Change this syntax to the same treatment as call expressions (check afterwards).
      lexer::TokenKind::Identifier(_)
        if self.after_pattern_is(&lexer::TokenKind::SymbolBracketL) =>
      {
        ast::NodeKind::ArrayIndexing(self.parse_array_indexing()?)
      }
      lexer::TokenKind::Identifier(_) => ast::NodeKind::Reference(self.parse_reference()?),
      lexer::TokenKind::SymbolBracketL => ast::NodeKind::ArrayValue(self.parse_array_value()?),
      lexer::TokenKind::KeywordNew => ast::NodeKind::StructValue(self.parse_struct_value()?),
      _ if self.is_unary_operator() => ast::NodeKind::UnaryExpr(self.parse_unary_expr()?),
      // Default to a literal if nothing else matched.
      _ => ast::NodeKind::Literal(self.parse_literal()?),
    };

    let unique_id = self.cache.create_unique_id();

    let mut node = ast::Node {
      kind,
      span: self.close_span(span_start),
      unique_id,
    };

    // Promote the node to a chain, if applicable.
    while self.is_chain() {
      let kind = match self.force_get() {
        lexer::TokenKind::SymbolParenthesesL => {
          ast::NodeKind::CallExpr(self.parse_call_expr(node)?)
        }
        lexer::TokenKind::SymbolDot => ast::NodeKind::MemberAccess(self.parse_member_access(node)?),
        _ => unreachable!(),
      };

      // TODO: Simplify (DRY)?
      node = ast::Node {
        kind,
        span: self.close_span(span_start),
        unique_id,
      };
    }

    Ok(node)
  }

  /// %expr '.' %name
  fn parse_member_access(&mut self, base_expr: ast::Node) -> ParserResult<ast::MemberAccess> {
    self.skip_past(&lexer::TokenKind::SymbolDot)?;

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
      lexer::TokenKind::SymbolBacktick => ast::OperatorKind::Cast,
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

  // TODO: Move to use the Pratt parsing technique instead to replace the non-tail recursive method.
  /// %expr %operator %expr
  fn parse_binary_expr_or_default(
    &mut self,
    left: ast::Node,
    min_precedence: usize,
  ) -> ParserResult<ast::Node> {
    let span_start = self.index;
    let unique_id = self.cache.create_unique_id();
    let mut token_buffer = self.force_get();
    let precedence = get_token_precedence(&token_buffer);
    let mut buffer = left;

    while self.is_binary_operator(token_buffer) && (precedence > min_precedence) {
      let operator = self.parse_operator()?;
      let mut right = self.parse_primary_expr()?;

      token_buffer = self.force_get();

      while self.is_binary_operator(&token_buffer)
        && get_token_precedence(&token_buffer) > precedence
      {
        // TODO: Are we adding the correct amount of precedence here? Shouldn't there be a higher difference in precedence?
        // TODO: This isn't tail-recursive?
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
        unique_id,
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

  // TODO: Better naming and/or positioning for logic.
  fn parse_expr(&mut self) -> ParserResult<ast::Node> {
    // TODO: Add support for unary expressions (such as `!`, '-', etc.).

    let starting_expr = self.parse_primary_expr()?;

    // TODO: Should the precedence be zero here?
    Ok(self.parse_binary_expr_or_default(starting_expr, 0)?)
  }

  /// %expr '(' (%expr (,))* ')'
  fn parse_call_expr(&mut self, callee_expr: ast::Node) -> ParserResult<ast::CallExpr> {
    // FIXME: On the expressions being parsed, the pattern may not be parsed as a pattern linking to a function or extern. Ensure this is actually the case.
    // let callee_pattern = self.parse_pattern(name_resolution::SymbolKind::FunctionOrExtern)?;

    self.skip_past(&lexer::TokenKind::SymbolParenthesesL)?;

    let mut arguments = vec![];

    while self.until(&lexer::TokenKind::SymbolParenthesesR)? {
      arguments.push(self.parse_expr()?);

      // TODO: What if the comma is omitted?
      if self.is(&lexer::TokenKind::SymbolComma) {
        self.skip();
      }
    }

    self.skip_past(&lexer::TokenKind::SymbolParenthesesR)?;

    Ok(ast::CallExpr {
      callee_expr: Box::new(callee_expr),
      arguments,
    })
  }

  /// '~' %name '(' (%expr (,))* ')'
  fn parse_intrinsic_call(&mut self) -> ParserResult<ast::IntrinsicCall> {
    self.skip_past(&lexer::TokenKind::SymbolTilde)?;

    let kind = match self.parse_name()?.as_str() {
      "panic" => ast::IntrinsicKind::Panic,
      _ => return Err(self.expected("valid intrinsic name")),
    };

    self.skip_past(&lexer::TokenKind::SymbolParenthesesL)?;

    let mut arguments = vec![];

    while self.until(&lexer::TokenKind::SymbolParenthesesR)? {
      arguments.push(self.parse_expr()?);

      // TODO: What if the comma is omitted?
      if self.is(&lexer::TokenKind::SymbolComma) {
        self.skip();
      }
    }

    self.skip_past(&lexer::TokenKind::SymbolParenthesesR)?;

    Ok(ast::IntrinsicCall { kind, arguments })
  }

  /// %pattern
  fn parse_reference(&mut self) -> ParserResult<ast::Reference> {
    let pattern = self.parse_pattern(name_resolution::SymbolKind::Definition)?;

    Ok(ast::Reference(pattern))
  }

  /// %expr '=' %expr
  fn parse_assign_stmt(&mut self, assignee_expr: ast::Node) -> ParserResult<ast::AssignStmt> {
    self.skip_past(&lexer::TokenKind::SymbolEqual)?;

    let value = Box::new(self.parse_expr()?);

    Ok(ast::AssignStmt {
      assignee_expr: Box::new(assignee_expr),
      value,
    })
  }

  /// enum %name '{' (%name (','))* '}'
  fn parse_enum(&mut self) -> ParserResult<ast::Enum> {
    self.skip_past(&lexer::TokenKind::KeywordEnum)?;

    let name = self.parse_name()?;

    self.skip_past(&lexer::TokenKind::SymbolBraceL)?;

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

    Ok(ast::Enum {
      name: name.clone(),
      variants,
    })
  }

  /// struct %name '{' (%name ':' %type ';')* '}'
  fn parse_struct_type(&mut self) -> ParserResult<ast::StructType> {
    self.skip_past(&lexer::TokenKind::KeywordStruct)?;

    let name = self.parse_name()?;

    self.skip_past(&lexer::TokenKind::SymbolBraceL)?;

    let mut fields = Vec::new();

    while self.until(&lexer::TokenKind::SymbolBraceR)? {
      let field_name = self.parse_name()?;

      self.skip_past(&lexer::TokenKind::SymbolColon)?;

      let field_type = self.parse_type()?;

      self.skip_past(&lexer::TokenKind::SymbolComma)?;
      fields.push((field_name, field_type));
    }

    // Skip the closing brace symbol.
    self.skip();

    Ok(ast::StructType {
      name: name.clone(),
      fields,
      unique_id: self.cache.create_unique_id(),
    })
  }

  /// new %name '{' (%name ':' %expr ',')* '}'
  fn parse_struct_value(&mut self) -> ParserResult<ast::StructValue> {
    self.skip_past(&lexer::TokenKind::KeywordNew)?;

    // TODO: Shouldn't it be `ScopeQualifier`?
    let struct_name = self.parse_name()?;

    self.skip_past(&lexer::TokenKind::SymbolBraceL)?;

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
      struct_name,
      fields,
      target_id: None,
    })
  }

  /// fn '[' (%name (','))* ']' %prototype %block
  fn parse_closure(&mut self) -> ParserResult<ast::Closure> {
    self.skip_past(&lexer::TokenKind::KeywordFn)?;

    let mut captures = Vec::new();

    if self.is(&lexer::TokenKind::SymbolBracketL) {
      self.skip();

      while self.until(&lexer::TokenKind::SymbolBracketR)? {
        captures.push((self.parse_name()?, None));
      }

      self.skip();
    }

    let prototype = self.parse_prototype()?;
    let body = self.parse_block()?;

    Ok(ast::Closure {
      captures,
      prototype,
      body,
    })
  }

  fn parse_struct_impl(&mut self) -> ParserResult<ast::StructImpl> {
    self.skip_past(&lexer::TokenKind::KeywordImpl)?;

    let mut struct_pattern = self.parse_pattern(name_resolution::SymbolKind::Type)?;
    let mut trait_pattern = None;

    if self.is(&lexer::TokenKind::KeywordFor) {
      trait_pattern = Some(struct_pattern);
      self.skip();
      struct_pattern = self.parse_pattern(name_resolution::SymbolKind::Type)?;
    }

    self.skip_past(&lexer::TokenKind::SymbolBraceL)?;

    // TODO: Support for trait specifications/implementations.

    let mut methods = Vec::new();

    while self.until(&lexer::TokenKind::SymbolBraceR)? {
      // TODO: Support for attributes.
      let method = self.parse_function(Vec::new())?;

      methods.push(method);
    }

    self.skip_past(&lexer::TokenKind::SymbolBraceR)?;

    Ok(ast::StructImpl {
      // TODO: Support for trait specialization.
      is_default: false,
      target_struct_pattern: struct_pattern,
      trait_pattern,
      methods,
    })
  }

  fn parse_trait(&mut self) -> ParserResult<ast::Trait> {
    self.skip_past(&lexer::TokenKind::KeywordTrait)?;

    let name = self.parse_name()?;

    self.skip_past(&lexer::TokenKind::SymbolBraceL)?;

    let mut methods = Vec::new();

    while self.until(&lexer::TokenKind::SymbolBraceR)? {
      self.skip_past(&lexer::TokenKind::KeywordFn)?;

      let method_name = self.parse_name()?;
      let prototype = self.parse_prototype()?;

      methods.push((method_name, prototype));
    }

    self.skip_past(&lexer::TokenKind::SymbolBraceR)?;

    Ok(ast::Trait {
      name: name.clone(),
      methods,
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
    parser.tokens.push((lexer::TokenKind::KeywordFn, 0));
    assert!(parser.is(&lexer::TokenKind::KeywordFn));
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

    assert!(parser.is_eof());
    parser.tokens.push((lexer::TokenKind::KeywordFn, 0));
    assert!(parser.is_eof());
    parser.tokens.push((lexer::TokenKind::KeywordFn, 0));
    assert!(!parser.is_eof());
    parser.skip();
    assert!(parser.is_eof());
  }

  #[test]
  fn after_pattern_is() {
    let mut context = cache::Cache::new();

    let mut parser = Parser::new(
      vec![(lexer::TokenKind::Identifier("test".to_string()), 0)],
      &mut context,
    );

    parser.tokens.push((lexer::TokenKind::SymbolBraceL, 0));
    assert!(parser.after_pattern_is(&lexer::TokenKind::SymbolBraceL));
    parser.tokens.pop();
    parser.tokens.push((lexer::TokenKind::SymbolDot, 0));

    parser
      .tokens
      .push((lexer::TokenKind::Identifier("foo".to_string()), 0));

    parser.tokens.push((lexer::TokenKind::SymbolBraceL, 0));
    assert!(parser.after_pattern_is(&lexer::TokenKind::SymbolBraceL));
  }

  #[test]
  fn is_binary_operator() {
    let mut context = cache::Cache::new();
    let mut parser = Parser::new(Vec::new(), &mut context);

    assert!(!parser.is_binary_operator(&lexer::TokenKind::SymbolBraceL));
    assert!(parser.is_binary_operator(&lexer::TokenKind::SymbolPlus));
    assert!(!parser.is_binary_operator(&lexer::TokenKind::SymbolEqual));
    parser.tokens.push((lexer::TokenKind::SymbolEqual, 0));
    assert!(!parser.is_binary_operator(&lexer::TokenKind::SymbolEqual));
    parser.tokens.push((lexer::TokenKind::SymbolEqual, 0));
    assert!(parser.is_binary_operator(&lexer::TokenKind::SymbolEqual));
    assert!(parser.is_binary_operator(&lexer::TokenKind::KeywordAnd));
    assert!(!parser.is_binary_operator(&lexer::TokenKind::EOF));
  }

  #[test]
  fn peek_is() {
    let mut context = cache::Cache::new();
    let mut parser = Parser::new(Vec::new(), &mut context);

    assert!(!parser.peek_is(&lexer::TokenKind::SymbolBraceL));
    parser.tokens.push((lexer::TokenKind::SymbolBraceL, 0));
    assert!(!parser.peek_is(&lexer::TokenKind::SymbolBraceL));
    parser.tokens.push((lexer::TokenKind::SymbolBraceL, 0));
    assert!(parser.peek_is(&lexer::TokenKind::SymbolBraceL));
  }

  // TODO: Add more tests.
}
