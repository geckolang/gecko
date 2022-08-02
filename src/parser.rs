use crate::{
  ast::{self, BlockExpr},
  lexer, name_resolution, symbol_table,
};

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

fn get_token_precedence(token: &lexer::TokenKind) -> usize {
  // FIXME: What about the `not` operator, and others?
  match token {
    lexer::TokenKind::Plus
    | lexer::TokenKind::Minus
    | lexer::TokenKind::Equality
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

type ParserResult<T> = Result<T, ast::Diagnostic>;

pub struct Parser<'a> {
  tokens: Vec<lexer::Token>,
  index: usize,
  cache: &'a mut symbol_table::SymbolTable,
}

impl<'a> Parser<'a> {
  pub fn new(tokens: Vec<lexer::Token>, cache: &'a mut symbol_table::SymbolTable) -> Self {
    Self {
      tokens,
      index: 0,
      cache,
    }
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
        | lexer::TokenKind::Equality
        | lexer::TokenKind::In
    )
  }

  /// Parse all top-level definitions.
  pub fn parse_all(&mut self) -> ParserResult<Vec<ast::NodeKind>> {
    let mut result = Vec::new();

    while !self.is_eof() {
      result.push(self.parse_root_node()?);
    }

    Ok(result)
  }

  fn current_location(&self) -> usize {
    // TODO: Handle EOF or out of bounds errors.
    let token = &self.tokens[self.index];

    token.1
  }

  fn seal_location(&self, start_location: usize) -> (usize, usize) {
    // TODO: Missing end-location?
    (start_location, self.current_location())
  }

  fn skip_past(&mut self, token_kind: &lexer::TokenKind) -> ParserResult<()> {
    if !self.is(token_kind) {
      return Err(self.expected(format!("token `{:?}`", token_kind).as_str()));
    }

    self.skip()?;

    Ok(())
  }

  fn expected(&self, expected: &str) -> ast::Diagnostic {
    codespan_reporting::diagnostic::Diagnostic::error().with_message(format!(
      "expected {}, but got `{:?}`",
      expected,
      self.get_token().unwrap_or(&lexer::TokenKind::EOF)
    ))
  }

  fn is_unary_operator(&self) -> bool {
    matches!(
      self.get_token().unwrap_or(&lexer::TokenKind::EOF),
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

  fn get_token(&self) -> ParserResult<&lexer::TokenKind> {
    if let Some(token) = self.tokens.get(self.index) {
      Ok(&token.0)
    } else {
      Err(
        codespan_reporting::diagnostic::Diagnostic::error()
          .with_message("unexpectedly reached end of file while retrieving current token"),
      )
    }
  }

  /// Compare the current token to the given one for equality.
  ///
  /// If `EOF` has been reached, `false` will always be returned. This is
  /// to avoid infinite loops that rely on this check as their condition.
  fn is(&self, token: &lexer::TokenKind) -> bool {
    if self.index >= self.tokens.len() {
      return false;
    }

    self.get_token().unwrap_or(&lexer::TokenKind::EOF) == token
  }

  /// Attempt to reposition the index to the next token (if any).
  ///
  /// Once the index reaches the length of the token list + 1, any
  /// attempt to skip will result in an `Err` being returned, indicating
  /// that the end-of-file has been reached.
  fn skip(&mut self) -> ParserResult<()> {
    if self.index >= self.tokens.len() {
      return Err(
        codespan_reporting::diagnostic::Diagnostic::error()
          .with_message("unexpectedly reached end of file while attempting to skip token"),
      );
    }

    self.index += 1;

    Ok(())
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
    self.tokens.is_empty() || self.index >= self.tokens.len() - 1
  }

  fn parse_pattern(
    &mut self,
    symbol_kind: name_resolution::SymbolKind,
  ) -> ParserResult<ast::Pattern> {
    let starting_name = self.parse_name()?;

    let qualifier = if self.is(&lexer::TokenKind::DoubleColon) {
      self.skip()?;

      Some(name_resolution::Qualifier {
        package_name: starting_name.clone(),
        module_name: self.parse_name()?,
      })
    } else {
      None
    };

    let base_name = if qualifier.is_none() {
      starting_name
    } else {
      self.skip_past(&lexer::TokenKind::DoubleColon)?;

      self.parse_name()?
    };

    let sub_name = if self.is(&lexer::TokenKind::DoubleColon) {
      self.skip_past(&lexer::TokenKind::DoubleColon)?;

      Some(self.parse_name()?)
    } else {
      None
    };

    // TODO: Add support for static sub-entities.

    Ok(ast::Pattern {
      link_id: self.cache.next_id(),
      qualifier,
      base_name,
      sub_name,
      symbol_kind,
    })
  }

  fn parse_name(&mut self) -> ParserResult<String> {
    // REVIEW: Illegal/unrecognized tokens MAY also be represented under 'Identifier'? Is this a problem?

    let name = match self.get_token()? {
      lexer::TokenKind::Identifier(value) => value.clone(),
      _ => return Err(self.expected("identifier")),
    };

    self.skip()?;

    Ok(name)
  }

  fn parse_statement(&mut self) -> ParserResult<ast::NodeKind> {
    let node = match self.get_token()? {
      lexer::TokenKind::Return => ast::NodeKind::ReturnStmt(self.parse_return_stmt()?),
      lexer::TokenKind::Let | lexer::TokenKind::Const => {
        ast::NodeKind::BindingStmt(std::rc::Rc::new(self.parse_binding_stmt()?))
      }
      lexer::TokenKind::Unsafe => ast::NodeKind::UnsafeExpr(self.parse_unsafe_expr()?),
      _ => {
        let expr = Box::new(self.parse_expr()?);
        let inline_expr_stmt = ast::NodeKind::InlineExprStmt(ast::InlineExprStmt { expr });

        // FIXME: Temporary workaround for yield expressions.
        // if self.is(&lexer::TokenKind::SemiColon) {
        //   self.skip()?;
        // }
        // self.skip_past(&lexer::TokenKind::SemiColon)?;

        inline_expr_stmt
      }
    };

    Ok(node)
  }

  fn parse_indent(&mut self) -> ParserResult<()> {
    self.skip_past(&lexer::TokenKind::Indent)
  }

  fn parse_dedent(&mut self) -> ParserResult<()> {
    self.skip_past(&lexer::TokenKind::Dedent)
  }

  /// {%indent (%statement+) %dedent | '=' {%statement | %expr}}
  fn parse_block_expr(&mut self) -> ParserResult<BlockExpr> {
    let mut statements = Vec::new();
    let mut yields = None;

    self.parse_indent()?;

    // REVIEW: What if the last statement is a let-statement? Let-statements have inferrable types, used internally.
    // REVIEW: This may be the reason why let-statements shouldn't have inferrable types (only internally).
    loop {
      // REVIEW: Is it okay to have exclusive syntax here, and instead not treat it as a statement?
      if self.is(&lexer::TokenKind::Yield) {
        self.skip()?;
        yields = Some(Box::new(self.parse_expr()?));

        break;
      } else if self.is(&lexer::TokenKind::Pass) {
        self.skip()?;

        break;
      }

      statements.push(self.parse_statement()?);

      if self.is(&lexer::TokenKind::Dedent) {
        break;
      }
    }

    self.parse_dedent()?;

    Ok(ast::BlockExpr {
      statements,
      yields,
      id: self.cache.next_id(),
    })
  }

  /// {U8 | U16 | U32 | U64 | I8 | I16 | Int | I64}
  fn parse_int_type(&mut self) -> ParserResult<ast::BasicType> {
    let size = match self.get_token()? {
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

    self.skip()?;

    Ok(ast::BasicType::Int(size))
  }

  /// Bool
  fn parse_bool_type(&mut self) -> ParserResult<ast::BasicType> {
    self.skip_past(&lexer::TokenKind::TypeBool)?;

    Ok(ast::BasicType::Bool)
  }

  // TODO: Specialize return types of the `parse_type_x` functions to their actual types instead of the `ast::Type` enum wrapper?
  fn parse_this_type(&mut self) -> ParserResult<ast::Type> {
    self.skip_past(&lexer::TokenKind::TypeThis)?;

    // BUG: This must be filled here, and it should no longer be `Option<>`. AST is always immutable.
    Ok(ast::Type::This(ast::ThisType { target_id: None }))
  }

  /// '[' %type, 0-9+ ']'
  fn parse_array_type(&mut self) -> ParserResult<ast::Type> {
    self.skip_past(&lexer::TokenKind::BracketL)?;

    let element_type = Box::new(self.parse_type()?);

    self.skip_past(&lexer::TokenKind::Comma)?;

    let size = match self.get_token()? {
      lexer::TokenKind::Int(value) => value.clone() as u32,
      _ => return Err(self.expected("array size")),
    };

    self.skip()?;
    self.skip_past(&lexer::TokenKind::BracketR)?;

    Ok(ast::Type::StaticIndexable(element_type, size))
  }

  fn parse_type(&mut self) -> ParserResult<ast::Type> {
    // TODO: Support for more types.
    let mut ty = match self.get_token()? {
      // TODO: Other types as well.
      lexer::TokenKind::TypeInt8
      | lexer::TokenKind::TypeInt16
      | lexer::TokenKind::TypeInt32
      | lexer::TokenKind::TypeInt64
      | lexer::TokenKind::TypeUint8
      | lexer::TokenKind::TypeUint16
      | lexer::TokenKind::TypeUint32
      | lexer::TokenKind::TypeUint64 => ast::Type::Basic(self.parse_int_type()?),
      lexer::TokenKind::TypeBool => ast::Type::Basic(self.parse_bool_type()?),
      lexer::TokenKind::Identifier(_) => ast::Type::Stub(self.parse_stub_type()?),
      lexer::TokenKind::BracketL => self.parse_array_type()?,
      lexer::TokenKind::Asterisk => {
        self.skip()?;

        ast::Type::Pointer(Box::new(self.parse_type()?))
      }
      lexer::TokenKind::TypeString => {
        self.skip()?;

        ast::Type::Basic(ast::BasicType::String)
      }
      lexer::TokenKind::TypeThis => self.parse_this_type()?,
      _ => return Err(self.expected("type")),
    };

    // Upgrade to a function type, if applicable.
    if self.is(&lexer::TokenKind::Arrow) {
      ty = ast::Type::Signature(self.parse_signature_type(ty)?);
    }

    Ok(ty)
  }

  /// fn '(' (%type (','))* ')' ('[' %type ']')
  fn parse_signature_type(&mut self, first_type: ast::Type) -> ParserResult<ast::SignatureType> {
    self.skip_past(&lexer::TokenKind::Arrow)?;

    let mut parameter_types = vec![first_type, self.parse_type()?];

    while self.is(&lexer::TokenKind::Arrow) {
      self.skip()?;
      parameter_types.push(self.parse_type()?);
    }

    let return_type = Box::new(parameter_types.remove(parameter_types.len() - 1));

    // TODO: Commented out.
    // Support for no parameters.
    // if parameter_types.len() == 1 && parameter_types[0] == ast::Type::Unit {
    //   parameter_types.clear();
    // }

    Ok(ast::SignatureType {
      parameter_types,
      return_type,
      // TODO: Support for variadic functions types? Such as a reference to an extern that is variadic? Think/investigate. Remember that externs may only be invoked from unsafe blocks.
      is_variadic: false,
    })
  }

  /// %pattern
  fn parse_stub_type(&mut self) -> ParserResult<ast::StubType> {
    let pattern = self.parse_pattern(name_resolution::SymbolKind::Type)?;

    Ok(ast::StubType { pattern })
  }

  /// %name ':' %type
  fn parse_parameter(&mut self, position: u32) -> ParserResult<ast::Parameter> {
    let name = self.parse_name()?;

    let type_hint = if self.is(&lexer::TokenKind::Colon) {
      self.skip()?;

      Some(self.parse_type()?)
    } else {
      None
    };

    let parameter = ast::Parameter {
      name,
      type_hint,
      position,
      id: self.cache.next_id(),
    };

    Ok(parameter)
  }

  fn parse_this_parameter(&mut self) -> ParserResult<ast::Parameter> {
    self.skip_past(&lexer::TokenKind::This)?;

    let type_hint = Some(ast::Type::This(ast::ThisType { target_id: None }));

    let parameter = ast::Parameter {
      name: String::from("this"),
      type_hint,
      position: 0,
      id: self.cache.next_id(),
    };

    Ok(parameter)
  }

  /// '(' {%parameter* (,)} (+) ')' ':' %type
  fn parse_signature(&mut self, is_extern: bool) -> ParserResult<ast::Signature> {
    self.skip_past(&lexer::TokenKind::ParenthesesL)?;

    let mut parameters = Vec::new();
    let mut is_variadic = false;
    let mut parameter_index_counter = 0;
    let mut accepts_instance = false;

    let this_parameter = if self.is(&lexer::TokenKind::This) {
      parameter_index_counter += 1;
      accepts_instance = true;

      Some(self.parse_this_parameter()?)
    } else {
      None
    };

    if this_parameter.is_some() && !self.is(&lexer::TokenKind::ParenthesesR) {
      self.skip_past(&lexer::TokenKind::Comma)?;
    }

    // REVISE: Analyze, and remove possibility of lonely comma.
    while self.until(&lexer::TokenKind::ParenthesesR)? {
      if self.is(&lexer::TokenKind::LongEllipsis) {
        is_variadic = true;
        self.skip()?;

        break;
      }

      parameters.push(std::rc::Rc::new(
        self.parse_parameter(parameter_index_counter)?,
      ));

      parameter_index_counter += 1;

      if !self.is(&lexer::TokenKind::Comma) {
        break;
      }

      self.skip()?;
    }

    self.skip_past(&lexer::TokenKind::ParenthesesR)?;

    let return_type_hint = if self.is(&lexer::TokenKind::Arrow) {
      self.skip()?;

      Some(self.parse_type()?)
    } else {
      None
    };

    Ok(ast::Signature {
      parameters,
      return_type_hint,
      is_variadic,
      accepts_instance,
      instance_type_id: None,
      return_type_id: self.cache.next_id(),
      this_parameter,
      is_extern,
    })
  }

  /// fn %signature %block
  fn parse_function(
    &mut self,
    static_owner_name: Option<String>,
    attributes: Vec<ast::Attribute>,
  ) -> ParserResult<ast::Function> {
    // TODO: Support for visibility.
    self.skip_past(&lexer::TokenKind::Func)?;

    let name = self.parse_name()?;

    let generics = if self.is(&lexer::TokenKind::LessThan) {
      Some(self.parse_generics()?)
    } else {
      None
    };

    let signature = self.parse_signature(false)?;

    self.skip_past(&lexer::TokenKind::Colon)?;

    let body = std::rc::Rc::new(self.parse_block_expr()?);
    let function_id = self.cache.next_id();

    let function = ast::Function {
      name,
      static_owner_name,
      signature,
      body,
      attributes,
      id: function_id.clone(),
      generics,
    };

    Ok(function)
  }

  /// extern fn %signature
  fn parse_extern_function(
    &mut self,
    attributes: Vec<ast::Attribute>,
  ) -> ParserResult<ast::ExternFunction> {
    // REVIEW: Support for visibility?

    self.skip_past(&lexer::TokenKind::Extern)?;
    self.skip_past(&lexer::TokenKind::Func)?;

    let name = self.parse_name()?;
    let signature = self.parse_signature(true)?;

    let extern_function = ast::ExternFunction {
      name,
      signature,
      attributes,
      id: self.cache.next_id(),
    };

    Ok(extern_function)
  }

  /// extern static %name ':' %type
  fn parse_extern_static(&mut self) -> ParserResult<ast::ExternStatic> {
    self.skip_past(&lexer::TokenKind::Extern)?;
    self.skip_past(&lexer::TokenKind::Static)?;

    let name = self.parse_name()?;

    self.skip_past(&lexer::TokenKind::Colon)?;

    let ty = self.parse_type()?;
    let extern_static_id = self.cache.next_id();

    let extern_static = ast::ExternStatic {
      name,
      ty,
      id: extern_static_id.clone(),
    };

    Ok(extern_static)
  }

  /// '@' %name ('(' (%literal (','))* ')')
  fn parse_attribute(&mut self) -> ParserResult<ast::Attribute> {
    self.skip_past(&lexer::TokenKind::At)?;

    let name = self.parse_name()?;
    let mut values = Vec::new();

    if self.is(&lexer::TokenKind::ParenthesesL) {
      self.skip()?;

      while self.until(&lexer::TokenKind::ParenthesesR)? {
        values.push(self.parse_literal()?);

        // REVIEW: Review this comma-skipping system.
        if !self.is(&lexer::TokenKind::Comma) {
          break;
        }

        self.skip()?;
      }

      self.skip_past(&lexer::TokenKind::ParenthesesR)?;
    }

    Ok(ast::Attribute { name, values })
  }

  /// {%function | %extern_function | %extern_static | %type_alias | %enum | %struct_type}
  fn parse_root_node(&mut self) -> ParserResult<ast::NodeKind> {
    // REVIEW: Why not move this check into the `get()` method?
    if self.is_eof() {
      return Err(
        codespan_reporting::diagnostic::Diagnostic::error()
          .with_message("expected top-level construct but got end of file"),
      );
    }

    let mut attributes: Vec<ast::Attribute> = Vec::new();

    while self.is(&lexer::TokenKind::At) {
      let attribute = self.parse_attribute()?;

      if attributes
        .iter()
        .any(|attribute| attribute.name == attribute.name)
      {
        return Err(
          codespan_reporting::diagnostic::Diagnostic::error()
            .with_message(format!("duplicate attribute `{}`", attribute.name)),
        );
      }

      attributes.push(attribute);
    }

    let is_attributable = matches!(
      self.get_token()?,
      lexer::TokenKind::Func | lexer::TokenKind::Extern
    );

    if !attributes.is_empty() && !is_attributable {
      return Err(
        codespan_reporting::diagnostic::Diagnostic::error()
          .with_message("attributes may only be attached to functions or externs"),
      );
    }

    let token = self.get_token();

    Ok(match token? {
      lexer::TokenKind::Func => {
        ast::NodeKind::Function(std::rc::Rc::new(self.parse_function(None, attributes)?))
      }
      lexer::TokenKind::Extern if self.peek_is(&lexer::TokenKind::Func) => {
        ast::NodeKind::ExternFunction(std::rc::Rc::new(self.parse_extern_function(attributes)?))
      }
      lexer::TokenKind::Extern if self.peek_is(&lexer::TokenKind::Static) => {
        ast::NodeKind::ExternStatic(std::rc::Rc::new(self.parse_extern_static()?))
      }
      lexer::TokenKind::Enum => ast::NodeKind::Enum(std::rc::Rc::new(self.parse_enum()?)),
      lexer::TokenKind::Struct => ast::NodeKind::Struct(std::rc::Rc::new(self.parse_struct()?)),
      lexer::TokenKind::Type => {
        ast::NodeKind::TypeAlias(std::rc::Rc::new(self.parse_type_alias()?))
      }
      lexer::TokenKind::Impl => ast::NodeKind::StructImpl(self.parse_struct_impl()?),
      lexer::TokenKind::Trait => ast::NodeKind::Trait(std::rc::Rc::new(self.parse_trait()?)),
      lexer::TokenKind::Using => ast::NodeKind::Import(self.parse_import()?),
      _ => return Err(self.expected("top-level construct")),
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
      id: self.cache.next_id(),
    })
  }

  /// return (%expr)
  fn parse_return_stmt(&mut self) -> ParserResult<ast::ReturnStmt> {
    self.skip_past(&lexer::TokenKind::Return)?;

    // TODO: New line or EOF.
    let value = if !self.is(&lexer::TokenKind::Whitespace('\n')) {
      Some(Box::new(self.parse_expr()?))
    } else {
      self.skip()?;

      None
    };

    Ok(ast::ReturnStmt { value })
  }

  /// {let | var | const} %name (':' %type) '=' %expr
  fn parse_binding_stmt(&mut self) -> ParserResult<ast::BindingStmt> {
    let is_const_expr = match self.get_token()? {
      lexer::TokenKind::Let => false,
      lexer::TokenKind::Const => true,
      _ => return Err(self.expected("binding modifier")),
    };

    self.skip()?;

    let name = self.parse_name()?;

    let type_hint = if self.is(&lexer::TokenKind::Colon) {
      self.skip()?;

      Some(self.parse_type()?)
    } else {
      None
    };

    self.skip_past(&lexer::TokenKind::Equal)?;

    let value = Box::new(self.parse_expr()?);

    // TODO: Value should be treated as rvalue, unless its using an address-of operator. Find out how to translate this to logic.

    Ok(ast::BindingStmt {
      name,
      value,
      is_const_expr,
      id: self.cache.next_id(),
      type_hint,
    })
  }

  /// if %expr %block (else %block)
  fn parse_if_expr(&mut self) -> ParserResult<ast::IfExpr> {
    self.skip_past(&lexer::TokenKind::If)?;

    let condition = Box::new(self.parse_expr()?);

    self.skip_past(&lexer::TokenKind::Colon)?;

    let then_value = Box::new(self.parse_expr()?);
    let mut alternative_branches = Vec::new();

    while self.is(&lexer::TokenKind::Elif) {
      self.skip_past(&lexer::TokenKind::Elif)?;

      let alternative_condition = self.parse_expr()?;

      self.skip_past(&lexer::TokenKind::Colon)?;

      let alternative_value = self.parse_expr()?;

      alternative_branches.push((alternative_condition, alternative_value));
    }

    let else_value = if self.is(&lexer::TokenKind::Else) {
      self.skip()?;
      self.skip_past(&lexer::TokenKind::Colon)?;

      Some(Box::new(self.parse_expr()?))
    } else {
      None
    };

    Ok(ast::IfExpr {
      id: self.cache.next_id(),
      condition,
      then_branch: then_value,
      alternative_branches,
      else_branch: else_value,
    })
  }

  // unsafe %expr
  fn parse_unsafe_expr(&mut self) -> ParserResult<ast::UnsafeExpr> {
    // REVIEW: Why not merge this with the normal block, and just have a flag?

    self.skip_past(&lexer::TokenKind::Unsafe)?;
    self.skip_past(&lexer::TokenKind::Colon)?;

    Ok(ast::UnsafeExpr(Box::new(self.parse_expr()?)))
  }

  /// {true | false}
  fn parse_bool_literal(&mut self) -> ParserResult<ast::Literal> {
    // REVISE: There shouldn't be a need to clone the token here.
    Ok(match self.get_token()?.clone() {
      lexer::TokenKind::Bool(value) => {
        self.skip()?;

        ast::Literal::Bool(value.clone())
      }
      _ => return Err(self.expected("boolean literal")),
    })
  }

  /// 0-9+
  fn parse_int_literal(&mut self) -> ParserResult<ast::Literal> {
    // REVISE: There shouldn't be a need to clone the token here.
    Ok(match self.get_token()?.clone() {
      lexer::TokenKind::Int(value) => {
        self.skip()?;

        let minimum_size = minimum_int_size_of(&value);

        // REVIEW: Deal with unsigned integers here?
        // Default size to 32 bit-width.
        let size = if minimum_size < ast::IntSize::I32 {
          ast::IntSize::I32
        } else {
          minimum_size
        };

        ast::Literal::Int(value.clone(), size)
      }
      _ => return Err(self.expected("integer literal")),
    })
  }

  /// '"' [^"]* '"'
  fn parse_string_literal(&mut self) -> ParserResult<ast::Literal> {
    let result = match self.get_token()? {
      lexer::TokenKind::String(value) => ast::Literal::String(value.clone()),
      _ => return Err(self.expected("string literal")),
    };

    self.skip()?;

    Ok(result)
  }

  /// '[' (%expr (','))* ']'
  fn parse_array(&mut self) -> ParserResult<ast::Array> {
    let mut elements = Vec::new();

    self.skip_past(&lexer::TokenKind::BracketL)?;

    while self.until(&lexer::TokenKind::BracketR)? {
      elements.push(self.parse_expr()?);

      // REVIEW: What if the comma isn't provided?
      if self.is(&lexer::TokenKind::Comma) {
        self.skip()?;
      }
    }

    // Skip the closing bracket.
    self.skip()?;

    Ok(ast::Array {
      elements,
      id: self.cache.next_id(),
      element_type_id: self.cache.next_id(),
    })
  }

  /// %expr '[' %expr ']'
  fn parse_indexing_expr(&mut self, target_expr: ast::NodeKind) -> ParserResult<ast::IndexingExpr> {
    self.skip_past(&lexer::TokenKind::BracketL)?;

    let index_expr = Box::new(self.parse_expr()?);

    self.skip_past(&lexer::TokenKind::BracketR)?;

    Ok(ast::IndexingExpr {
      target_expr: Box::new(target_expr),
      index_expr,
    })
  }

  fn parse_nullptr_literal(&mut self) -> ParserResult<ast::Literal> {
    self.skip_past(&lexer::TokenKind::Nullptr)?;
    self.skip_past(&lexer::TokenKind::BracketL)?;

    let ty = self.parse_type()?;

    self.skip_past(&lexer::TokenKind::BracketR)?;

    Ok(ast::Literal::Nullptr(self.cache.next_id(), Some(ty)))
  }

  fn parse_sizeof_intrinsic(&mut self) -> ParserResult<ast::SizeofIntrinsic> {
    self.skip_past(&lexer::TokenKind::Sizeof)?;
    self.skip_past(&lexer::TokenKind::ParenthesesL)?;

    let ty = self.parse_type()?;

    self.skip_past(&lexer::TokenKind::ParenthesesR)?;

    Ok(ast::SizeofIntrinsic { ty })
  }

  fn parse_literal(&mut self) -> ParserResult<ast::Literal> {
    Ok(match self.get_token()? {
      lexer::TokenKind::Bool(_) => self.parse_bool_literal()?,
      lexer::TokenKind::Int(_) => self.parse_int_literal()?,
      lexer::TokenKind::Char(_) => self.parse_char_literal()?,
      lexer::TokenKind::String(_) => self.parse_string_literal()?,
      lexer::TokenKind::Nullptr => self.parse_nullptr_literal()?,
      _ => return Err(self.expected("literal")),
    })
  }

  fn parse_char_literal(&mut self) -> ParserResult<ast::Literal> {
    let result = match self.get_token()? {
      lexer::TokenKind::Char(value) => ast::Literal::Char(value.clone()),
      _ => return Err(self.expected("char literal")),
    };

    self.skip()?;

    Ok(result)
  }

  fn is_promotion_chain(&self) -> bool {
    if self.is_eof() {
      return false;
    }

    matches!(
      self.get_token().unwrap_or(&lexer::TokenKind::EOF),
      lexer::TokenKind::Dot | lexer::TokenKind::ParenthesesL | lexer::TokenKind::BracketL
    )
  }

  /// '?' {%name | sizeof}
  fn parse_intrinsic(&mut self) -> ParserResult<ast::NodeKind> {
    self.skip_past(&lexer::TokenKind::QuestionMark)?;

    Ok(match self.get_token()? {
      lexer::TokenKind::Sizeof => ast::NodeKind::SizeofIntrinsic(self.parse_sizeof_intrinsic()?),
      lexer::TokenKind::Identifier(_) => ast::NodeKind::IntrinsicCall(self.parse_intrinsic_call()?),
      _ => return Err(self.expected("intrinsic")),
    })
  }

  fn parse_primary_expr(&mut self) -> ParserResult<ast::NodeKind> {
    let mut node = match self.get_token()? {
      lexer::TokenKind::Int(_) if self.peek_is(&lexer::TokenKind::ShortEllipsis) => {
        ast::NodeKind::Range(self.parse_range()?)
      }
      // REVIEW: Possible redundant check after the fn keyword. But how do we know we're still not on a block and accidentally parse a function as a closure?
      lexer::TokenKind::Func
        if (self.peek_is(&lexer::TokenKind::BracketL)
          || self.peek_is(&lexer::TokenKind::ParenthesesL)) =>
      {
        ast::NodeKind::Closure(self.parse_closure()?)
      }
      lexer::TokenKind::If => ast::NodeKind::IfExpr(self.parse_if_expr()?),
      lexer::TokenKind::Identifier(_) => ast::NodeKind::Reference(self.parse_reference()?),
      lexer::TokenKind::BracketL => ast::NodeKind::Array(self.parse_array()?),
      lexer::TokenKind::New => ast::NodeKind::StructValue(self.parse_struct_value()?),
      lexer::TokenKind::Indent => {
        ast::NodeKind::BlockExpr(std::rc::Rc::new(self.parse_block_expr()?))
      }
      lexer::TokenKind::Unsafe => ast::NodeKind::UnsafeExpr(self.parse_unsafe_expr()?),
      lexer::TokenKind::QuestionMark => self.parse_intrinsic()?,
      lexer::TokenKind::ParenthesesL => {
        ast::NodeKind::ParenthesesExpr(self.parse_parentheses_expr()?)
      }
      _ if self.is_unary_operator() => ast::NodeKind::UnaryExpr(self.parse_unary_expr()?),
      // Default to a literal if nothing else matched.
      _ => ast::NodeKind::Literal(self.parse_literal()?),
    };

    // Promote the node to a chain, if applicable.
    while self.is_promotion_chain() {
      node = match self.get_token()? {
        lexer::TokenKind::ParenthesesL => ast::NodeKind::CallExpr(self.parse_call_expr(node)?),
        lexer::TokenKind::Dot if self.peek_is(&lexer::TokenKind::As) => {
          ast::NodeKind::CastExpr(self.parse_cast_expr(node)?)
        }
        lexer::TokenKind::Dot => ast::NodeKind::MemberAccess(self.parse_member_access(node)?),
        lexer::TokenKind::BracketL => ast::NodeKind::IndexingExpr(self.parse_indexing_expr(node)?),
        _ => unreachable!(),
      };
    }

    Ok(node)
  }

  /// %expr '.' %name
  fn parse_member_access(&mut self, base_expr: ast::NodeKind) -> ParserResult<ast::MemberAccess> {
    self.skip_past(&lexer::TokenKind::Dot)?;

    Ok(ast::MemberAccess {
      base_expr: std::rc::Rc::new(base_expr),
      member_name: self.parse_name()?,
    })
  }

  /// {'+' | '-' | '*' | '/'}
  fn parse_operator(&mut self) -> ParserResult<ast::OperatorKind> {
    let operator = match self.get_token()? {
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
      lexer::TokenKind::Equality => ast::OperatorKind::Equality,
      // TODO: Implement logic for GTE & LTE.
      _ => return Err(self.expected("operator")),
    };

    self.skip()?;

    Ok(operator)
  }

  // REVISE: Move to use the Pratt-parsing technique instead to replace the non-tail recursive method.
  // ... Or, simply adjust this implementation to not be recursive.
  /// %expr %operator %expr
  fn parse_binary_expr_or_default(
    &mut self,
    left: ast::NodeKind,
    min_precedence: usize,
  ) -> ParserResult<ast::NodeKind> {
    // REVIEW: Is this logic correct?
    let mut token_buffer = if let Ok(token) = self.get_token() {
      token
    } else {
      return Ok(left);
    };

    let precedence = get_token_precedence(&token_buffer);
    let mut buffer = left;

    while Parser::is_binary_operator(&token_buffer) && (precedence > min_precedence) {
      let operator = self.parse_operator()?;
      let mut right = self.parse_primary_expr()?;

      token_buffer = self.get_token()?;

      while Parser::is_binary_operator(&token_buffer)
        && get_token_precedence(&token_buffer) > precedence
      {
        // REVIEW: Are we adding the correct amount of precedence here? Shouldn't there be a higher difference in precedence?
        // REVISE: This isn't tail-recursive.
        right = self.parse_binary_expr_or_default(right, precedence + 1)?;
        token_buffer = self.get_token()?;
      }

      let kind = ast::NodeKind::BinaryExpr(ast::BinaryExpr {
        left_operand: std::rc::Rc::new(buffer),
        right_operand: std::rc::Rc::new(right),
        operator,
      });

      buffer = kind;
    }

    Ok(buffer)
  }

  /// %expr '.' as '<' %type '>'
  fn parse_cast_expr(&mut self, operand: ast::NodeKind) -> ParserResult<ast::CastExpr> {
    // REVIEW: Consider having a dual syntax for ergonomics: `expr as T`.

    self.skip_past(&lexer::TokenKind::Dot)?;
    self.skip_past(&lexer::TokenKind::As)?;
    self.skip_past(&lexer::TokenKind::ParenthesesL)?;

    let cast_type = self.parse_type()?;

    self.skip_past(&lexer::TokenKind::ParenthesesR)?;

    Ok(ast::CastExpr {
      cast_type,
      operand: std::rc::Rc::new(operand),
    })
  }

  /// %operator %expr
  fn parse_unary_expr(&mut self) -> ParserResult<ast::UnaryExpr> {
    // Aid the user by providing a helpful diagnostic.
    if !self.is_unary_operator() {
      return Err(self.expected("unary operator"));
    }

    let operator = self.parse_operator()?;
    let expr = std::rc::Rc::new(self.parse_expr()?);

    // TODO:
    // if !matches!(operator, ast::OperatorKind::AddressOf) {
    //   expr.as_rvalue = true;
    // }

    Ok(ast::UnaryExpr {
      operator,
      operand: expr,
    })
  }

  // REVISE: Better naming and/or positioning for logic.
  fn parse_expr(&mut self) -> ParserResult<ast::NodeKind> {
    // TODO: Add support for unary expressions (such as `!`, '-', etc.).

    let starting_expr = self.parse_primary_expr()?;

    // REVIEW: Should the precedence be zero here?
    Ok(self.parse_binary_expr_or_default(starting_expr, 0)?)
  }

  /// %expr '(' (%expr (,))* ')'
  fn parse_call_expr(&mut self, callee_expr: ast::NodeKind) -> ParserResult<ast::CallExpr> {
    // REVIEW: On the expressions being parsed, the pattern may not be parsed as a pattern linking to a function or extern. Ensure this is actually the case.
    // let callee_pattern = self.parse_pattern(name_resolution::SymbolKind::FunctionOrExtern)?;

    self.skip_past(&lexer::TokenKind::ParenthesesL)?;

    let mut arguments = Vec::new();

    while self.until(&lexer::TokenKind::ParenthesesR)? {
      arguments.push(self.parse_expr()?);

      // REVIEW: What if the comma is omitted?
      if self.is(&lexer::TokenKind::Comma) {
        self.skip()?;
      }
    }

    self.skip_past(&lexer::TokenKind::ParenthesesR)?;

    Ok(ast::CallExpr {
      callee_expr: Box::new(callee_expr),
      arguments,
    })
  }

  /// %name '(' (%expr (,))* ')'
  fn parse_intrinsic_call(&mut self) -> ParserResult<ast::IntrinsicCall> {
    let kind = match self.parse_name()?.as_str() {
      "length_of" => ast::IntrinsicKind::LengthOf,
      // REVISE: This error message won't apply as expected, instead it would compare it with "parentheses" token.
      _ => return Err(self.expected("a valid intrinsic name")),
    };

    self.skip_past(&lexer::TokenKind::ParenthesesL)?;

    let mut arguments = Vec::new();

    while self.until(&lexer::TokenKind::ParenthesesR)? {
      arguments.push(self.parse_expr()?);

      if !self.is(&lexer::TokenKind::ParenthesesR) {
        self.skip_past(&lexer::TokenKind::Comma)?;
      }
    }

    self.skip_past(&lexer::TokenKind::ParenthesesR)?;

    Ok(ast::IntrinsicCall { kind, arguments })
  }

  /// %pattern
  fn parse_reference(&mut self) -> ParserResult<ast::Reference> {
    // REVIEW: Would there be an instance where this method can accept which symbol kind to parse?
    let pattern = self.parse_pattern(name_resolution::SymbolKind::Definition)?;

    Ok(ast::Reference { pattern })
  }

  /// enum %name ':' %indent (%name ',')+ %dedent
  fn parse_enum(&mut self) -> ParserResult<ast::Enum> {
    self.skip_past(&lexer::TokenKind::Enum)?;

    let name = self.parse_name()?;

    self.skip_past(&lexer::TokenKind::Colon)?;

    let mut variants = vec![];

    self.parse_indent()?;

    // TODO: Support for different basic values/types.
    loop {
      variants.push((self.parse_name()?, self.cache.next_id()));
      self.skip_past(&lexer::TokenKind::Comma)?;

      if self.is(&lexer::TokenKind::Dedent) {
        break;
      }
    }

    self.parse_dedent()?;

    let enum_id = self.cache.next_id();

    // TODO: Infer the type from the value, or default to `I32` if values are omitted.
    // ... Issue an error diagnostic if the value is `nullptr`. Finally, verify that all
    // ... the values are of the same inferred type. Allow any constant expression to serve
    // ... as the value.
    let value_type = ast::Type::Basic(ast::BasicType::Int(ast::IntSize::I32));

    let enum_ = ast::Enum {
      name,
      variants,
      id: enum_id.clone(),
      value_type,
    };

    Ok(enum_)
  }

  /// struct %name ':' %indent (%name ':' %type ',')+ %dedent
  fn parse_struct(&mut self) -> ParserResult<ast::Struct> {
    self.skip_past(&lexer::TokenKind::Struct)?;

    let name = self.parse_name()?;

    self.skip_past(&lexer::TokenKind::Colon)?;

    let mut fields = Vec::new();

    self.parse_indent()?;

    loop {
      let field_name = self.parse_name()?;

      self.skip_past(&lexer::TokenKind::Colon)?;

      let field_type = self.parse_type()?;

      self.skip_past(&lexer::TokenKind::Comma)?;
      fields.push((field_name, field_type));

      if self.is(&lexer::TokenKind::Dedent) {
        break;
      }
    }

    self.parse_dedent()?;

    Ok(ast::Struct {
      name,
      fields,
      id: self.cache.next_id(),
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
      fields.push(std::rc::Rc::new(self.parse_expr()?));

      // TODO: Disallow trailing comma.
      if self.is(&lexer::TokenKind::Comma) {
        self.skip()?;
      }
    }

    // TODO: What if `EOF` is reached before the closing brace symbol?
    // Skip the closing brace symbol.
    self.skip()?;

    Ok(ast::StructValue {
      struct_name,
      fields,
      target_id: self.cache.next_id(),
      ty: None,
    })
  }

  /// func '[' (%name (','))* ']' %signature ':' %block
  fn parse_closure(&mut self) -> ParserResult<ast::Closure> {
    self.skip_past(&lexer::TokenKind::Func)?;

    let mut captures = Vec::new();

    if self.is(&lexer::TokenKind::BracketL) {
      self.skip()?;

      while self.until(&lexer::TokenKind::BracketR)? {
        captures.push((self.parse_name()?, None));
      }

      self.skip()?;
    }

    let signature = self.parse_signature(false)?;

    self.skip_past(&lexer::TokenKind::Colon)?;

    let body = std::rc::Rc::new(self.parse_block_expr()?);

    Ok(ast::Closure {
      captures,
      signature,
      body,
      id: self.cache.next_id(),
    })
  }

  fn parse_struct_impl(&mut self) -> ParserResult<ast::StructImpl> {
    self.skip_past(&lexer::TokenKind::Impl)?;

    let mut target_struct_pattern = self.parse_pattern(name_resolution::SymbolKind::Type)?;
    let mut trait_pattern = None;

    if self.is(&lexer::TokenKind::For) {
      trait_pattern = Some(target_struct_pattern);
      self.skip()?;
      target_struct_pattern = self.parse_pattern(name_resolution::SymbolKind::Type)?;
    }

    self.skip_past(&lexer::TokenKind::Colon)?;
    self.parse_indent()?;

    // TODO: Support for trait specifications/implementations.

    let mut member_methods = Vec::new();
    let mut static_methods = Vec::new();

    loop {
      // REVISE: Simplify?
      if self.is(&lexer::TokenKind::Static) {
        self.skip()?;

        let static_method =
          self.parse_function(Some(target_struct_pattern.base_name.clone()), Vec::new())?;

        static_methods.push(std::rc::Rc::new(static_method));
      } else {
        // TODO: Support for attributes.
        member_methods.push(std::rc::Rc::new(self.parse_function(None, Vec::new())?));
      }

      if self.is(&lexer::TokenKind::Dedent) {
        break;
      }
    }

    self.parse_dedent()?;

    Ok(ast::StructImpl {
      // TODO: Support for trait specialization.
      is_default: false,
      target_struct_pattern,
      trait_pattern,
      member_methods,
      static_methods,
    })
  }

  fn parse_trait(&mut self) -> ParserResult<ast::Trait> {
    self.skip_past(&lexer::TokenKind::Trait)?;

    let name = self.parse_name()?;

    self.skip_past(&lexer::TokenKind::Colon)?;

    let mut methods = Vec::new();

    loop {
      self.skip_past(&lexer::TokenKind::Func)?;

      let method_name = self.parse_name()?;
      let signature = self.parse_signature(false)?;

      methods.push((method_name, signature));

      if self.is(&lexer::TokenKind::Dedent) {
        break;
      }
    }

    self.parse_dedent()?;

    Ok(ast::Trait {
      name,
      methods,
      id: self.cache.next_id(),
    })
  }

  fn parse_parentheses_expr(&mut self) -> ParserResult<ast::ParenthesesExpr> {
    self.skip_past(&lexer::TokenKind::ParenthesesL)?;

    let expr = self.parse_expr()?;

    self.skip_past(&lexer::TokenKind::ParenthesesR)?;

    Ok(ast::ParenthesesExpr(std::rc::Rc::new(expr)))
  }

  /// import %pattern ('::' '{' (%name ',')+ '}')
  fn parse_import(&mut self) -> ParserResult<ast::Import> {
    self.skip_past(&lexer::TokenKind::Import)?;

    let package_name = self.parse_name()?;

    self.skip_past(&lexer::TokenKind::DoubleColon)?;

    let module_name = self.parse_name()?;

    Ok(ast::Import {
      package_name,
      module_name,
    })
  }

  /// '<' ()* '>'
  fn parse_generics(&mut self) -> ParserResult<ast::Generics> {
    self.skip_past(&lexer::TokenKind::LessThan)?;

    let mut parameters = Vec::new();

    loop {
      parameters.push(self.parse_name()?);

      if !self.is(&lexer::TokenKind::GreaterThan) {
        self.skip_past(&lexer::TokenKind::Comma)?;
      } else {
        break;
      }
    }

    self.skip_past(&lexer::TokenKind::GreaterThan)?;

    Ok(ast::Generics {
      parameters,
      // TODO: Missing parsing of constrains.
      constraints: None,
    })
  }

  /// %int_literal '..' %int_literal
  fn parse_range(&mut self) -> ParserResult<ast::Range> {
    let start = self.parse_int_literal()?;

    self.skip_past(&lexer::TokenKind::ShortEllipsis)?;

    let end = self.parse_int_literal()?;

    Ok(ast::Range { start, end })
  }
}

#[cfg(test)]
mod tests {
  use super::*;
  use pretty_assertions::{assert_eq, assert_ne};

  fn create_parser<'a>(
    tokens: Vec<lexer::TokenKind>,
    cache: &'a mut symbol_table::SymbolTable,
  ) -> Parser<'a> {
    // TODO: Consider making the position incremental.
    Parser::new(tokens.into_iter().map(|token| (token, 0)).collect(), cache)
  }

  #[test]
  fn proper_initial_values() {
    let mut cache = symbol_table::SymbolTable::new();
    let parser = create_parser(Vec::new(), &mut cache);

    assert_eq!(0, parser.index);
  }

  #[test]
  fn is() {
    let mut cache = symbol_table::SymbolTable::new();
    let mut parser = create_parser(Vec::new(), &mut cache);

    assert!(!parser.is(&lexer::TokenKind::EOF));
    parser.index = 1;
    assert!(!parser.is(&lexer::TokenKind::EOF));
    parser.index = 0;
    parser.tokens.push((lexer::TokenKind::Func, 0));
    assert!(parser.is(&lexer::TokenKind::Func));
  }

  #[test]
  fn is_empty() {
    let mut cache = symbol_table::SymbolTable::new();
    let parser = create_parser(Vec::new(), &mut cache);

    assert_eq!(false, parser.is(&lexer::TokenKind::Func));
  }

  #[test]
  fn skip() {
    let mut cache = symbol_table::SymbolTable::new();

    let mut parser = create_parser(
      vec![lexer::TokenKind::Func, lexer::TokenKind::Func],
      &mut cache,
    );

    assert!(parser.skip().is_ok());
    assert_eq!(1, parser.index);
  }

  #[test]
  fn skip_out_of_bounds() {
    let mut cache = symbol_table::SymbolTable::new();
    let mut parser = create_parser(vec![lexer::TokenKind::Func], &mut cache);

    assert!(parser.skip().is_ok());
    assert_eq!(1, parser.index);
  }

  #[test]
  fn is_eof() {
    let mut cache = symbol_table::SymbolTable::new();
    let mut parser = create_parser(Vec::new(), &mut cache);

    assert!(parser.is_eof());
    parser.tokens.push((lexer::TokenKind::Func, 0));
    assert!(parser.is_eof());
    parser.tokens.push((lexer::TokenKind::Func, 0));
    assert!(!parser.is_eof());
    assert!(parser.skip().is_ok());
    assert!(parser.is_eof());
  }

  #[test]
  fn is_binary_operator() {
    assert!(!Parser::is_binary_operator(&lexer::TokenKind::BraceL));
    assert!(Parser::is_binary_operator(&lexer::TokenKind::Plus));
    assert!(Parser::is_binary_operator(&lexer::TokenKind::Equality));
    assert!(Parser::is_binary_operator(&lexer::TokenKind::And));
    assert!(!Parser::is_binary_operator(&lexer::TokenKind::EOF));

    // TODO: More.
  }

  #[test]
  fn peek_is() {
    let mut cache = symbol_table::SymbolTable::new();
    let mut parser = create_parser(Vec::new(), &mut cache);

    assert!(!parser.peek_is(&lexer::TokenKind::BraceL));
    parser.tokens.push((lexer::TokenKind::BraceL, 0));
    assert!(!parser.peek_is(&lexer::TokenKind::BraceL));
    parser.tokens.push((lexer::TokenKind::BraceL, 0));
    assert!(parser.peek_is(&lexer::TokenKind::BraceL));
  }

  // TODO: Add more tests.
}
