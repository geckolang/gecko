use crate::{ast, context, diagnostic, name_resolution, token};

macro_rules! skip_past {
  ($self:expr, $token:expr) => {
    if !$self.is($token) {
      return Err(diagnostic::Diagnostic {
        message: format!(
          "expected token `{}`, but got `{}`",
          $token, $self.tokens[$self.index].0
        ),
        severity: diagnostic::Severity::Error,
        // TODO: No location provided.
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

/// Determine whether the given token is considered a valid binary
/// operator.
fn is_binary_operator(token: &token::TokenKind) -> bool {
  match token {
    token::TokenKind::SymbolPlus
    | token::TokenKind::SymbolMinus
    | token::TokenKind::SymbolAsterisk
    | token::TokenKind::SymbolSlash => true,
    _ => false,
  }
}

fn get_token_precedence(token: &token::TokenKind) -> usize {
  match token {
    token::TokenKind::SymbolPlus | token::TokenKind::SymbolMinus => 1,
    token::TokenKind::SymbolAsterisk | token::TokenKind::SymbolSlash => 2,
    _ => 0,
  }
}

type ParserResult<T> = Result<T, diagnostic::Diagnostic>;

pub struct Parser<'a> {
  tokens: Vec<token::Token>,
  index: usize,
  context: &'a mut context::Context,
}

impl<'a> Parser<'a> {
  pub fn new(tokens: Vec<token::Token>, context: &'a mut context::Context) -> Self {
    Self {
      tokens,
      index: 0,
      context,
    }
  }

  pub fn from_tokens(
    token_kinds: Vec<token::TokenKind>,
    context: &'a mut context::Context,
  ) -> Self {
    let tokens = token_kinds
      .iter()
      .map(|kind| (kind.to_owned(), 0 as usize))
      .collect::<Vec<_>>();

    Self::new(tokens, context)
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

  fn get(&self) -> &token::TokenKind {
    &self.tokens[self.index].0
  }

  fn get_location(&self) -> Option<diagnostic::Location> {
    let position = self.tokens[self.index].1;

    Some(position..position)
  }

  /// Compare the current token to the given token.
  ///
  /// If `EOF` has been reached, `false` will always be returned.
  fn is(&self, token: token::TokenKind) -> bool {
    if self.index >= self.tokens.len() {
      return false;
    }

    self.get().clone() == token
  }

  /// Attempt to reposition the index to the next token (if any).
  ///
  /// Returns whether the index was repositioned. If false, it indicates
  /// that the end of file was reached (`EOF`).
  fn skip(&mut self) -> bool {
    // FIXME: Address out of bounds problem (if any at this state).
    if self.index + 1 >= self.tokens.len() {
      return false;
    }

    self.index += 1;

    true
  }

  /// Retrieve the upcoming token (if any).
  fn peek(&self) -> Option<&token::TokenKind> {
    match self.tokens.get(self.index + 1) {
      Some(value) => Some(&value.0),
      None => None,
    }
  }

  /// Compare the upcoming token to the given token.
  fn peek_is(&self, token: token::TokenKind) -> bool {
    let next_token = self.peek();

    if next_token.is_none() {
      return false;
    }

    token == *next_token.unwrap()
  }

  /// Whether the parser has reached the end of the input.
  ///
  /// Will return true if the tokens vector provided is empty,
  /// or if the index is at the end of the tokens vector.
  pub fn is_eof(&self) -> bool {
    self.tokens.is_empty() || self.index == self.tokens.len() - 1
  }

  fn parse_scope_qualifier(&mut self) -> ParserResult<ast::ScopeQualifier> {
    let base_name = self.parse_name()?;
    let mut scope = Vec::new();

    while !self.is_eof() && self.is(token::TokenKind::SymbolColon) {
      self.skip();
      scope.push(self.parse_name()?);
    }

    Ok(ast::ScopeQualifier(base_name, scope))
  }

  /// %identifier
  fn parse_name(&mut self) -> ParserResult<String> {
    // TODO: Illegal/unrecognized tokens are also represented under 'Identifier'.

    // TODO: Wrong error message. Create an `expect` method.
    assert!(matches!(self.get(), token::TokenKind::Identifier(_)));

    let name = {
      match self.get() {
        token::TokenKind::Identifier(value) => Some(value.clone()),
        _ => None,
      }
    };

    crate::diagnostic_assert!(name.is_some());
    self.skip();

    Ok(name.unwrap())
  }

  /// '{' (%statement+) '}'
  fn parse_block(&mut self) -> ParserResult<ast::Block> {
    // TODO: Have a symbol table for blocks, and check for re-declarations here?

    skip_past!(self, token::TokenKind::SymbolBraceL);

    let mut statements = vec![];

    while !self.is(token::TokenKind::SymbolBraceR) && !self.is_eof() {
      statements.push(Box::new(match self.get() {
        token::TokenKind::KeywordReturn => ast::Node::ReturnStmt(self.parse_return_stmt()?),
        token::TokenKind::KeywordLet => ast::Node::Definition(self.parse_let_stmt()?),
        token::TokenKind::KeywordIf => ast::Node::IfStmt(self.parse_if_stmt()?),
        token::TokenKind::KeywordWhile => ast::Node::WhileStmt(self.parse_while_stmt()?),
        token::TokenKind::KeywordBreak => ast::Node::BreakStmt(self.parse_break_stmt()?),
        token::TokenKind::KeywordContinue => ast::Node::ContinueStmt(self.parse_continue_stmt()?),
        token::TokenKind::KeywordUnsafe => ast::Node::UnsafeBlock(self.parse_unsafe_block_stmt()?),
        token::TokenKind::Identifier(_) if self.peek_is(token::TokenKind::SymbolEqual) => {
          ast::Node::VariableAssignStmt(self.parse_lvalue_assign_stmt()?)
        }
        token::TokenKind::Identifier(_) if !self.peek_is(token::TokenKind::SymbolParenthesesL) => {
          ast::Node::VariableRef(self.parse_variable_ref()?)
        }
        _ => {
          let result = ast::Node::ExprWrapperStmt(ast::ExprStmt {
            expr: Box::new(self.parse_expr()?),
          });

          skip_past!(self, token::TokenKind::SymbolSemiColon);

          result
        }
      }));
    }

    skip_past!(self, token::TokenKind::SymbolBraceR);

    Ok(ast::Block { statements })
  }

  /// {u8 | u16 | u32 | u64 | i8 | i16 | i32 | i64}
  fn parse_int_type(&mut self) -> ParserResult<ast::Type> {
    // TODO: Accessing index unsafely.
    let current_token = self.get();

    let size = match current_token {
      token::TokenKind::TypeInt16 => ast::IntSize::I16,
      token::TokenKind::TypeInt32 => ast::IntSize::I32,
      token::TokenKind::TypeInt64 => ast::IntSize::I64,
      // TODO: Add unsigned type tokens.
      _ => {
        return Err(diagnostic::Diagnostic {
          message: format!(
            "unexpected token `{}`, expected integer type",
            current_token
          ),
          severity: diagnostic::Severity::Error,
          location: self.get_location(),
        })
      }
    };

    self.skip();

    Ok(ast::Type::Primitive(ast::PrimitiveType::Int(size)))
  }

  // TODO: Merge with the `parse_type` function (too small).
  /// bool
  fn parse_bool_type(&mut self) -> ParserResult<ast::Type> {
    skip_past!(self, token::TokenKind::TypeBool);

    Ok(ast::Type::Primitive(ast::PrimitiveType::Bool))
  }

  /// '[' %type, 0-9+ ']'
  fn parse_array_type(&mut self) -> ParserResult<ast::Type> {
    skip_past!(self, token::TokenKind::SymbolBracketL);

    let element_type = self.parse_type()?;

    skip_past!(self, token::TokenKind::SymbolComma);

    // TODO: Cloning token.
    let current_token = self.get().clone();

    let size = match current_token {
      token::TokenKind::LiteralInt(value) => value as u32,
      _ => {
        return Err(diagnostic::Diagnostic {
          message: format!("unexpected token `{}`, expected array size", current_token),
          severity: diagnostic::Severity::Error,
          location: self.get_location(),
        });
      }
    };

    self.skip();
    skip_past!(self, token::TokenKind::SymbolBracketR);

    Ok(ast::Type::Array(Box::new(element_type), size.clone()))
  }

  /// %type
  fn parse_type(&mut self) -> ParserResult<ast::Type> {
    // TODO: Check if the index is valid?
    // TODO: Support for more types.
    match self.get() {
      // TODO: Other types as well.
      token::TokenKind::TypeInt16 | token::TokenKind::TypeInt32 | token::TokenKind::TypeInt64 => {
        self.parse_int_type()
      }
      token::TokenKind::TypeBool => self.parse_bool_type(),
      token::TokenKind::TypeString => {
        self.skip();

        Ok(ast::Type::Primitive(ast::PrimitiveType::String))
      }
      token::TokenKind::SymbolBracketL => self.parse_array_type(),
      token::TokenKind::SymbolAsterisk => {
        self.skip();

        Ok(ast::Type::Pointer(Box::new(self.parse_type()?)))
      }
      token::TokenKind::Identifier(_) => self.parse_user_defined_type(),
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

  fn parse_user_defined_type(&mut self) -> ParserResult<ast::Type> {
    let name = self.parse_name()?;

    Ok(ast::Type::UserDefined(ast::UserDefinedType {
      name,
      target_key: None,
    }))
  }

  /// %name ':' %type_group
  fn parse_parameter(&mut self, index: u32) -> ParserResult<ast::Parameter> {
    let name = self.parse_name()?;

    skip_past!(self, token::TokenKind::SymbolColon);

    let type_group = self.parse_type()?;

    Ok((name, type_group, index))
  }

  /// '(' {%parameter* (,)} (+) ')' '~' %type_group
  fn parse_prototype(&mut self) -> ParserResult<ast::Prototype> {
    skip_past!(self, token::TokenKind::SymbolParenthesesL);

    // TODO: Parameters must be a `Declaration` node, in order for their references to be resolved.
    let mut parameters = vec![];
    let mut is_variadic = false;
    let mut parameter_index_counter = 0;

    // TODO: Analyze, and remove possibility of lonely comma.
    while !self.is(token::TokenKind::SymbolParenthesesR) && !self.is_eof() {
      if self.is(token::TokenKind::SymbolPlus) {
        is_variadic = true;
        self.skip();

        break;
      }

      parameters.push(self.parse_parameter(parameter_index_counter)?);
      parameter_index_counter += 1;

      if !self.is(token::TokenKind::SymbolComma) {
        break;
      }

      self.skip();
    }

    skip_past!(self, token::TokenKind::SymbolParenthesesR);

    let mut return_type = None;

    if self.is(token::TokenKind::SymbolTilde) {
      self.skip();
      return_type = Some(self.parse_type()?);
    }

    Ok(ast::Prototype {
      parameters,
      return_type,
      is_variadic,
    })
  }

  /// fn %prototype %block
  fn parse_function(&mut self) -> ParserResult<ast::Definition> {
    skip_past!(self, token::TokenKind::KeywordFn);

    let name = self.parse_name()?;
    let prototype = self.parse_prototype()?;
    let body = self.parse_block()?;

    let function = ast::Function {
      name: name.clone(),
      prototype,
      body,
    };

    Ok(ast::Definition {
      name,
      symbol_kind: name_resolution::SymbolKind::FunctionOrExtern,
      node: std::rc::Rc::new(std::cell::RefCell::new(ast::Node::Function(function))),
      definition_key: self.context.create_definition_key(),
    })
  }

  /// extern fn %prototype ';'
  fn parse_extern(&mut self) -> ParserResult<ast::Definition> {
    // TODO: Support for visibility.

    skip_past!(self, token::TokenKind::KeywordExtern);
    skip_past!(self, token::TokenKind::KeywordFn);

    let name = self.parse_name()?;
    let prototype = self.parse_prototype()?;

    skip_past!(self, token::TokenKind::SymbolSemiColon);

    let extern_node = ast::Extern {
      name: name.clone(),
      prototype,
    };

    Ok(ast::Definition {
      name,
      symbol_kind: name_resolution::SymbolKind::FunctionOrExtern,
      node: std::rc::Rc::new(std::cell::RefCell::new(ast::Node::Extern(extern_node))),
      definition_key: self.context.create_definition_key(),
    })
  }

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

    let token = self.get();

    Ok(match token {
      // TODO: Why not create the definition here? That way we allow testability (functions actually return what they parse).
      token::TokenKind::KeywordFn => ast::Node::Definition(self.parse_function()?),
      token::TokenKind::KeywordExtern => ast::Node::Definition(self.parse_extern()?),
      token::TokenKind::KeywordEnum => ast::Node::Definition(self.parse_enum()?),
      token::TokenKind::KeywordStruct => ast::Node::Definition(self.parse_struct_type()?),
      _ => {
        return Err(diagnostic::Diagnostic {
          message: format!("unexpected token `{}`, expected top-level construct", token),
          severity: diagnostic::Severity::Error,
          location: self.get_location(),
        })
      }
    })
  }

  /// return (%expr)
  fn parse_return_stmt(&mut self) -> ParserResult<ast::ReturnStmt> {
    skip_past!(self, token::TokenKind::KeywordReturn);

    let mut value = None;

    // TODO: Does this cover all cases?
    if !self.is(token::TokenKind::SymbolSemiColon) {
      value = Some(Box::new(self.parse_expr()?));
    }

    skip_past!(self, token::TokenKind::SymbolSemiColon);

    Ok(ast::ReturnStmt { value })
  }

  /// let %name (':' %type_group) '=' %expr ';'
  fn parse_let_stmt(&mut self) -> ParserResult<ast::Definition> {
    skip_past!(self, token::TokenKind::KeywordLet);

    let mut is_mutable = false;

    if self.is(token::TokenKind::KeywordMut) {
      self.skip();
      is_mutable = true;
    }

    let name = self.parse_name()?;
    let mut ty = None;

    if self.is(token::TokenKind::SymbolColon) {
      self.skip();
      ty = Some(self.parse_type()?);
    }

    skip_past!(self, token::TokenKind::SymbolEqual);

    let value = self.parse_expr()?;

    skip_past!(self, token::TokenKind::SymbolSemiColon);

    // Infer the type based on the value.
    if ty.is_none() {
      // FIXME: Implement type inference.
      todo!();
    }

    // TODO: Is mutable.
    let let_stmt = ast::LetStmt {
      name: name.clone(),
      ty: ty.unwrap(),
      value: Box::new(value),
      is_mutable,
    };

    Ok(ast::Definition {
      name,
      symbol_kind: name_resolution::SymbolKind::VariableOrParameter,
      node: std::rc::Rc::new(std::cell::RefCell::new(ast::Node::LetStmt(let_stmt))),
      definition_key: self.context.create_definition_key(),
    })
  }

  /// if %expr %block (else %block)
  fn parse_if_stmt(&mut self) -> ParserResult<ast::IfStmt> {
    skip_past!(self, token::TokenKind::KeywordIf);

    let condition = self.parse_expr()?;
    let then_block = self.parse_block()?;
    let mut else_block = None;

    if self.is(token::TokenKind::KeywordElse) {
      self.skip();
      else_block = Some(self.parse_block()?);
    }

    Ok(ast::IfStmt {
      condition: Box::new(condition),
      then_block,
      else_block,
    })
  }

  /// while %expr %block
  fn parse_while_stmt(&mut self) -> ParserResult<ast::WhileStmt> {
    skip_past!(self, token::TokenKind::KeywordWhile);

    let condition = self.parse_expr()?;
    let body = self.parse_block()?;

    Ok(ast::WhileStmt {
      condition: Box::new(condition),
      body,
    })
  }

  /// break ';'
  fn parse_break_stmt(&mut self) -> ParserResult<ast::BreakStmt> {
    skip_past!(self, token::TokenKind::KeywordBreak);
    skip_past!(self, token::TokenKind::SymbolSemiColon);

    Ok(ast::BreakStmt {})
  }

  /// continue ';'
  fn parse_continue_stmt(&mut self) -> ParserResult<ast::ContinueStmt> {
    skip_past!(self, token::TokenKind::KeywordContinue);
    skip_past!(self, token::TokenKind::SymbolSemiColon);

    Ok(ast::ContinueStmt)
  }

  // unsafe %block
  fn parse_unsafe_block_stmt(&mut self) -> ParserResult<ast::UnsafeBlockStmt> {
    skip_past!(self, token::TokenKind::KeywordUnsafe);

    Ok(ast::UnsafeBlockStmt(self.parse_block()?))
  }

  /// {true | false}
  fn parse_bool_literal(&mut self) -> ParserResult<ast::Literal> {
    // TODO: Accessing tokens like this is unsafe/unchecked.
    Ok(match self.get().clone() {
      token::TokenKind::LiteralBool(value) => {
        self.skip();

        ast::Literal::Bool(value)
      }
      // TODO: Better error.
      _ => {
        return Err(diagnostic::Diagnostic {
          message: "unexpected token, expected boolean literal".to_string(),
          severity: diagnostic::Severity::Error,
          location: self.get_location(),
        })
      }
    })
  }

  /// 0-9+
  fn parse_int_literal(&mut self) -> ParserResult<ast::Literal> {
    // TODO: Accessing tokens like this is unsafe/unchecked.
    // TODO: Possibly cloning value.
    Ok(match self.get().clone() {
      token::TokenKind::LiteralInt(value) => {
        self.skip();

        let mut size = minimum_int_size_of(&value);

        // TODO: Deal with unsigned integers here?
        // Default size to 32 bit-width.
        if size < ast::IntSize::I32 {
          size = ast::IntSize::I32;
        }

        ast::Literal::Int(value.clone(), size)
      }
      _ => {
        return Err(diagnostic::Diagnostic {
          message: "expected integer literal but got end of file".to_string(),
          severity: diagnostic::Severity::Error,
          location: self.get_location(),
        })
      }
    })
  }

  /// '"' [^"]* '"'
  fn parse_string_literal(&mut self) -> ParserResult<ast::Literal> {
    // TODO: Accessing tokens like this is unsafe/unchecked.
    let result = match self.get() {
      token::TokenKind::LiteralString(value) => ast::Literal::String(value.clone()),
      _ => {
        return Err(diagnostic::Diagnostic {
          message: "expected string literal but got end of file".to_string(),
          severity: diagnostic::Severity::Error,
          location: self.get_location(),
        })
      }
    };

    self.skip();

    Ok(result)
  }

  /// '[' (%expr (','))* ']'
  fn parse_array_value(&mut self) -> ParserResult<ast::ArrayValue> {
    let mut elements = Vec::new();

    skip_past!(self, token::TokenKind::SymbolBracketL);

    loop {
      if self.is(token::TokenKind::SymbolBracketR) {
        break;
      }

      elements.push(self.parse_expr()?);

      // TODO: Make sure there isn't space for lonely commas.
      if self.is(token::TokenKind::SymbolComma) {
        self.skip();
      }
    }

    // Skip the closing bracket.
    self.skip();

    let mut explicit_type = None;

    // TODO: In the future, the type of an empty array will be inferred.
    if elements.is_empty() {
      explicit_type = Some(self.parse_type()?);
    }

    Ok(ast::ArrayValue {
      elements,
      explicit_type,
    })
  }

  /// %name '[' %expr ']'
  fn parse_array_indexing(&mut self) -> ParserResult<ast::ArrayIndexing> {
    let name = self.parse_name()?;

    skip_past!(self, token::TokenKind::SymbolBracketL);

    let index = Box::new(self.parse_expr()?);

    skip_past!(self, token::TokenKind::SymbolBracketR);

    Ok(ast::ArrayIndexing {
      name,
      index,
      target_key: None,
    })
  }

  fn parse_literal(&mut self) -> ParserResult<ast::Literal> {
    let current_token = self.get();

    Ok(match current_token {
      token::TokenKind::LiteralBool(_) => self.parse_bool_literal()?,
      token::TokenKind::LiteralInt(_) => self.parse_int_literal()?,
      token::TokenKind::LiteralString(_) => self.parse_string_literal()?,
      _ => {
        return Err(diagnostic::Diagnostic {
          // TODO: Show the actual token.
          message: format!("unexpected token `{}`, expected literal", current_token),
          severity: diagnostic::Severity::Error,
          location: self.get_location(),
        });
      }
    })
  }

  fn parse_primary_expr(&mut self) -> ParserResult<ast::Node> {
    // TODO: Might need to revisit. Might need to make room for other cases in the future (binary/unary operators, etc).
    Ok(match self.get() {
      token::TokenKind::Identifier(_) => {
        if self.peek_is(token::TokenKind::SymbolParenthesesL) {
          ast::Node::FunctionCall(self.parse_function_call()?)
        } else if self.peek_is(token::TokenKind::SymbolBracketL) {
          ast::Node::ArrayIndexing(self.parse_array_indexing()?)
        } else {
          ast::Node::VariableRef(self.parse_variable_ref()?)
        }
      }
      token::TokenKind::SymbolMinus
      | token::TokenKind::SymbolBang
      | token::TokenKind::SymbolAmpersand
      | token::TokenKind::SymbolAsterisk => ast::Node::UnaryExpr(self.parse_unary_expr()?),
      token::TokenKind::SymbolBracketL => ast::Node::ArrayValue(self.parse_array_value()?),
      token::TokenKind::KeywordNew => ast::Node::StructValue(self.parse_struct_value()?),
      // Default to a literal if nothing else matched.
      _ => ast::Node::Literal(self.parse_literal()?),
    })
  }

  /// {'+' | '-' | '*' | '/'}
  fn parse_operator(&mut self) -> ParserResult<ast::OperatorKind> {
    // TODO: Unsafe access. Also, cloning token.
    let current_token = self.get().clone();

    let operator = match current_token {
      token::TokenKind::SymbolBang => ast::OperatorKind::Not,
      token::TokenKind::SymbolPlus => ast::OperatorKind::Add,
      token::TokenKind::SymbolMinus => ast::OperatorKind::SubtractOrNegate,
      token::TokenKind::SymbolAsterisk => ast::OperatorKind::MultiplyOrDereference,
      token::TokenKind::SymbolSlash => ast::OperatorKind::Divide,
      token::TokenKind::SymbolLessThan => ast::OperatorKind::LessThan,
      token::TokenKind::SymbolGreaterThan => ast::OperatorKind::GreaterThan,
      token::TokenKind::SymbolAmpersand => ast::OperatorKind::AddressOf,
      // TODO: Implement logic for GTE & LTE.
      _ => {
        return Err(diagnostic::Diagnostic {
          message: format!("unexpected token `{}`, expected operator", current_token),
          severity: diagnostic::Severity::Error,
          location: self.get_location(),
        })
      }
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

    while is_binary_operator(token) && (precedence > min_precedence) {
      let operator = self.parse_operator()?;
      let mut right = self.parse_primary_expr()?;

      token = self.get();

      while is_binary_operator(&token) && get_token_precedence(&token) > precedence {
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
    // TODO: Need support for unary expressions (such as `!`, '-', etc.).

    let left = self.parse_primary_expr()?;

    // TODO: Should the precedence be zero here?
    Ok(self.parse_binary_expr(left, 0)?)
  }

  /// %name '(' (%expr (,))* ')'
  fn parse_function_call(&mut self) -> ParserResult<ast::FunctionCall> {
    let callee_id = self.parse_scope_qualifier()?;

    skip_past!(self, token::TokenKind::SymbolParenthesesL);

    let mut arguments = vec![];

    while !self.is_eof() && !self.is(token::TokenKind::SymbolParenthesesR) {
      arguments.push(self.parse_expr()?);

      if self.is(token::TokenKind::SymbolComma) {
        self.skip();
      }
    }

    skip_past!(self, token::TokenKind::SymbolParenthesesR);

    Ok(ast::FunctionCall {
      callee_id,
      target_key: None,
      arguments,
    })
  }

  /// %name
  fn parse_variable_ref(&mut self) -> ParserResult<ast::VariableRef> {
    let name = self.parse_name()?;

    Ok(ast::VariableRef {
      name,
      target_key: None,
    })
  }

  /// %name '=' %expr ';'
  fn parse_lvalue_assign_stmt(&mut self) -> ParserResult<ast::AssignStmt> {
    let lvalue_expr = Box::new(self.parse_expr()?);

    skip_past!(self, token::TokenKind::SymbolEqual);

    let value = Box::new(self.parse_expr()?);

    skip_past!(self, token::TokenKind::SymbolSemiColon);

    Ok(ast::AssignStmt {
      assignee_expr: lvalue_expr,
      value,
    })
  }

  /// enum %name '{' (%name (','))* '}'
  fn parse_enum(&mut self) -> ParserResult<ast::Definition> {
    skip_past!(self, token::TokenKind::KeywordEnum);

    let name = self.parse_name()?;

    skip_past!(self, token::TokenKind::SymbolBraceL);

    let mut variants = vec![];

    while !self.is_eof() && !self.is(token::TokenKind::SymbolBraceR) {
      variants.push(self.parse_name()?);

      // TODO: Iron out case for lonely comma.
      if self.is(token::TokenKind::SymbolComma) {
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
      node: std::rc::Rc::new(std::cell::RefCell::new(ast::Node::Enum(enum_))),
      definition_key: self.context.create_definition_key(),
    })
  }

  /// struct %name '{' (%name ':' %type ';')* '}'
  fn parse_struct_type(&mut self) -> ParserResult<ast::Definition> {
    skip_past!(self, token::TokenKind::KeywordStruct);

    let name = self.parse_name()?;

    skip_past!(self, token::TokenKind::SymbolBraceL);

    let mut fields = std::collections::HashMap::new();

    while !self.is_eof() && !self.is(token::TokenKind::SymbolBraceR) {
      let field_name = self.parse_name()?;

      skip_past!(self, token::TokenKind::SymbolColon);

      let field_type = self.parse_type()?;

      skip_past!(self, token::TokenKind::SymbolSemiColon);
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
      node: std::rc::Rc::new(std::cell::RefCell::new(ast::Node::StructType(struct_type))),
      definition_key: self.context.create_definition_key(),
    })
  }

  fn parse_struct_value(&mut self) -> ParserResult<ast::StructValue> {
    skip_past!(self, token::TokenKind::KeywordNew);

    // TODO: Shouldn't it be `ScopeQualifier`?
    let name = self.parse_name()?;

    skip_past!(self, token::TokenKind::SymbolBraceL);

    let mut fields = Vec::new();

    while !self.is_eof() && !self.is(token::TokenKind::SymbolBraceR) {
      let field = self.parse_expr()?;

      fields.push(field);

      // TODO: Disallow trailing comma.
      if self.is(token::TokenKind::SymbolComma) {
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
    let mut context = context::Context::new();
    let parser = Parser::new(vec![], &mut context);

    assert_eq!(0, parser.index);
  }

  #[test]
  fn is() {
    let mut context = context::Context::new();
    let parser = Parser::from_tokens(vec![token::TokenKind::KeywordFn], &mut context);

    assert_eq!(true, parser.is(token::TokenKind::KeywordFn));
  }

  #[test]
  fn is_empty() {
    let mut context = context::Context::new();
    let parser = Parser::new(vec![], &mut context);

    assert_eq!(false, parser.is(token::TokenKind::KeywordFn));
  }

  #[test]
  fn skip() {
    let mut context = context::Context::new();

    let mut parser = Parser::from_tokens(
      vec![token::TokenKind::KeywordFn, token::TokenKind::KeywordFn],
      &mut context,
    );

    parser.skip();
    assert_eq!(1, parser.index);
  }

  #[test]
  fn skip_out_of_bounds() {
    let mut context = context::Context::new();
    let mut parser = Parser::from_tokens(vec![token::TokenKind::KeywordFn], &mut context);

    parser.skip();
    assert_eq!(0, parser.index);
  }

  #[test]
  fn is_eof() {
    let mut context = context::Context::new();
    let mut parser = Parser::new(vec![], &mut context);

    assert_eq!(true, parser.is_eof());
    parser.tokens.push((token::TokenKind::KeywordFn, 0));
    assert_eq!(true, parser.is_eof());
    parser.tokens.push((token::TokenKind::KeywordFn, 0));
    assert_eq!(false, parser.is_eof());
    parser.skip();
    assert_eq!(true, parser.is_eof());
  }

  // TODO: Add more tests.
}
