use crate::{ast, context, diagnostic, name_resolution, token};

macro_rules! skip_past {
  ($self:expr, $token:expr) => {
    if !$self.is($token) {
      return Err(diagnostic::Diagnostic {
        message: format!(
          "expected token `{}`, but got `{}`",
          $token, $self.tokens[$self.index]
        ),
        severity: diagnostic::Severity::Error,
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

fn is_binary_operator(token: &token::Token) -> bool {
  match token {
    token::Token::SymbolPlus
    | token::Token::SymbolMinus
    | token::Token::SymbolAsterisk
    | token::Token::SymbolSlash => true,
    _ => false,
  }
}

fn get_token_precedence(token: &token::Token) -> usize {
  match token {
    token::Token::SymbolPlus | token::Token::SymbolMinus => 1,
    token::Token::SymbolAsterisk | token::Token::SymbolSlash => 2,
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

  /// Parse all top-level definitions.
  pub fn parse_all(&mut self) -> ParserResult<Vec<ast::Node>> {
    let mut result = Vec::new();

    while !self.is_eof() {
      result.push(self.parse_top_level_node()?);

      // FIXME: Nothing being done to the parsed top-level node.
    }

    Ok(result)
  }

  fn is(&self, token: token::Token) -> bool {
    if self.index >= self.tokens.len() {
      return false;
    }

    self.tokens[self.index] == token
  }

  fn skip(&mut self) -> bool {
    // FIXME: Address out of bounds problem (if any at this state).
    if self.index + 1 >= self.tokens.len() {
      return false;
    }

    self.index += 1;

    true
  }

  fn peek(&self) -> Option<&token::Token> {
    match self.tokens.get(self.index + 1) {
      Some(value) => Some(value),
      None => None,
    }
  }

  fn peek_is(&self, token: token::Token) -> bool {
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

  /// %name
  fn parse_name(&mut self) -> ParserResult<String> {
    // TODO: Illegal/unrecognized tokens are also represented under 'Identifier'.

    // TODO: Wrong error message.
    crate::diagnostic_assert!(match &self.tokens[self.index] {
      token::Token::Identifier(_) => true,
      _ => false,
    });

    let name = {
      match &self.tokens[self.index] {
        token::Token::Identifier(value) => Some(value.clone()),
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

    skip_past!(self, token::Token::SymbolBraceL);

    let mut statements = vec![];

    while !self.is(token::Token::SymbolBraceR) && !self.is_eof() {
      statements.push(Box::new(match self.tokens[self.index] {
        token::Token::KeywordReturn => ast::Node::ReturnStmt(self.parse_return_stmt()?),
        token::Token::KeywordLet => ast::Node::Definition(self.parse_let_stmt()?),
        token::Token::KeywordIf => ast::Node::IfStmt(self.parse_if_stmt()?),
        token::Token::KeywordWhile => ast::Node::WhileStmt(self.parse_while_stmt()?),
        token::Token::KeywordBreak => ast::Node::BreakStmt(self.parse_break_stmt()?),
        token::Token::KeywordContinue => ast::Node::ContinueStmt(self.parse_continue_stmt()?),
        token::Token::KeywordUnsafe => ast::Node::UnsafeBlock(self.parse_unsafe_block_stmt()?),
        token::Token::Identifier(_) if self.peek_is(token::Token::SymbolBracketL) => {
          ast::Node::ArrayAssignStmt(self.parse_array_assign_stmt()?)
        }
        token::Token::Identifier(_) if !self.peek_is(token::Token::SymbolParenthesesL) => {
          ast::Node::VariableRef(self.parse_variable_ref()?)
        }
        _ => {
          let result = ast::Node::ExprWrapperStmt(ast::ExprWrapperStmt {
            expr: Box::new(self.parse_expr()?),
          });

          skip_past!(self, token::Token::SymbolSemiColon);

          result
        }
      }));
    }

    skip_past!(self, token::Token::SymbolBraceR);

    Ok(ast::Block { statements })
  }

  /// (unsigned) {i8 | i16 | i32 | i64}
  fn parse_int_type(&mut self) -> ParserResult<ast::Type> {
    // TODO: Accessing index unsafely.
    let current_token = &self.tokens[self.index];

    let size = match current_token {
      token::Token::TypeInt16 => ast::IntSize::I16,
      token::Token::TypeInt32 => ast::IntSize::I32,
      token::Token::TypeInt64 => ast::IntSize::I64,
      // TODO: Add unsigned type tokens.
      _ => {
        return Err(diagnostic::Diagnostic {
          message: format!(
            "unexpected token `{}`, expected integer type",
            current_token
          ),
          severity: diagnostic::Severity::Error,
        })
      }
    };

    self.skip();

    Ok(ast::Type::PrimitiveType(ast::PrimitiveType::Int(size)))
  }

  /// bool
  fn parse_bool_type(&mut self) -> ParserResult<ast::Type> {
    skip_past!(self, token::Token::TypeBool);

    Ok(ast::Type::PrimitiveType(ast::PrimitiveType::Bool))
  }

  fn parse_array_type(&mut self) -> ParserResult<ast::Type> {
    skip_past!(self, token::Token::SymbolBracketL);

    let element_type = self.parse_type()?;

    skip_past!(self, token::Token::SymbolComma);

    // TODO: Cloning token.
    let current_token = self.tokens[self.index].clone();

    let size = match current_token {
      token::Token::LiteralInt(value) => value as u32,
      _ => {
        return Err(diagnostic::Diagnostic {
          message: format!("unexpected token `{}`, expected array size", current_token),
          severity: diagnostic::Severity::Error,
        });
      }
    };

    self.skip();
    skip_past!(self, token::Token::SymbolBracketR);

    Ok(ast::Type::Array(Box::new(element_type), size.clone()))
  }

  /// %type
  fn parse_type(&mut self) -> ParserResult<ast::Type> {
    // TODO: Check if the index is valid?
    // TODO: Support for more types.
    match self.tokens[self.index] {
      // TODO: Other types as well.
      token::Token::TypeInt16 | token::Token::TypeInt32 | token::Token::TypeInt64 => {
        self.parse_int_type()
      }
      token::Token::TypeBool => self.parse_bool_type(),
      token::Token::TypeString => {
        self.skip();

        Ok(ast::Type::PrimitiveType(ast::PrimitiveType::String))
      }
      token::Token::SymbolBracketL => self.parse_array_type(),
      _ => {
        return Err(diagnostic::Diagnostic {
          message: format!(
            "unexpected token `{:?}`, expected type",
            // TODO: Check if the index is valid?
            self.tokens[self.index]
          ),
          severity: diagnostic::Severity::Error,
        });
      }
    }
  }

  /// %name ':' %type_group
  fn parse_parameter(&mut self, index: u32) -> ParserResult<ast::Definition> {
    let name = self.parse_name()?;

    skip_past!(self, token::Token::SymbolColon);

    let type_group = self.parse_type()?;

    Ok(ast::Definition {
      name: name.clone(),
      symbol_kind: name_resolution::SymbolKind::VariableOrParameter,
      node: std::rc::Rc::new(std::cell::RefCell::new(ast::Node::Parameter((
        name, type_group, index,
      )))),
      key: self.context.create_definition_key(),
    })
  }

  /// '(' {%parameter* (,)} (+) ')' '~' %type_group
  fn parse_prototype(&mut self) -> ParserResult<ast::Type> {
    skip_past!(self, token::Token::SymbolParenthesesL);

    // TODO: Parameters must be a `Declaration` node, in order for their references to be resolved.
    let mut parameters = vec![];
    let mut is_variadic = false;
    let mut parameter_index_counter = 0;

    // TODO: Analyze, and remove possibility of lonely comma.
    while !self.is(token::Token::SymbolParenthesesR) && !self.is_eof() {
      if self.is(token::Token::SymbolPlus) {
        is_variadic = true;
        self.skip();

        break;
      }

      parameters.push(self.parse_parameter(parameter_index_counter)?);
      parameter_index_counter += 1;

      if !self.is(token::Token::SymbolComma) {
        break;
      }

      self.skip();
    }

    skip_past!(self, token::Token::SymbolParenthesesR);

    let mut return_type = None;

    if self.is(token::Token::SymbolTilde) {
      self.skip();
      return_type = Some(Box::new(self.parse_type()?));
    }

    Ok(ast::Type::Prototype(parameters, return_type, is_variadic))
  }

  /// fn %prototype %block
  fn parse_function(&mut self) -> ParserResult<ast::Definition> {
    skip_past!(self, token::Token::KeywordFn);

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
      key: self.context.create_definition_key(),
    })
  }

  /// extern fn %prototype ';'
  fn parse_extern(&mut self) -> ParserResult<ast::Definition> {
    // TODO: Support for visibility.

    skip_past!(self, token::Token::KeywordExtern);
    skip_past!(self, token::Token::KeywordFn);

    let name = self.parse_name()?;
    let prototype = self.parse_prototype()?;

    skip_past!(self, token::Token::SymbolSemiColon);

    let extern_node = ast::Extern {
      name: name.clone(),
      prototype,
    };

    Ok(ast::Definition {
      name,
      symbol_kind: name_resolution::SymbolKind::FunctionOrExtern,
      node: std::rc::Rc::new(std::cell::RefCell::new(ast::Node::Extern(extern_node))),
      key: self.context.create_definition_key(),
    })
  }

  fn parse_top_level_node(&mut self) -> ParserResult<ast::Node> {
    let token = self.tokens.get(self.index);

    if token.is_none() {
      return Err(diagnostic::Diagnostic {
        message: "expected top-level construct but got end of file".to_string(),
        severity: diagnostic::Severity::Error,
      });
    }

    Ok(match token.unwrap() {
      token::Token::KeywordFn => ast::Node::Definition(self.parse_function()?),
      token::Token::KeywordExtern => ast::Node::Definition(self.parse_extern()?),
      _ => {
        return Err(diagnostic::Diagnostic {
          message: format!(
            "unexpected token `{}`, expected top-level construct",
            token.unwrap()
          ),
          severity: diagnostic::Severity::Error,
        })
      }
    })
  }

  /// return (%expr)
  fn parse_return_stmt(&mut self) -> ParserResult<ast::ReturnStmt> {
    skip_past!(self, token::Token::KeywordReturn);

    let mut value = None;

    // TODO: Does this cover all cases?
    if !self.is(token::Token::SymbolSemiColon) {
      value = Some(Box::new(self.parse_expr()?));
    }

    skip_past!(self, token::Token::SymbolSemiColon);

    Ok(ast::ReturnStmt { value })
  }

  /// let %name (':' %type_group) '=' %expr ';'
  fn parse_let_stmt(&mut self) -> ParserResult<ast::Definition> {
    skip_past!(self, token::Token::KeywordLet);

    let name = self.parse_name()?;
    let mut ty = None;

    if self.is(token::Token::SymbolColon) {
      self.skip();
      ty = Some(self.parse_type()?);
    }

    skip_past!(self, token::Token::SymbolEqual);

    let value = self.parse_expr()?;

    skip_past!(self, token::Token::SymbolSemiColon);

    // Infer the type based on the value.
    if ty.is_none() {
      // FIXME: Implement type inference.
      todo!();
    }

    let let_stmt = ast::LetStmt {
      name: name.clone(),
      ty: ty.unwrap(),
      value: Box::new(value),
    };

    Ok(ast::Definition {
      name,
      symbol_kind: name_resolution::SymbolKind::VariableOrParameter,
      key: self.context.create_definition_key(),
      node: std::rc::Rc::new(std::cell::RefCell::new(ast::Node::LetStmt(let_stmt))),
    })
  }

  /// if %expr %block (else %block)
  fn parse_if_stmt(&mut self) -> ParserResult<ast::IfStmt> {
    skip_past!(self, token::Token::KeywordIf);

    let condition = self.parse_expr()?;
    let then_block = self.parse_block()?;
    let mut else_block = None;

    if self.is(token::Token::KeywordElse) {
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
    skip_past!(self, token::Token::KeywordWhile);

    let condition = self.parse_expr()?;
    let body = self.parse_block()?;

    Ok(ast::WhileStmt {
      condition: Box::new(condition),
      body,
    })
  }

  /// break ';'
  fn parse_break_stmt(&mut self) -> ParserResult<ast::BreakStmt> {
    skip_past!(self, token::Token::KeywordBreak);
    skip_past!(self, token::Token::SymbolSemiColon);

    Ok(ast::BreakStmt {})
  }

  /// continue ';'
  fn parse_continue_stmt(&mut self) -> ParserResult<ast::ContinueStmt> {
    skip_past!(self, token::Token::KeywordContinue);
    skip_past!(self, token::Token::SymbolSemiColon);

    Ok(ast::ContinueStmt)
  }

  // unsafe %block
  fn parse_unsafe_block_stmt(&mut self) -> ParserResult<ast::UnsafeBlock> {
    skip_past!(self, token::Token::KeywordUnsafe);

    Ok(ast::UnsafeBlock(self.parse_block()?))
  }

  fn parse_array_assign_stmt(&mut self) -> ParserResult<ast::ArrayAssignStmt> {
    let name = self.parse_name()?;

    skip_past!(self, token::Token::SymbolBracketL);

    let index = Box::new(self.parse_expr()?);

    skip_past!(self, token::Token::SymbolBracketR);
    skip_past!(self, token::Token::SymbolEqual);

    let value = Box::new(self.parse_expr()?);

    skip_past!(self, token::Token::SymbolSemiColon);

    Ok(ast::ArrayAssignStmt {
      name,
      index,
      value,
      definition_key: None,
    })
  }

  /// {true | false}
  fn parse_bool_literal(&mut self) -> ParserResult<ast::Literal> {
    // TODO: Accessing tokens like this is unsafe/unchecked.
    Ok(match self.tokens[self.index] {
      token::Token::LiteralBool(value) => {
        self.skip();

        ast::Literal::Bool(value)
      }
      // TODO: Better error.
      _ => {
        return Err(diagnostic::Diagnostic {
          message: "unexpected token, expected boolean literal".to_string(),
          severity: diagnostic::Severity::Error,
        })
      }
    })
  }

  fn parse_int_literal(&mut self) -> ParserResult<ast::Literal> {
    // TODO: Accessing tokens like this is unsafe/unchecked.
    // TODO: Possibly cloning value.
    Ok(match self.tokens[self.index] {
      token::Token::LiteralInt(value) => {
        self.skip();

        let mut size = minimum_int_size_of(&value);

        // TODO: Deal with unsigned integers here?
        // Default size to 32 bit-width.
        if size < ast::IntSize::I32 {
          size = ast::IntSize::I32;
        }

        ast::Literal::Int(value, size)
      }
      _ => {
        return Err(diagnostic::Diagnostic {
          message: "expected integer literal but got end of file".to_string(),
          severity: diagnostic::Severity::Error,
        })
      }
    })
  }

  fn parse_string_literal(&mut self) -> ParserResult<ast::Literal> {
    // TODO: Accessing tokens like this is unsafe/unchecked.
    let result = match &self.tokens[self.index] {
      token::Token::LiteralString(value) => ast::Literal::String(value.clone()),
      _ => {
        return Err(diagnostic::Diagnostic {
          message: "expected string literal but got end of file".to_string(),
          severity: diagnostic::Severity::Error,
        })
      }
    };

    self.skip();

    Ok(result)
  }

  fn parse_array_value(&mut self) -> ParserResult<ast::ArrayValue> {
    let mut elements = Vec::new();

    skip_past!(self, token::Token::SymbolBracketL);

    loop {
      if self.is(token::Token::SymbolBracketR) {
        break;
      }

      elements.push(self.parse_expr()?);

      // TODO: Make sure there isn't space for lonely commas.
      if self.is(token::Token::SymbolComma) {
        self.skip();
      }
    }

    // Skip the closing bracket.
    self.skip();

    let mut explicit_type = None;

    if elements.is_empty() {
      explicit_type = Some(self.parse_type()?);
    }

    Ok(ast::ArrayValue {
      elements,
      explicit_type,
    })
  }

  fn parse_array_indexing(&mut self) -> ParserResult<ast::ArrayIndexing> {
    let name = self.parse_name()?;

    skip_past!(self, token::Token::SymbolBracketL);

    let index = Box::new(self.parse_expr()?);

    skip_past!(self, token::Token::SymbolBracketR);

    Ok(ast::ArrayIndexing {
      name,
      index,
      definition_key: None,
    })
  }

  fn parse_literal(&mut self) -> ParserResult<ast::Literal> {
    let current_token = &self.tokens[self.index];

    Ok(match current_token {
      token::Token::LiteralBool(_) => self.parse_bool_literal()?,
      token::Token::LiteralInt(_) => self.parse_int_literal()?,
      token::Token::LiteralString(_) => self.parse_string_literal()?,
      _ => {
        return Err(diagnostic::Diagnostic {
          // TODO: Show the actual token.
          message: format!("unexpected token `{}`, expected literal", current_token),
          severity: diagnostic::Severity::Error,
        });
      }
    })
  }

  fn parse_primary_expr(&mut self) -> ParserResult<ast::Node> {
    // TODO: Might need to revisit. Might need to make room for other cases in the future (binary/unary operators, etc).
    Ok(match self.tokens[self.index] {
      token::Token::Identifier(_) => {
        if self.peek_is(token::Token::SymbolParenthesesL) {
          ast::Node::FunctionCall(self.parse_function_call()?)
        } else if self.peek_is(token::Token::SymbolBracketL) {
          ast::Node::ArrayIndexing(self.parse_array_indexing()?)
        } else {
          ast::Node::VariableRef(self.parse_variable_ref()?)
        }
      }
      token::Token::SymbolBracketL => ast::Node::ArrayValue(self.parse_array_value()?),
      // Default to a literal if nothing else matched.
      _ => ast::Node::Literal(self.parse_literal()?),
    })
  }

  fn parse_operator(&mut self) -> ParserResult<ast::OperatorKind> {
    // TODO: Unsafe access. Also, cloning token.
    let current_token = self.tokens[self.index].clone();

    let operator = match current_token {
      token::Token::SymbolBang => ast::OperatorKind::Not,
      token::Token::SymbolPlus => ast::OperatorKind::Add,
      token::Token::SymbolMinus => ast::OperatorKind::Subtract,
      token::Token::SymbolAsterisk => ast::OperatorKind::Multiply,
      token::Token::SymbolSlash => ast::OperatorKind::Divide,
      token::Token::SymbolLessThan => ast::OperatorKind::LessThan,
      token::Token::SymbolGreaterThan => ast::OperatorKind::GreaterThan,
      // TODO: Implement logic for GTE & LTE.
      _ => {
        return Err(diagnostic::Diagnostic {
          message: format!("unexpected token `{}`, expected operator", current_token),
          severity: diagnostic::Severity::Error,
        })
      }
    };

    self.skip();

    Ok(operator)
  }

  fn parse_bin_expr(&mut self, left: ast::Node, min_precedence: usize) -> ParserResult<ast::Node> {
    let mut token = &self.tokens[self.index];
    let precedence = get_token_precedence(&token);
    let mut result = left;

    while is_binary_operator(token) && (precedence > min_precedence) {
      let operator = self.parse_operator()?;
      let mut right = self.parse_primary_expr()?;

      token = &self.tokens[self.index];

      while is_binary_operator(&token) && get_token_precedence(&token) > precedence {
        right = self.parse_bin_expr(right, precedence + 1)?;
        token = &self.tokens[self.index];
      }

      result = ast::Node::BinaryExpr(ast::BinaryExpr {
        left: Box::new(result),
        operator,
        right: Box::new(right),
      });
    }

    Ok(result)
  }

  // TODO: Better naming and/or positioning for logic.
  fn parse_expr(&mut self) -> ParserResult<ast::Node> {
    // TODO: Need support for unary expressions (such as `!`, '-', etc.).

    let left = self.parse_primary_expr()?;

    // TODO: Should the precedence be zero here?
    Ok(self.parse_bin_expr(left, 0)?)
  }

  /// %name '(' (%expr (,))* ')'
  fn parse_function_call(&mut self) -> ParserResult<ast::FunctionCall> {
    let callee_name = self.parse_name()?;

    skip_past!(self, token::Token::SymbolParenthesesL);

    let mut arguments = vec![];

    while !self.is_eof() && !self.is(token::Token::SymbolParenthesesR) {
      arguments.push(self.parse_expr()?);

      if self.is(token::Token::SymbolComma) {
        self.skip();
      }
    }

    skip_past!(self, token::Token::SymbolParenthesesR);

    Ok(ast::FunctionCall {
      callee_name,
      callee_definition_key: None,
      arguments,
    })
  }

  // %name
  fn parse_variable_ref(&mut self) -> ParserResult<ast::VariableRef> {
    let name = self.parse_name()?;

    Ok(ast::VariableRef {
      name,
      definition_key: None,
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
    let parser = Parser::new(vec![token::Token::KeywordFn], &mut context);

    assert_eq!(true, parser.is(token::Token::KeywordFn));
  }

  #[test]
  fn is_empty() {
    let mut context = context::Context::new();
    let parser = Parser::new(vec![], &mut context);

    assert_eq!(false, parser.is(token::Token::KeywordFn));
  }

  #[test]
  fn skip() {
    let mut context = context::Context::new();

    let mut parser = Parser::new(
      vec![token::Token::KeywordFn, token::Token::KeywordFn],
      &mut context,
    );

    parser.skip();
    assert_eq!(1, parser.index);
  }

  #[test]
  fn skip_out_of_bounds() {
    let mut context = context::Context::new();
    let mut parser = Parser::new(vec![token::Token::KeywordFn], &mut context);

    parser.skip();
    assert_eq!(0, parser.index);
  }

  #[test]
  fn is_eof() {
    let mut context = context::Context::new();
    let mut parser = Parser::new(vec![], &mut context);

    assert_eq!(true, parser.is_eof());
    parser.tokens.push(token::Token::KeywordFn);
    assert_eq!(true, parser.is_eof());
    parser.tokens.push(token::Token::KeywordFn);
    assert_eq!(false, parser.is_eof());
    parser.skip();
    assert_eq!(true, parser.is_eof());
  }

  // TODO: Add more tests.
}
