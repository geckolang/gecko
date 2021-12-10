use crate::{diagnostic, int_kind, ast, token};

macro_rules! skip_past {
  ($self:expr, $token:expr) => {
    // FIXME: Something's wrong, we get an error when no input is provided (eof at index 0).
    // if $self.is_eof() {
    //   return Err(diagnostic::Diagnostic {
    //     message: format!("expected token `{}` but reached eof", $token),
    //     severity: diagnostic::Severity::Error,
    //   });
    // }

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

#[macro_export]
macro_rules! pass_assert {
  ($condition:expr) => {
    match $condition {
      true => true,
      false => {
        return Err(diagnostic::Diagnostic {
          message: format!("assertion failed: `{}`", stringify!($condition)),
          severity: diagnostic::Severity::Internal,
        });
      }
    }
  };
}

type ParserResult<T> = Result<T, diagnostic::Diagnostic>;

pub struct Parser {
  tokens: Vec<token::Token>,
  index: usize,
}

impl<'a> Parser {
  pub fn new(tokens: Vec<token::Token>) -> Self {
    Self { tokens, index: 0 }
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
    crate::pass_assert!(match &self.tokens[self.index] {
      token::Token::Identifier(_) => true,
      _ => false,
    });

    let name = {
      match &self.tokens[self.index] {
        token::Token::Identifier(value) => Some(value.clone()),
        _ => None,
      }
    };

    crate::pass_assert!(name.is_some());
    self.skip();

    Ok(name.unwrap())
  }

  /// TODO: Be specific, which statements? Also include lonely expression.
  /// '{' (%statement+) '}'
  fn parse_block(&mut self, llvm_name: &str) -> ParserResult<ast::Block<'a>> {
    // TODO: Have a symbol table for blocks, and check for re-declarations here.

    skip_past!(self, token::Token::SymbolBraceL);

    let mut statements = vec![];

    while !self.is(token::Token::SymbolBraceR) && !self.is_eof() {
      statements.push(match self.tokens[self.index] {
        token::Token::KeywordReturn => ast::AnyStmtNode::ReturnStmt(self.parse_return_stmt()?),
        token::Token::KeywordLet => ast::AnyStmtNode::LetStmt(self.parse_let_stmt()?),
        token::Token::KeywordIf => ast::AnyStmtNode::IfStmt(self.parse_if_stmt()?),
        token::Token::KeywordWhile => ast::AnyStmtNode::WhileStmt(self.parse_while_stmt()?),
        token::Token::SymbolBraceL => ast::AnyStmtNode::BlockStmt(self.parse_block_stmt()?),
        token::Token::KeywordBreak => ast::AnyStmtNode::BreakStmt(self.parse_break_stmt()?),
        _ => {
          let expr_wrapper_stmt = ast::AnyStmtNode::ExprWrapperStmt(self.parse_expr()?);

          expr_wrapper_stmt
        }
      });
    }

    skip_past!(self, token::Token::SymbolBraceR);

    Ok(ast::Block {
      llvm_name: llvm_name.to_string(),
      statements,
    })
  }

  /// (unsigned) {i8 | i16 | i32 | i64}
  fn parse_int_kind(&mut self) -> ParserResult<int_kind::IntKind> {
    let mut is_signed = true;

    if self.is(token::Token::KeywordUnsigned) {
      self.skip();
      is_signed = false;
    }

    // TODO: Accessing index unsafely.
    let current_token = &self.tokens[self.index];

    let size = match current_token {
      token::Token::TypeInt16 => int_kind::IntSize::Size16,
      token::Token::TypeInt32 => int_kind::IntSize::Size32,
      token::Token::TypeInt64 => int_kind::IntSize::Size64,
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

    Ok(int_kind::IntKind { size, is_signed })
  }

  /// bool
  fn parse_bool_kind(&mut self) -> ParserResult<int_kind::BoolKind> {
    skip_past!(self, token::Token::TypeBool);

    Ok(int_kind::BoolKind)
  }

  /// ('&') (mut) %kind
  fn parse_kind_group(&mut self) -> ParserResult<ast::KindGroup> {
    let mut is_reference = false;
    let mut is_mutable = false;

    if self.is(token::Token::SymbolAmpersand) {
      is_reference = true;
      self.skip();
    }

    if self.is(token::Token::KeywordMut) {
      is_mutable = true;
      self.skip();
    }

    // TODO: Check if the index is valid?
    // TODO: Support for more types.
    let kind = match self.tokens[self.index] {
      token::Token::KeywordUnsigned
      | token::Token::TypeInt16
      | token::Token::TypeInt32
      | token::Token::TypeInt64 => ast::KindHolder::IntKind(self.parse_int_kind()?),
      token::Token::TypeBool => ast::KindHolder::BoolKind(self.parse_bool_kind()?),
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
    };

    Ok(ast::KindGroup {
      kind,
      is_reference,
      is_mutable,
    })
  }

  /// %name ':' %kind_group
  fn parse_parameter(&mut self) -> ParserResult<ast::Parameter> {
    let name = self.parse_name()?;

    skip_past!(self, token::Token::SymbolColon);

    let kind_group = self.parse_kind_group()?;

    Ok((name, kind_group))
  }

  /// %name '(' {%parameter* (,)} (+) ')' '~' %kind_group
  fn parse_prototype(&mut self) -> ParserResult<ast::Prototype> {
    let name = self.parse_name()?;

    skip_past!(self, token::Token::SymbolParenthesesL);

    let mut parameters = vec![];
    let mut is_variadic = false;

    // TODO: Analyze, and remove possibility of lonely comma.
    while !self.is(token::Token::SymbolParenthesesR) && !self.is_eof() {
      if self.is(token::Token::SymbolPlus) {
        is_variadic = true;
        self.skip();

        break;
      }

      parameters.push(self.parse_parameter()?);

      if !self.is(token::Token::SymbolComma) {
        break;
      }

      self.skip();
    }

    skip_past!(self, token::Token::SymbolParenthesesR);

    let mut return_kind_group = None;

    if self.is(token::Token::SymbolTilde) {
      self.skip();
      return_kind_group = Some(self.parse_kind_group()?);
    }

    Ok(ast::Prototype {
      name,
      parameters,
      is_variadic,
      return_kind_group,
    })
  }

  /// (pub) fn %prototype %block
  fn parse_function(&mut self) -> ParserResult<ast::Function<'a>> {
    // TODO: Visibility should not be handled here.

    let mut is_public = false;

    if self.is(token::Token::KeywordPub) {
      is_public = true;
      self.skip();
    }

    skip_past!(self, token::Token::KeywordFn);

    let prototype = self.parse_prototype()?;
    let mut body = self.parse_block("entry")?;

    // Insert a return void instruction if the function body is empty.
    if body.statements.is_empty() {
      body
        .statements
        .push(ast::AnyStmtNode::ReturnStmt(ast::ReturnStmt {
          value: None,
        }));
    }

    Ok(ast::Function {
      is_public,
      prototype,
      body,
    })
  }

  /// (pub) extern fn %prototype
  fn parse_external(&mut self) -> ParserResult<ast::External> {
    // TODO: Support for visibility.

    skip_past!(self, token::Token::KeywordExtern);
    skip_past!(self, token::Token::KeywordFn);

    let prototype = self.parse_prototype()?;

    Ok(ast::External { prototype })
  }

  fn parse_top_level_node(&mut self) -> ParserResult<ast::TopLevelNodeHolder<'a>> {
    let mut token = self.tokens.get(self.index);

    if self.is(token::Token::KeywordPub) {
      token = self.peek();
    }

    if token.is_none() {
      return Err(diagnostic::Diagnostic {
        message: "expected top-level construct but got end of file".to_string(),
        severity: diagnostic::Severity::Error,
      });
    }

    Ok(match token.unwrap() {
      token::Token::KeywordFn => ast::TopLevelNodeHolder::Function(self.parse_function()?),
      token::Token::KeywordExtern => ast::TopLevelNodeHolder::External(self.parse_external()?),
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
  fn parse_return_stmt(&mut self) -> ParserResult<ast::ReturnStmt<'a>> {
    skip_past!(self, token::Token::KeywordReturn);

    let mut value = None;

    // TODO: Does this cover all cases?
    if !self.is(token::Token::SymbolBraceR) {
      value = Some(self.parse_literal()?);
    }

    Ok(ast::ReturnStmt { value })
  }

  /// let %name (':' %kind_group) '=' %expr
  fn parse_let_stmt(&mut self) -> ParserResult<ast::LetStmt<'a>> {
    skip_past!(self, token::Token::KeywordLet);

    let name = self.parse_name()?;
    let mut kind_group = None;

    if self.is(token::Token::SymbolColon) {
      self.skip();
      kind_group = Some(self.parse_kind_group()?);
    }

    skip_past!(self, token::Token::SymbolEqual);

    let value = self.parse_expr()?;

    // Infer the kind of the variable, based on its value.
    if kind_group.is_none() {
      let kind = match &value {
        ast::ExprHolder::BoolLiteral(_) => ast::KindHolder::BoolKind(int_kind::BoolKind),
        ast::ExprHolder::IntLiteral(int_literal) => {
          ast::KindHolder::IntKind(int_literal.kind.clone())
        }
        // TODO: Implement.
        ast::ExprHolder::CallExpr(_) => todo!(),
      };

      kind_group = Some(ast::KindGroup {
        kind,
        is_mutable: false,
        is_reference: false,
      });
    }

    Ok(ast::LetStmt {
      name,
      kind_group: kind_group.unwrap(),
      value,
    })
  }

  /// if %expr %block (else %block)
  fn parse_if_stmt(&mut self) -> ParserResult<ast::IfStmt<'a>> {
    skip_past!(self, token::Token::KeywordIf);

    let condition = self.parse_expr()?;
    let then_block = self.parse_block("if_then")?;
    let mut else_block = None;

    if self.is(token::Token::KeywordElse) {
      self.skip();
      else_block = Some(self.parse_block("if_else")?);
    }

    Ok(ast::IfStmt {
      condition,
      then_block,
      else_block,
    })
  }

  fn parse_while_stmt(&mut self) -> ParserResult<ast::WhileStmt<'a>> {
    skip_past!(self, token::Token::KeywordWhile);

    let condition = self.parse_expr()?;
    let body = self.parse_block("while_then")?;

    Ok(ast::WhileStmt { condition, body })
  }

  fn parse_block_stmt(&mut self) -> ParserResult<ast::BlockStmt<'a>> {
    Ok(ast::BlockStmt {
      block: self.parse_block("block_stmt")?,
    })
  }

  fn parse_break_stmt(&mut self) -> ParserResult<ast::BreakStmt> {
    skip_past!(self, token::Token::KeywordBreak);

    Ok(ast::BreakStmt {})
  }

  /// {true | false}
  fn parse_bool_literal(&mut self) -> ParserResult<ast::BoolLiteral> {
    Ok(match self.tokens[self.index] {
      token::Token::LiteralBool(value) => {
        self.skip();

        ast::BoolLiteral { value }
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

  fn parse_int_literal(&mut self) -> ParserResult<ast::IntLiteral> {
    // TODO:
    // Ok(match self.tokens.get(self.index) {
    //   Some(token::Token::LiteralInt(value)) => {
    //     self.skip();

    //     let size = int_kind::calculate_int_size_of(value);

    //     node::IntLiteral {
    //       value: *value,
    //       kind: int_kind::IntKind { size, is_signed: true },
    //     }
    //   }
    //   _ => return Err(diagnostic::error_unexpected_eof("integer literal")),
    // })
    Ok(match self.tokens[self.index] {
      token::Token::LiteralInt(value) => {
        self.skip();

        let mut size = int_kind::minimum_int_size_of(&value);

        if size < int_kind::IntSize::Size32 {
          size = int_kind::IntSize::Size32;
        }

        ast::IntLiteral {
          value,
          // TODO: Signed or not.
          kind: int_kind::IntKind {
            size,
            is_signed: true,
          },
        }
      }
      _ => {
        return Err(diagnostic::Diagnostic {
          message: "expected integer literal but got end of file".to_string(),
          severity: diagnostic::Severity::Error,
        })
      }
    })
  }

  fn parse_literal(&mut self) -> ParserResult<ast::ExprHolder<'a>> {
    Ok(match self.tokens[self.index] {
      token::Token::LiteralBool(_) => ast::ExprHolder::BoolLiteral(self.parse_bool_literal()?),
      token::Token::LiteralInt(_) => ast::ExprHolder::IntLiteral(self.parse_int_literal()?),
      _ => {
        return Err(diagnostic::Diagnostic {
          // TODO: Show the actual token.
          message: "unexpected token, expected literal".to_string(),
          severity: diagnostic::Severity::Error,
        });
      }
    })
  }

  fn parse_expr(&mut self) -> ParserResult<ast::ExprHolder<'a>> {
    Ok(match self.tokens[self.index] {
      token::Token::Identifier(_) => {
        if self.peek_is(token::Token::SymbolParenthesesL) {
          ast::ExprHolder::CallExpr(self.parse_call_expr()?)
        } else {
          return Err(diagnostic::Diagnostic {
            // TODO: Show the actual token.
            message: "unexpected token, expected expression".to_string(),
            severity: diagnostic::Severity::Error,
          });
        }
      }
      _ => self.parse_literal()?,
    })
  }

  /// %name '(' (%expr (,))* ')'
  fn parse_call_expr(&mut self) -> ParserResult<ast::CallExpr<'a>> {
    let callee_name = self.parse_name()?;

    skip_past!(self, token::Token::SymbolParenthesesL);

    // TODO: Shouldn't it be a `Vec<node::ExprTransport<'a>>`?
    let arguments: Vec<ast::ExprTransport<'a>> = vec![];

    // TODO: Parse arguments.

    skip_past!(self, token::Token::SymbolParenthesesR);

    Ok(ast::CallExpr {
      callee: ast::CalleeStub {
        name: callee_name,
        value: None,
      },
      arguments,
    })
  }

  // TODO: Must add tests for this.
  /// module %name
  pub fn parse_module(&mut self) -> ParserResult<ast::Module<'a>> {
    skip_past!(self, token::Token::KeywordModule);

    let name = self.parse_name()?;
    let mut module = ast::Module::new(name.as_str());

    while !self.is_eof() && !self.is(token::Token::KeywordModule) {
      let top_level_node = self.parse_top_level_node()?;

      let top_level_node_name = match &top_level_node {
        ast::TopLevelNodeHolder::Function(function) => &function.prototype.name,
        ast::TopLevelNodeHolder::External(external) => &external.prototype.name,
      };

      if module.symbol_table.contains_key(top_level_node_name) {
        return Err(diagnostic::Diagnostic {
          message: format!(
            "definition of function `{}` already exists",
            top_level_node_name
          ),
          severity: diagnostic::Severity::Error,
        });
      }

      // TODO: Cloning name.
      module
        .symbol_table
        .insert(top_level_node_name.to_string(), top_level_node);
    }

    Ok(module)
  }
}

#[cfg(test)]
mod tests {
  use super::*;

  #[test]
  fn proper_initial_values() {
    let parser = Parser::new(vec![]);

    assert_eq!(0, parser.index);
  }

  #[test]
  fn is() {
    let parser = Parser::new(vec![token::Token::KeywordFn]);

    assert_eq!(true, parser.is(token::Token::KeywordFn));
  }

  #[test]
  fn is_empty() {
    let parser = Parser::new(vec![]);

    assert_eq!(false, parser.is(token::Token::KeywordFn));
  }

  #[test]
  fn skip() {
    let mut parser = Parser::new(vec![token::Token::KeywordFn, token::Token::KeywordFn]);

    parser.skip();
    assert_eq!(1, parser.index);
  }

  #[test]
  fn skip_out_of_bounds() {
    let mut parser = Parser::new(vec![token::Token::KeywordFn]);

    parser.skip();
    assert_eq!(0, parser.index);
  }

  #[test]
  fn is_eof() {
    let mut parser = Parser::new(vec![]);

    assert_eq!(true, parser.is_eof());
    parser.tokens.push(token::Token::KeywordFn);
    assert_eq!(true, parser.is_eof());
    parser.tokens.push(token::Token::KeywordFn);
    assert_eq!(false, parser.is_eof());
    parser.skip();
    assert_eq!(true, parser.is_eof());
  }

  #[test]
  fn parse_name() {
    let mut parser = Parser::new(vec![token::Token::Identifier("foo".to_string())]);
    let name = parser.parse_name();

    assert_eq!(true, name.is_ok());
    assert_eq!("foo", name.ok().unwrap().as_str());
  }

  #[test]
  fn parse_block() {
    let mut parser = Parser::new(vec![token::Token::SymbolBraceL, token::Token::SymbolBraceR]);
    let block = parser.parse_block("test");

    assert_eq!(true, block.is_ok());
  }

  #[test]
  fn parse_int_kind() {
    let mut parser = Parser::new(vec![token::Token::TypeInt32]);
    let int_kind = parser.parse_int_kind();

    assert_eq!(true, int_kind.is_ok());
    assert_eq!(int_kind.ok().unwrap().size, int_kind::IntSize::Size32);
  }

  #[test]
  fn parse_module_decl() {
    let mut parser = Parser::new(vec![
      token::Token::KeywordModule,
      token::Token::Identifier("test".to_string()),
    ]);

    let module = parser.parse_module();

    assert_eq!(true, module.is_ok());
    assert_eq!(String::from("test"), module.unwrap().name);
  }

  #[test]
  fn parse_external() {
    let mut parser = Parser::new(vec![
      token::Token::KeywordExtern,
      token::Token::KeywordFn,
      token::Token::Identifier("test".to_string()),
      token::Token::SymbolParenthesesL,
      token::Token::SymbolParenthesesR,
    ]);

    let external = parser.parse_external();

    assert_eq!(true, external.is_ok());

    let external_prototype = &external.unwrap().prototype;

    assert_eq!(String::from("test"), external_prototype.name);
    assert_eq!(false, external_prototype.is_variadic);

    // TODO: Verify return kind.
  }

  #[test]
  fn parse_bool_literal() {
    let mut parser_for_true = Parser::new(vec![token::Token::LiteralBool(true)]);
    let true_bool_literal = parser_for_true.parse_bool_literal();

    assert_eq!(true, true_bool_literal.is_ok());
    assert_eq!(true, true_bool_literal.unwrap().value);

    let mut parser_for_false = Parser::new(vec![token::Token::LiteralBool(false)]);
    let false_bool_literal = parser_for_false.parse_bool_literal();

    assert_eq!(true, false_bool_literal.is_ok());
    assert_eq!(false, false_bool_literal.unwrap().value);
  }

  #[test]
  fn parse_int_literal() {
    let mut parser = Parser::new(vec![token::Token::LiteralInt(123)]);
    let int_literal_result = parser.parse_int_literal();

    assert_eq!(true, int_literal_result.is_ok());

    let int_literal = int_literal_result.unwrap();

    assert_eq!(123, int_literal.value);
    assert_eq!(int_kind::IntSize::Size32, int_literal.kind.size);
    assert_eq!(true, int_literal.kind.is_signed);
  }

  #[test]
  fn parse_literal() {
    let mut parser = Parser::new(vec![
      token::Token::LiteralInt(123),
      token::Token::LiteralBool(true),
    ]);

    assert_eq!(true, parser.parse_literal().is_ok());
    assert_eq!(true, parser.parse_literal().is_ok());
  }

  #[test]
  fn parse_parameter() {
    let mut parser = Parser::new(vec![
      token::Token::Identifier("foo".to_string()),
      token::Token::SymbolColon,
      token::Token::TypeInt32,
    ]);

    let parameter = parser.parse_parameter();

    assert_eq!(true, parameter.is_ok());
    assert_eq!(String::from("foo"), parameter.unwrap().0);
  }

  #[test]
  fn parse_kind_group() {
    let mut parser = Parser::new(vec![token::Token::TypeInt32]);
    let kind_group_result = parser.parse_kind_group();

    assert_eq!(true, kind_group_result.is_ok());

    let kind_group_value = kind_group_result.unwrap();

    assert_eq!(false, kind_group_value.is_reference);
    assert_eq!(false, kind_group_value.is_mutable);
  }

  #[test]
  fn parse_kind_group_reference() {
    let mut parser = Parser::new(vec![token::Token::SymbolAmpersand, token::Token::TypeInt32]);
    let kind_group_result = parser.parse_kind_group();

    assert_eq!(true, kind_group_result.is_ok());

    let kind_group_value = kind_group_result.unwrap();

    assert_eq!(true, kind_group_value.is_reference);
    assert_eq!(false, kind_group_value.is_mutable);
  }

  #[test]
  fn parse_kind_group_mutable() {
    let mut parser = Parser::new(vec![token::Token::KeywordMut, token::Token::TypeInt32]);
    let kind_group_result = parser.parse_kind_group();

    assert_eq!(true, kind_group_result.is_ok());

    let kind_group_value = kind_group_result.unwrap();

    assert_eq!(false, kind_group_value.is_reference);
    assert_eq!(true, kind_group_value.is_mutable);
  }

  #[test]
  fn parse_kind_group_mutable_reference() {
    let mut parser = Parser::new(vec![
      token::Token::SymbolAmpersand,
      token::Token::KeywordMut,
      token::Token::TypeInt32,
    ]);

    let kind_group_result = parser.parse_kind_group();

    assert_eq!(true, kind_group_result.is_ok());

    let kind_group_value = kind_group_result.unwrap();

    assert_eq!(true, kind_group_value.is_reference);
    assert_eq!(true, kind_group_value.is_mutable);
  }

  #[test]
  fn parse_return_stmt() {
    let mut parser = Parser::new(vec![
      token::Token::KeywordReturn,
      token::Token::LiteralInt(123),
    ]);

    let return_stmt_result = parser.parse_return_stmt();

    assert_eq!(true, return_stmt_result.is_ok());

    let return_stmt = return_stmt_result.unwrap();

    assert_eq!(true, return_stmt.value.is_some());
  }

  #[test]
  fn parse_return_stmt_void() {
    let mut parser = Parser::new(vec![
      token::Token::KeywordReturn,
      token::Token::SymbolBraceR,
    ]);

    let return_stmt_result = parser.parse_return_stmt();

    assert_eq!(true, return_stmt_result.is_ok());

    let return_stmt = return_stmt_result.unwrap();

    assert_eq!(true, return_stmt.value.is_none());
  }

  #[test]
  fn parse_prototype_no_params() {
    let mut parser = Parser::new(vec![
      token::Token::Identifier("foo".to_string()),
      token::Token::SymbolParenthesesL,
      token::Token::SymbolParenthesesR,
    ]);

    let prototype_result = parser.parse_prototype();

    assert_eq!(true, prototype_result.is_ok());

    let prototype = prototype_result.unwrap();

    assert_eq!(String::from("foo"), prototype.name);
    assert_eq!(false, prototype.is_variadic);
    assert_eq!(true, prototype.parameters.is_empty());
  }

  #[test]
  fn parse_if_stmt() {
    let mut parser = Parser::new(vec![
      token::Token::KeywordIf,
      token::Token::LiteralBool(true),
      token::Token::SymbolBraceL,
      token::Token::SymbolBraceR,
    ]);

    let if_stmt_result = parser.parse_if_stmt();

    assert_eq!(true, if_stmt_result.is_ok());

    let if_stmt = if_stmt_result.unwrap();

    assert_eq!(true, if_stmt.else_block.is_none());
  }

  // TODO: Add more tests.
}
