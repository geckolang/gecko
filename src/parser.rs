use crate::{diagnostic, int_kind, node, token, void_kind};

macro_rules! skip_past {
  ($self:expr, $token:expr) => {
    // FIXME: Something's wrong, we get an error when no input is provided (eof at index 0).
    // if $self.is_eof() {
    //   return Err(diagnostic::Diagnostic {
    //     message: format!("expected token `{}` but reached eof", $token),
    //     severity: diagnostic::DiagnosticSeverity::Error,
    //   });
    // }

    if !$self.is($token) {
      return Err(diagnostic::Diagnostic {
        message: format!(
          "expected token `{}` but got `{}`",
          $token, $self.tokens[$self.index]
        ),
        severity: diagnostic::DiagnosticSeverity::Error,
      });
    }

    $self.skip();
  };
}

#[macro_export]
macro_rules! assert {
  ($condition:expr) => {
    match $condition {
      true => true,
      false => {
        return Err(diagnostic::Diagnostic {
          message: format!("assertion failed: `{}`", stringify!($condition)),
          severity: diagnostic::DiagnosticSeverity::Internal,
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

impl Parser {
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
    // FIXME: Address out of bounds problem.
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

  pub fn is_eof(&self) -> bool {
    self.tokens.len() == 0 || self.index == self.tokens.len() - 1
  }

  pub fn parse_name(&mut self) -> ParserResult<String> {
    // TODO: Illegal/unrecognized tokens are also represented under 'Identifier'.

    // TODO: Wrong error message.
    assert!(match &self.tokens[self.index] {
      token::Token::Identifier(_) => true,
      _ => false,
    });

    let name = {
      match &self.tokens[self.index] {
        token::Token::Identifier(value) => Some(value.clone()),
        _ => None,
      }
    };

    assert!(name.is_some());
    self.skip();

    Ok(name.unwrap())
  }

  pub fn parse_block(&mut self) -> ParserResult<node::Block> {
    skip_past!(self, token::Token::SymbolBraceL);

    let mut statements = vec![];

    while !self.is(token::Token::SymbolBraceR) && !self.is_eof() {
      statements.push(match self.tokens[self.index] {
        token::Token::KeywordReturn => {
          node::AnyStatementNode::ReturnStmt(self.parse_return_stmt()?)
        }
        _ => {
          return Err(diagnostic::Diagnostic {
            message: format!(
              "unexpected token `{:?}`, expected statements",
              self.tokens[self.index]
            ),
            severity: diagnostic::DiagnosticSeverity::Error,
          })
        }
      });
    }

    skip_past!(self, token::Token::SymbolBraceR);

    Ok(node::Block { statements })
  }

  pub fn parse_int_kind(&mut self) -> ParserResult<int_kind::IntKind> {
    // TODO: Simplify weird, unreadable logic?
    let token = if self.skip() {
      self.tokens[self.index - 1].clone()
    } else {
      self.tokens[self.index].clone()
    };

    let is_signed = if token == token::Token::KeywordUnsigned {
      self.skip();

      false
    } else {
      true
    };

    let size = match token {
      token::Token::TypeInt32 => int_kind::IntSize::Bit32,
      _ => {
        return Err(diagnostic::Diagnostic {
          message: format!("unexpected token `{}`, expected integer type", token),
          severity: diagnostic::DiagnosticSeverity::Error,
        })
      }
    };

    Ok(int_kind::IntKind { size, is_signed })
  }

  pub fn parse_void_kind(&mut self) -> ParserResult<void_kind::VoidKind> {
    skip_past!(self, token::Token::TypeVoid);

    Ok(void_kind::VoidKind)
  }

  pub fn parse_bool_kind(&mut self) -> ParserResult<int_kind::BoolKind> {
    skip_past!(self, token::Token::TypeBool);

    Ok(int_kind::BoolKind)
  }

  pub fn parse_kind_group(&mut self) -> ParserResult<node::KindGroup> {
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
      token::Token::TypeVoid => node::AnyKindNode::VoidKind(self.parse_void_kind()?),
      token::Token::TypeInt32 => node::AnyKindNode::IntKind(self.parse_int_kind()?),
      token::Token::TypeBool => node::AnyKindNode::BoolKind(self.parse_bool_kind()?),
      _ => {
        return Err(diagnostic::Diagnostic {
          message: format!(
            "unexpected token `{:?}`, expected type",
            // TODO: Check if the index is valid?
            self.tokens[self.index]
          ),
          severity: diagnostic::DiagnosticSeverity::Error,
        });
      }
    };

    Ok(node::KindGroup {
      kind,
      is_reference,
      is_mutable,
    })
  }

  pub fn parse_parameter(&mut self) -> ParserResult<node::Parameter> {
    let name = self.parse_name()?;

    skip_past!(self, token::Token::SymbolColon);

    let kind_group = self.parse_kind_group()?;

    Ok((name, kind_group))
  }

  pub fn parse_prototype(&mut self) -> ParserResult<node::Prototype> {
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
    skip_past!(self, token::Token::SymbolTilde);

    let return_kind_group = self.parse_kind_group()?;

    Ok(node::Prototype {
      name,
      parameters,
      is_variadic,
      return_kind_group,
    })
  }

  pub fn parse_function(&mut self) -> ParserResult<node::Function> {
    // TODO: Visibility should not be handled here.

    let mut is_public = false;

    if self.is(token::Token::KeywordPub) {
      is_public = true;
      self.skip();
    }

    skip_past!(self, token::Token::KeywordFn);

    let prototype = self.parse_prototype()?;
    let body = self.parse_block()?;

    Ok(node::Function {
      is_public,
      prototype,
      body,
    })
  }

  pub fn parse_external(&mut self) -> ParserResult<node::External> {
    // TODO: Support for visibility.

    skip_past!(self, token::Token::KeywordExtern);
    skip_past!(self, token::Token::KeywordFn);

    let prototype = self.parse_prototype()?;

    skip_past!(self, token::Token::SymbolSemiColon);

    Ok(node::External { prototype })
  }

  pub fn parse_package_decl(&mut self) -> ParserResult<node::Package> {
    skip_past!(self, token::Token::KeywordPackage);

    let name = self.parse_name()?;

    skip_past!(self, token::Token::SymbolSemiColon);

    Ok(node::Package::new(name))
  }

  pub fn parse_top_level_node(&mut self) -> ParserResult<node::TopLevelNode> {
    let mut token = self.tokens.get(self.index);

    if self.is(token::Token::KeywordPub) {
      token = self.peek();
    }

    if token.is_none() {
      return Err(diagnostic::error_unexpected_eof("top-level construct"));
    }

    Ok(match token.unwrap() {
      token::Token::KeywordFn => node::TopLevelNode::Function(self.parse_function()?),
      token::Token::KeywordExtern => node::TopLevelNode::External(self.parse_external()?),
      _ => {
        return Err(diagnostic::Diagnostic {
          message: format!(
            "unexpected token: {:?}, expected top-level construct",
            self.tokens[self.index]
          ),
          severity: diagnostic::DiagnosticSeverity::Error,
        })
      }
    })
  }

  pub fn parse_return_stmt(&mut self) -> ParserResult<node::ReturnStmt> {
    skip_past!(self, token::Token::KeywordReturn);

    let mut value = None;

    if !self.is(token::Token::SymbolSemiColon) {
      value = Some(self.parse_literal()?);
    }

    skip_past!(self, token::Token::SymbolSemiColon);

    Ok(node::ReturnStmt { value })
  }

  pub fn parse_bool_literal(&mut self) -> ParserResult<node::BoolLiteral> {
    Ok(match self.tokens[self.index] {
      token::Token::LiteralBool(value) => {
        self.skip();

        node::BoolLiteral { value }
      }
      // TODO: Better error.
      _ => {
        return Err(diagnostic::Diagnostic {
          message: String::from("unexpected token, expected boolean literal"),
          severity: diagnostic::DiagnosticSeverity::Error,
        })
      }
    })
  }

  pub fn parse_int_literal(&mut self) -> ParserResult<node::IntLiteral> {
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

        let mut size = int_kind::calculate_int_size_of(&value);

        if size < int_kind::IntSize::Bit32 {
          size = int_kind::IntSize::Bit32;
        }

        node::IntLiteral {
          value,
          // TODO: Signed or not.
          kind: int_kind::IntKind {
            size,
            is_signed: true,
          },
        }
      }
      _ => return Err(diagnostic::error_unexpected_eof("integer literal")),
    })
  }

  pub fn parse_literal(&mut self) -> ParserResult<node::AnyLiteralNode> {
    Ok(match self.tokens[self.index] {
      token::Token::LiteralBool(_) => node::AnyLiteralNode::BoolLiteral(self.parse_bool_literal()?),
      token::Token::LiteralInt(_) => node::AnyLiteralNode::IntLiteral(self.parse_int_literal()?),
      _ => {
        return Err(diagnostic::Diagnostic {
          message: String::from("unexpected token, expected literal"),
          severity: diagnostic::DiagnosticSeverity::Error,
        })
      }
    })
  }
}

#[cfg(test)]
mod tests {
  use super::*;

  #[test]
  fn parser_proper_initial_values() {
    let parser = Parser::new(vec![]);

    assert_eq!(0, parser.index);
  }

  #[test]
  fn parser_is() {
    let parser = Parser::new(vec![token::Token::KeywordFn]);

    assert_eq!(true, parser.is(token::Token::KeywordFn));
  }

  #[test]
  fn parser_is_empty() {
    let parser = Parser::new(vec![]);

    assert_eq!(false, parser.is(token::Token::KeywordFn));
  }

  #[test]
  fn parser_skip() {
    let mut parser = Parser::new(vec![token::Token::KeywordFn, token::Token::KeywordFn]);

    parser.skip();
    assert_eq!(1, parser.index);
  }

  #[test]
  fn parser_skip_out_of_bounds() {
    let mut parser = Parser::new(vec![token::Token::KeywordFn]);

    parser.skip();
    assert_eq!(0, parser.index);
  }

  #[test]
  fn parser_is_eof() {
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
  fn parser_parse_name() {
    let mut parser = Parser::new(vec![token::Token::Identifier(String::from("foo"))]);
    let name = parser.parse_name();

    assert_eq!(true, name.is_ok());
    assert_eq!("foo", name.ok().unwrap().as_str());
  }

  #[test]
  fn parser_parse_block() {
    let mut parser = Parser::new(vec![token::Token::SymbolBraceL, token::Token::SymbolBraceR]);
    let block = parser.parse_block();

    assert_eq!(true, block.is_ok());
  }

  #[test]
  fn parser_parse_int_kind() {
    let mut parser = Parser::new(vec![token::Token::TypeInt32]);
    let int_kind = parser.parse_int_kind();

    assert_eq!(true, int_kind.is_ok());
    assert_eq!(int_kind.ok().unwrap().size, int_kind::IntSize::Bit32);
  }

  #[test]
  fn parse_void_kind() {
    let mut parser = Parser::new(vec![token::Token::TypeVoid]);
    let void_kind = parser.parse_void_kind();

    assert_eq!(true, void_kind.is_ok());
  }

  #[test]
  fn parse_package_decl() {
    let mut parser = Parser::new(vec![
      token::Token::KeywordPackage,
      token::Token::Identifier(String::from("test")),
      token::Token::SymbolSemiColon,
    ]);

    let package = parser.parse_package_decl();

    assert_eq!(true, package.is_ok());
    assert_eq!(String::from("test"), package.unwrap().name);
  }

  #[test]
  fn parse_external() {
    let mut parser = Parser::new(vec![
      token::Token::KeywordExtern,
      token::Token::KeywordFn,
      token::Token::Identifier(String::from("test")),
      token::Token::SymbolParenthesesL,
      token::Token::SymbolParenthesesR,
      token::Token::SymbolTilde,
      token::Token::TypeVoid,
      token::Token::SymbolSemiColon,
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
    assert_eq!(int_kind::IntSize::Bit32, int_literal.kind.size);
    assert_eq!(true, int_literal.kind.is_signed);
  }

  #[test]
  fn parse_parameter() {
    let mut parser = Parser::new(vec![
      token::Token::Identifier(String::from("foo")),
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

  // TODO: Add missing tests (is_eof, etc.).
}
