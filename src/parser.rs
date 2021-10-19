use crate::{block, diagnostic, external, function, int_kind, node, prototype, token, void_kind};

macro_rules! skip_past {
  ($self:expr, $token:expr) => {
    if !$self.is($token) {
      return Err(diagnostic::Diagnostic {
        message: format!(
          "expected token `{:?}` but got `{:?}`",
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

macro_rules! assert_ok {
  ($expr:expr) => {
    match $expr {
      Ok(_) => $expr.ok().unwrap(),
      Err(_) => return Result::Err($expr.err().unwrap()),
    }
  };
}

macro_rules! assert_some {
  ($expr:expr) => {
    match $expr {
      None => {
        return Err(diagnostic::Diagnostic {
          // TODO:
          message: String::from("todo temporary val"),
          severity: diagnostic::DiagnosticSeverity::Error,
        });
      }
      _ => $expr.unwrap(),
    }
  };
}

type ParserResult<T> = Result<T, diagnostic::Diagnostic>;

struct Parser {
  tokens: Vec<token::Token>,
  index: usize,
}

impl Parser {
  fn new(tokens: Vec<token::Token>) -> Self {
    Self { tokens, index: 0 }
  }

  fn is(&self, token: token::Token) -> bool {
    if self.index >= self.tokens.len() {
      return false;
    }

    self.tokens[self.index] == token
  }

  fn skip(&mut self) -> Option<&token::Token> {
    // FIXME: Address out of bounds problem.
    if self.index + 1 >= self.tokens.len() {
      return None;
    }

    self.index += 1;

    Some(&self.tokens[self.index])
  }

  pub fn parse_name(&mut self) -> ParserResult<String> {
    // TODO: Empty string?
    skip_past!(self, token::Token::Identifier(String::from("")));

    let name = match &self.tokens[self.index] {
      token::Token::Identifier(value) => Some(value),
      _ => None,
    };

    // TODO:
    // if name.is_none() {
    //   return None;
    // }

    Ok(name.unwrap().clone())
  }

  pub fn parse_block(&mut self) -> ParserResult<block::Block> {
    skip_past!(self, token::Token::SymbolBraceL);

    // TODO: Do not depend on an EOF token, (can't enforce its presence in tokens vector).
    while !self.is(token::Token::SymbolBraceR) && !self.is(token::Token::EOF) {
      // TODO: Parse expressions.
    }

    skip_past!(self, token::Token::SymbolBraceR);

    // TODO:
    // if self.is(token::Token::EOF) {
    //   return None;
    // }

    Ok(block::Block {})
  }

  pub fn parse_int_kind(&mut self) -> ParserResult<int_kind::IntKind> {
    let token = assert_some!(self.skip());

    match token {
      token::Token::Identifier(value) => Ok(int_kind::IntKind {
        size: match value.as_str() {
          "i8" => int_kind::IntSize::Signed8,
          "i16" => int_kind::IntSize::Signed16,
          "i32" => int_kind::IntSize::Signed32,
          "i64" => int_kind::IntSize::Signed64,
          "i128" => int_kind::IntSize::Signed128,
          _ => {
            return Err(diagnostic::Diagnostic {
              // TODO:
              message: String::from(format!("invalid integer type name")),
              severity: diagnostic::DiagnosticSeverity::Error,
            });
          }
        },
      }),
      _ => {
        return Err(diagnostic::Diagnostic {
          message: String::from("invalid"),
          severity: diagnostic::DiagnosticSeverity::Error,
        })
      }
    }
  }

  pub fn parse_void_kind(&mut self) -> ParserResult<node::AnyKindNode> {
    skip_past!(self, token::Token::TypeVoid);

    Ok(node::AnyKindNode::VoidKind(void_kind::VoidKind {}))
  }

  pub fn parse_kind(&mut self) -> ParserResult<node::AnyKindNode> {
    assert!(match self.tokens[self.index] {
      token::Token::Identifier(_) => true,
      _ => false,
    });

    // TODO: Support for more types.

    let int_kind = self.parse_int_kind();

    if int_kind.is_err() {
      return Result::Err(int_kind.err().unwrap());
    }

    return Ok(node::AnyKindNode::IntKind(int_kind.ok().unwrap()));
  }

  pub fn parse_prototype(&mut self) -> ParserResult<prototype::Prototype> {
    skip_past!(self, token::Token::SymbolParenthesesL);

    // let mut args = vec![];

    // FIXME: And EOF token.
    // while self.is(token::Token::ParenthesesR) {
    Ok(prototype::Prototype {
      name: assert_ok!(self.parse_name()),
      // TODO: Support for variadic.
      is_variadic: false,
      return_kind: assert_ok!(self.parse_kind()),
    })
    // }
  }

  pub fn parse_function(&mut self) -> ParserResult<function::Function> {
    let mut is_public = false;

    if self.is(token::Token::KeywordPub) {
      is_public = true;
      self.skip();
    }

    skip_past!(self, token::Token::KeywordFn);

    Ok(function::Function {
      is_public,
      prototype: self.parse_prototype().ok().unwrap(),
      body: self.parse_block().ok().unwrap(),
    })
  }

  fn parse_external(&mut self) -> ParserResult<external::External> {
    skip_past!(self, token::Token::KeywordExtern);

    Ok(external::External {
      prototype: assert_ok!(self.parse_prototype()),
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
  fn parser_parse_name() {
    let mut parser = Parser::new(vec![token::Token::Identifier(String::from("foo"))]);
    let name = parser.parse_name();

    assert_eq!(false, name.is_err());
    assert_eq!(String::from("foo"), parser.parse_name().ok().unwrap());
  }

  #[test]
  fn parser_parse_block() {
    let mut parser = Parser::new(vec![token::Token::SymbolBraceL, token::Token::SymbolBraceR]);
    let block = parser.parse_block();

    assert_eq!(false, block.is_err());
  }

  #[test]
  fn parse_kind() {
    let mut parser = Parser::new(vec![token::Token::Identifier(String::from("i8"))]);
    let int_kind = parser.parse_int_kind();

    assert_eq!(false, int_kind.is_err());
    assert_eq!(int_kind.ok().unwrap().size, int_kind::IntSize::Signed8);
  }

  // TODO: Add missing tests.
}
