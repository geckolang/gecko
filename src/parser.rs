use crate::{
  block, diagnostic, external, function, int_kind, namespace, node, prototype, token, void_kind,
};

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

type ParserResult<T> = Result<T, diagnostic::Diagnostic>;

pub struct Parser {
  tokens: Vec<token::Token>,
  index: usize,
}

fn find_top_level_node_name(top_level_node: &namespace::TopLevelNode) -> String {
  match top_level_node {
    namespace::TopLevelNode::Function(function) => function.prototype.name.clone(),
    namespace::TopLevelNode::External(external) => external.prototype.name.clone(),
  }
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

  fn is_eof(&self) -> bool {
    self.tokens.len() == 0 || self.index == self.tokens.len() - 1
  }

  pub fn parse_name(&mut self) -> ParserResult<String> {
    // TODO: Wrong error message.
    assert!(match &self.tokens[self.index] {
      token::Token::Identifier(_) => true,
      _ => false,
    });
    // skip_past!(self, token::Token::Identifier(String::from("")));

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
    let token = match self.skip() {
      true => &self.tokens[self.index - 1],
      false => &self.tokens[self.index],
    };

    match token {
      // FIXME: Integer kind tokens are available.
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

  pub fn parse_void_kind(&mut self) -> ParserResult<void_kind::VoidKind> {
    skip_past!(self, token::Token::TypeVoid);

    Ok(void_kind::VoidKind {})
  }

  pub fn parse_kind(&mut self) -> ParserResult<node::AnyKindNode> {
    // TODO: Support for more types.
    Ok(match self.tokens[self.index] {
      token::Token::TypeVoid => node::AnyKindNode::VoidKind(self.parse_void_kind()?),
      token::Token::TypeInt32 => node::AnyKindNode::IntKind(self.parse_int_kind()?),
      _ => {
        return Err(diagnostic::Diagnostic {
          message: String::from("foo"),
          severity: diagnostic::DiagnosticSeverity::Internal,
        })
      }
    })
  }

  pub fn parse_prototype(&mut self) -> ParserResult<prototype::Prototype> {
    let name = self.parse_name()?;

    skip_past!(self, token::Token::SymbolParenthesesL);

    // TODO: Parse args.

    skip_past!(self, token::Token::SymbolParenthesesR);
    skip_past!(self, token::Token::SymbolTilde);

    let return_kind = self.parse_kind()?;

    Ok(prototype::Prototype {
      name,
      // TODO: Support for variadic.
      is_variadic: false,
      return_kind,
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

    let prototype = self.parse_prototype()?;
    let body = self.parse_block()?;

    Ok(function::Function {
      is_public,
      prototype,
      body,
    })
  }

  pub fn parse_external(&mut self) -> ParserResult<external::External> {
    skip_past!(self, token::Token::KeywordExtern);

    Ok(external::External {
      prototype: self.parse_prototype()?,
    })
  }

  pub fn parse_namespace(&mut self) -> ParserResult<namespace::Namespace> {
    skip_past!(self, token::Token::KeywordNamespace);

    let name = self.parse_name()?;

    skip_past!(self, token::Token::SymbolBraceL);

    let mut namespace = namespace::Namespace::new(name);

    // TODO: Verify condition.
    while !self.is(token::Token::SymbolBraceR) && !self.is_eof() {
      let top_level_node = match self.tokens[self.index] {
        token::Token::KeywordFn => namespace::TopLevelNode::Function(self.parse_function()?),
        token::Token::KeywordExtern => namespace::TopLevelNode::External(self.parse_external()?),
        _ => {
          return Err(diagnostic::Diagnostic {
            message: format!("unexpected token: {:?}", self.tokens[self.index]),
            severity: diagnostic::DiagnosticSeverity::Error,
          })
        }
      };

      println!("parsed top-level!!");

      namespace
        .symbol_table
        .insert(find_top_level_node_name(&top_level_node), top_level_node);
    }

    skip_past!(self, token::Token::SymbolBraceR);

    Ok(namespace)
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
    assert_eq!("foo", name.ok().unwrap().as_str());
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

  // TODO: Add missing tests (is_eof, parse_namespace, etc.).
}
