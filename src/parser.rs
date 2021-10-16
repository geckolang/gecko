use crate::{block, external, function, int_kind, node, prototype, token};

macro_rules! skip {
  ($self:expr, $token:expr) => {
    if (!$self.is($token)) {
      return None;
    }

    $self.skip();
  };
}

macro_rules! require {
  ($self:expr, $expr:expr) => {
    match $expr {
      None => return None,
      _ => $expr.unwrap(),
    };
  };
}

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

  fn skip(&mut self) {
    // FIXME: Address out of bounds problem.
    if self.index + 1 >= self.tokens.len() {
      return;
    }

    self.index += 1;
  }

  pub fn parse_name(&mut self) -> Option<String> {
    // TODO: Empty vector?
    skip!(self, token::Token::Identifier(vec![]));

    let name = match &self.tokens[self.index] {
      token::Token::Identifier(chars) => Some(chars),
      _ => None,
    };

    if name.is_none() {
      return None;
    }

    Some(String::from(
      name.unwrap().iter().cloned().collect::<String>(),
    ))
  }

  pub fn parse_block(&mut self) -> Option<block::Block> {
    skip!(self, token::Token::BraceL);

    // TODO: Do not depend on an EOF token, (can't enforce its presence in tokens vector).
    while !self.is(token::Token::BraceR) && !self.is(token::Token::EOF) {
      // TODO: Parse expressions.
    }

    skip!(self, token::Token::BraceR);

    if self.is(token::Token::EOF) {
      return None;
    }

    Some(block::Block {})
  }

  pub fn parse_int_kind(&mut self) -> Option<int_kind::IntKind> {
    let token = self.tokens[self.index];

    self.skip();

    match token {
      token::Token::Identifier(chars) => Some(int_kind::IntKind {
        size: match chars.iter().cloned().collect::<String>().as_str() {
          "i8" => int_kind::IntSize::Signed8,
          "i16" => int_kind::IntSize::Signed16,
          "i32" => int_kind::IntSize::Signed32,
          "i64" => int_kind::IntSize::Signed64,
          "i128" => int_kind::IntSize::Signed128,
          _ => return None,
        },
      }),
      _ => None,
    }
  }

  pub fn parse_kind(&mut self) -> Option<node::AnyKindNode> {
    // TODO: Simplify?
    match self.tokens[self.index] {
      token::Token::Identifier(chars) => {
        let int_kind = self.parse_int_kind();

        if int_kind.is_none() {
          return None;
        }

        return Some(node::AnyKindNode::IntKind(&int_kind.unwrap()));
      }
      _ => return None,
    };
  }

  pub fn parse_prototype(&mut self) -> Option<prototype::Prototype> {
    let name = require!(self, self.parse_name());

    skip!(self, token::Token::ParenthesesL);

    let args = vec![];

    while self.is(token::Token::ParenthesesR) && !self.is(token::Token::EOF) {
      // TODO: Support for variadic.

      args.push((
        require!(self, self.parse_kind()),
        require!(self, self.parse_name()),
      ));
    }

    None
  }

  pub fn parse_function(&mut self) -> Option<function::Function> {
    skip!(self, token::Token::Fn);

    Some(function::Function {
      prototype: require!(self, self.parse_prototype()),

      // FIXME:
      body: require!(self, self.parse_block()),
    })
  }

  fn parse_external(&mut self) -> Option<external::External> {
    skip!(self, token::Token::Extern);

    Some(external::External {
      prototype: require!(self, self.parse_prototype()),
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
    let parser = Parser::new(vec![token::Token::Fn]);

    assert_eq!(true, parser.is(token::Token::Fn));
  }

  #[test]
  fn parser_is_empty() {
    let parser = Parser::new(vec![]);

    assert_eq!(false, parser.is(token::Token::Fn));
  }

  #[test]
  fn parser_skip() {
    let mut parser = Parser::new(vec![token::Token::Fn, token::Token::Fn]);

    parser.skip();
    assert_eq!(1, parser.index);
  }

  #[test]
  fn parser_skip_out_of_bounds() {
    let mut parser = Parser::new(vec![token::Token::Fn]);

    parser.skip();
    assert_eq!(0, parser.index);
  }

  #[test]
  fn parser_parse_name() {
    let mut parser = Parser::new(vec![token::Token::Identifier(vec!['f', 'o', 'o'])]);
    let name = parser.parse_name();

    assert_eq!(false, name.is_none());
    assert_eq!(String::from("foo"), parser.parse_name().unwrap());
  }

  #[test]
  fn parser_parse_block() {
    let mut parser = Parser::new(vec![token::Token::BraceL, token::Token::BraceR]);
    let block = parser.parse_block();

    assert_eq!(false, block.is_none());
  }

  #[test]
  fn parse_kind() {
    let mut parser = Parser::new(vec![token::Token::Identifier(vec!['i', '8'])]);
    let int_kind = parser.parse_int_kind();

    assert_eq!(false, int_kind.is_none());
    assert_eq!(int_kind.unwrap().size, int_kind::IntSize::Signed8);
  }

  // TODO: Add missing tests.
}
