use crate::{block, node, token};

macro_rules! skip {
  ($self:expr, $tok:expr) => {
    if (!$self.is($tok)) {
      return None;
    }

    $self.skip();
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

    match &self.tokens[self.index] {
      token => true,
      _ => false,
    }
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
    skip!(self, token::Token::BraceL('{'));

    // TODO: Do not depend on an EOF token, (can't enforce its presence in tokens vector).
    while !self.is(token::Token::BraceR('}')) && !self.is(token::Token::EOF) {
      // TODO: Parse expressions.
    }

    skip!(self, token::Token::BraceR('}'));

    if self.is(token::Token::EOF) {
      return None;
    }

    Some(block::Block {})
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
    let mut parser = Parser::new(vec![token::Token::BraceL('{'), token::Token::BraceR('}')]);
    let block = parser.parse_block();

    assert_eq!(false, block.is_none());
  }
}
