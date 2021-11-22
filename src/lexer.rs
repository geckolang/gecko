use crate::token;

pub struct Lexer {
  input: Vec<char>,
  index: usize,
  read_index: usize,

  /// Represents the current character. If the input
  /// string was empty, or if the read index is out of
  /// bounds, it will be `None`.
  current_char: Option<char>,
}

/// Determine whether a character is a letter, and within
/// the range of a-Z, or is _.
fn is_letter(character: char) -> bool {
  ('a' <= character && character <= 'z')
    || ('A' <= character && character <= 'Z')
    || character == '_'
}

/// Determine if the character is a digit within the range
/// of 0-9.
fn is_digit(character: char) -> bool {
  character.is_digit(10)
}

impl Lexer {
  pub fn new(input: Vec<char>) -> Self {
    let current_char = if input.is_empty() {
      None
    } else {
      Some(input[0])
    };

    Self {
      input,
      index: 0,
      read_index: 0,
      current_char,
    }
  }

  /// Set the current character buffer to the character on
  /// the next index. If there is no more characters, the
  /// current character buffer will be set to [`None`] to
  /// indicate the end of the input string.
  pub fn read_char(&mut self) {
    if self.read_index >= self.input.len() {
      self.current_char = None;
    } else {
      self.current_char = match self.input.get(self.read_index) {
        Some(value) => Some(value.clone()),
        None => None,
      };
    }

    self.index = self.read_index;
    self.read_index += 1;
  }

  /// Determine if the current character is a whitespace
  /// character.
  fn is_whitespace(&mut self) -> bool {
    if self.current_char.is_none() {
      return false;
    }

    let current_char = self.current_char.unwrap();

    current_char == ' ' || current_char == '\t' || current_char == '\n' || current_char == '\r'
  }

  /// Determine if the current character is unset, and therefore
  /// signifies the end of the input string.
  fn is_eof(&self) -> bool {
    self.input.is_empty() || self.index == self.input.len() - 1
  }

  fn read_identifier(&mut self) -> String {
    let index = self.index;
    let mut current_char = self.current_char.unwrap();

    // NOTE: At this point, we know that the first character
    // of the identifier was a letter, because this closure is
    // only invoked when that is the case.
    while self.index < self.input.len() && (is_letter(current_char) || is_digit(current_char)) {
      self.read_char();

      // TODO: Simplify.
      if self.index < self.input.len() {
        current_char = self.current_char.unwrap();
      }
    }

    self.input[index..self.index]
      .to_vec()
      .iter()
      .cloned()
      .collect::<String>()
  }

  fn read_number(&mut self) -> u64 {
    let index = self.index;

    while self.index < self.input.len() && is_digit(self.current_char.unwrap()) {
      self.read_char();
    }

    self.input[index..self.index]
      .to_vec()
      .iter()
      .cloned()
      .collect::<String>()
      .parse::<u64>()
      .unwrap()
  }
}

impl Iterator for Lexer {
  type Item = token::Token;

  /// Attempt to retrieve the next token.
  ///
  /// If the end of the input string has been reached, `None` will be
  /// returned. If the current character is neither an identifier nor a
  /// digit, an [`Illegal`] token with the encountered character as its
  /// value will be returned.
  fn next(&mut self) -> Option<Self::Item> {
    if self.current_char.is_none() {
      return None;
    }

    // TODO: What if it's EOF + whitespace?
    while self.is_whitespace() && !self.is_eof() {
      self.read_char();
    }

    let token: token::Token = match self.current_char? {
      '{' => token::Token::SymbolBraceL,
      '}' => token::Token::SymbolBraceR,
      '(' => token::Token::SymbolParenthesesL,
      ')' => token::Token::SymbolParenthesesR,
      '~' => token::Token::SymbolTilde,
      ';' => token::Token::SymbolSemicolon,
      ':' => token::Token::SymbolColon,
      '&' => token::Token::SymbolAmpersand,
      ',' => token::Token::SymbolComma,
      '+' => token::Token::SymbolPlus,
      '=' => token::Token::SymbolEqual,
      _ => {
        return if is_letter(self.current_char.unwrap()) {
          let identifier = self.read_identifier();

          match token::get_keyword_or_type_token(identifier.as_str()) {
            Ok(keyword_token) => Some(keyword_token),
            Err(_) => Some(token::Token::Identifier(identifier)),
          }
        } else if is_digit(self.current_char.unwrap()) {
          Some(token::Token::LiteralInt(self.read_number()))
        } else {
          let illegal_char = self.current_char.unwrap();

          self.read_char();

          Some(token::Token::Illegal(illegal_char))
        }
      }
    };

    self.read_char();

    Some(token)
  }
}

#[cfg(test)]
mod tests {
  use super::*;

  #[test]
  fn is_letter() {
    assert_eq!(true, super::is_letter('a'));
    assert_eq!(true, super::is_letter('z'));
    assert_eq!(true, super::is_letter('_'));
    assert_eq!(false, super::is_letter('0'));
    assert_eq!(false, super::is_letter('1'));
    assert_eq!(false, super::is_letter('!'));
  }

  #[test]
  fn is_digit() {
    assert_eq!(false, super::is_digit('a'));
    assert_eq!(false, super::is_digit('z'));
    assert_eq!(false, super::is_digit('_'));
    assert_eq!(true, super::is_digit('0'));
    assert_eq!(true, super::is_digit('1'));
    assert_eq!(false, super::is_digit('!'));
  }

  #[test]
  fn proper_initial_values() {
    let lexer = Lexer::new(vec!['a']);

    assert_eq!(lexer.input.len(), 1);
    assert_eq!(lexer.input[0], 'a');
    assert_eq!(lexer.index, 0);
    assert_eq!(lexer.read_index, 0);
    assert_eq!(lexer.current_char, Some('a'));
  }

  #[test]
  fn next_identifier() {
    let mut lexer = Lexer::new(vec!['a']);

    lexer.read_char();
    assert_eq!(
      Some(token::Token::Identifier("a".to_string())),
      lexer.next()
    );
  }

  #[test]
  fn next_eof() {
    let mut lexer = Lexer::new(vec!['a']);

    lexer.read_char();
    lexer.next();
    assert_eq!(None, lexer.next());
  }

  #[test]
  fn next_none() {
    let mut lexer = Lexer::new(vec![]);

    lexer.read_char();
    assert_eq!(None, lexer.next());
  }

  #[test]
  fn next_illegal() {
    let mut lexer = Lexer::new(vec!['?']);

    lexer.read_char();
    assert_eq!(Some(token::Token::Illegal('?')), lexer.next());
  }

  #[test]
  fn read_char_single() {
    let mut lexer = Lexer::new(vec!['a']);

    lexer.read_char();
    assert_eq!(lexer.index, 0);
    assert_eq!(lexer.read_index, 1);
    assert_eq!(lexer.current_char, Some('a'));
  }

  #[test]
  fn read_char_overflow() {
    let mut lexer = Lexer::new(vec!['a']);

    lexer.read_char();
    lexer.read_char();
    assert_eq!(lexer.index, 1);
    assert_eq!(lexer.read_index, 2);
    assert_eq!(lexer.current_char, None);
  }

  #[test]
  fn is_whitespace() {
    let mut lexer = Lexer::new(vec![' ']);

    assert_eq!(true, lexer.is_whitespace());
  }

  #[test]
  fn is_whitespace_not() {
    let mut lexer = Lexer::new(vec!['a']);

    assert_eq!(false, lexer.is_whitespace());
  }

  #[test]
  fn lex_types() {
    // TODO: Add all types.
    let mut lexer = Lexer::new("i16 i32 i64".chars().collect());

    assert_eq!(Some(token::Token::TypeInt16), lexer.next());
    assert_eq!(Some(token::Token::TypeInt32), lexer.next());
    assert_eq!(Some(token::Token::TypeInt64), lexer.next());
  }

  #[test]
  fn lex_keywords() {
    let mut lexer = Lexer::new(
      "extern pub fn module return mut unsigned let if else"
        .chars()
        .collect(),
    );

    assert_eq!(Some(token::Token::KeywordExtern), lexer.next());
    assert_eq!(Some(token::Token::KeywordPub), lexer.next());
    assert_eq!(Some(token::Token::KeywordFn), lexer.next());
    assert_eq!(Some(token::Token::KeywordModule), lexer.next());
    assert_eq!(Some(token::Token::KeywordReturn), lexer.next());
    assert_eq!(Some(token::Token::KeywordMut), lexer.next());
    assert_eq!(Some(token::Token::KeywordUnsigned), lexer.next());
    assert_eq!(Some(token::Token::KeywordLet), lexer.next());
    assert_eq!(Some(token::Token::KeywordIf), lexer.next());
    assert_eq!(Some(token::Token::KeywordElse), lexer.next());
  }
}
