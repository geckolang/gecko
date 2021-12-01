use crate::{diagnostic, token};

// TODO: Document (or consider merging) the `index` and `read_index` fields.
pub struct Lexer {
  input: Vec<char>,
  index: usize,
  read_index: usize,
  /// Represents the current character.
  ///
  /// If the input
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

/// Determine if the current character is a whitespace
/// character.
fn is_whitespace(character: char) -> bool {
  match character {
    ' ' | '\t' | '\n' | '\r' => true,
    _ => false,
  }
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
  /// the next index. If there are no more characters, the
  /// current character buffer will be set to `None` to
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

  pub fn collect(&mut self) -> Result<Vec<token::Token>, diagnostic::Diagnostic> {
    let mut tokens = Vec::new();

    loop {
      match self.lex_token()? {
        Some(token) => tokens.push(token),
        None => break,
      }
    }

    Ok(tokens)
  }

  /// Determine if the current character is unset, and therefore
  /// signifies the end of the input string.
  fn is_eof(&self) -> bool {
    self.input.is_empty() || self.index == self.input.len() - 1
  }

  fn read_identifier(&mut self) -> String {
    let start_index = self.index;
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

    self.input[start_index..self.index]
      .iter()
      .collect::<String>()
  }

  fn read_number(&mut self) -> Result<u64, diagnostic::Diagnostic> {
    let start_index = self.index;

    while self.index < self.input.len() && is_digit(self.current_char.unwrap()) {
      self.read_char();
    }

    let number_result = self.input[start_index..self.index]
      .iter()
      .collect::<String>()
      .parse::<u64>();

    if let Err(_) = number_result {
      return Err(diagnostic::Diagnostic {
        message: "number might be too large or invalid".to_string(),
        severity: diagnostic::Severity::Error,
      });
    }

    Ok(number_result.unwrap())
  }

  fn read_comment(&mut self) -> String {
    // Skip the hash symbol.
    self.read_char();

    let start_index = self.read_index;

    loop {
      self.read_char();

      if self.current_char.is_none() || self.current_char.unwrap() == '\n' {
        break;
      }
    }

    // TODO: Is there a need to check for EOF here?

    self.input[start_index..self.index]
      .iter()
      .collect::<String>()
  }

  fn read_whitespace(&mut self) -> String {
    // NOTE: At this point, we expect the first character to be a whitespace.
    let start_index = self.index;

    loop {
      self.read_char();

      if self.current_char.is_none() || !is_whitespace(self.current_char.unwrap()) {
        break;
      }
    }

    // TODO: Is there a need to check for EOF here?

    self.input[start_index..self.index]
      .iter()
      .collect::<String>()
  }

  /// Attempt to retrieve the next token.
  ///
  /// If the end of the input string has been reached, `None` will be
  /// returned. If the current character is neither an identifier nor a
  /// digit, an [`Illegal`] token with the encountered character as its
  /// value will be returned.
  fn lex_token(&mut self) -> Result<Option<token::Token>, diagnostic::Diagnostic> {
    if self.current_char.is_none() {
      return Ok(None);
    }

    let current_char = self.current_char.unwrap();

    let token: token::Token = if is_whitespace(current_char) {
      token::Token::Whitespace(self.read_whitespace())
    } else {
      match current_char {
        '#' => token::Token::Comment(self.read_comment()),
        '{' => token::Token::SymbolBraceL,
        '}' => token::Token::SymbolBraceR,
        '(' => token::Token::SymbolParenthesesL,
        ')' => token::Token::SymbolParenthesesR,
        '~' => token::Token::SymbolTilde,
        ':' => token::Token::SymbolColon,
        '&' => token::Token::SymbolAmpersand,
        ',' => token::Token::SymbolComma,
        '+' => token::Token::SymbolPlus,
        '=' => token::Token::SymbolEqual,
        _ => {
          return if is_letter(current_char) {
            let identifier = self.read_identifier();

            match token::get_keyword_or_type_token(identifier.as_str()) {
              Ok(keyword_token) => Ok(Some(keyword_token)),
              Err(_) => Ok(Some(token::Token::Identifier(identifier))),
            }
          } else if is_digit(current_char) {
            Ok(Some(token::Token::LiteralInt(self.read_number()?)))
          } else {
            let illegal_char = current_char;

            self.read_char();

            Ok(Some(token::Token::Illegal(illegal_char)))
          }
        }
      }
    };

    self.read_char();

    Ok(Some(token))
  }
}

// FIXME: These tests are wrong? They are all missing `read_char()` calls?

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

    assert_eq!(
      Ok(Some(token::Token::Identifier("a".to_string()))),
      lexer.lex_token()
    );
  }

  #[test]
  fn next_eof() {
    let mut lexer = Lexer::new(vec!['a']);

    assert_eq!(true, lexer.lex_token().is_ok());
    assert_eq!(Ok(None), lexer.lex_token());
  }

  #[test]
  fn next_none() {
    let mut lexer = Lexer::new(vec![]);

    assert_eq!(Ok(None), lexer.lex_token());
  }

  #[test]
  fn next_illegal() {
    let mut lexer = Lexer::new(vec!['?']);

    assert_eq!(Ok(Some(token::Token::Illegal('?'))), lexer.lex_token());
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
    assert_eq!(true, super::is_whitespace(' '));
    assert_eq!(false, super::is_whitespace('a'));
  }

  #[test]
  fn lex_types() {
    // TODO: Add all types.
    let mut lexer = Lexer::new(vec!["bool", "i16", "i32", "i64"].concat().chars().collect());

    assert_eq!(Ok(Some(token::Token::TypeBool)), lexer.lex_token());
    assert_eq!(Ok(Some(token::Token::TypeInt16)), lexer.lex_token());
    assert_eq!(Ok(Some(token::Token::TypeInt32)), lexer.lex_token());
    assert_eq!(Ok(Some(token::Token::TypeInt64)), lexer.lex_token());
  }

  #[test]
  fn lex_keywords() {
    let mut lexer = Lexer::new(
      vec![
        "extern", "pub", "fn", "module", "return", "mut", "unsigned", "let", "if", "else",
      ]
      .concat()
      .chars()
      .collect(),
    );

    assert_eq!(Ok(Some(token::Token::KeywordExtern)), lexer.lex_token());
    assert_eq!(Ok(Some(token::Token::KeywordPub)), lexer.lex_token());
    assert_eq!(Ok(Some(token::Token::KeywordFn)), lexer.lex_token());
    assert_eq!(Ok(Some(token::Token::KeywordModule)), lexer.lex_token());
    assert_eq!(Ok(Some(token::Token::KeywordReturn)), lexer.lex_token());
    assert_eq!(Ok(Some(token::Token::KeywordMut)), lexer.lex_token());
    assert_eq!(Ok(Some(token::Token::KeywordUnsigned)), lexer.lex_token());
    assert_eq!(Ok(Some(token::Token::KeywordLet)), lexer.lex_token());
    assert_eq!(Ok(Some(token::Token::KeywordIf)), lexer.lex_token());
    assert_eq!(Ok(Some(token::Token::KeywordElse)), lexer.lex_token());
  }

  #[test]
  fn lex_comment() {
    let mut lexer = Lexer::new("#test".chars().collect());

    assert_eq!(
      Ok(Some(token::Token::Comment("test".to_string()))),
      lexer.lex_token()
    );
  }

  #[test]
  fn lex_comment_space() {
    let mut lexer = Lexer::new("#hello world".chars().collect());

    assert_eq!(
      Ok(Some(token::Token::Comment("hello world".to_string()))),
      lexer.lex_token()
    );
  }

  #[test]
  fn lex_comment_new_line() {
    let mut lexer = Lexer::new("#hello\n world".chars().collect());

    assert_eq!(
      Ok(Some(token::Token::Comment("hello".to_string()))),
      lexer.lex_token()
    );
  }

  #[test]
  fn lex_comment_before() {
    let mut lexer = Lexer::new("a #hello\n world".chars().collect());

    assert_eq!(true, lexer.lex_token().is_ok());

    assert_eq!(
      Ok(Some(token::Token::Comment("hello".to_string()))),
      lexer.lex_token()
    );
  }
}
