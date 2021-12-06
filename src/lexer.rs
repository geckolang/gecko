use crate::{diagnostic, token};

// TODO: Document (or consider merging) the `index` and `read_index` fields.
pub struct Lexer {
  input: Vec<char>,
  index: usize,
  /// Represents the current character.
  ///
  /// If the input string was empty, or if the index is out of
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
      current_char,
    }
  }

  pub fn from_str(string: &str) -> Self {
    Self::new(string.chars().collect())
  }

  /// Set the current character buffer to the character on
  /// the next index.
  ///
  /// If there are no more characters, the
  /// current character buffer will be set to `None` to
  /// indicate the end of the input string.
  pub fn read_char(&mut self) -> Option<char> {
    if self.index + 1 >= self.input.len() {
      self.current_char = None;

      return None;
    }

    self.index += 1;
    self.current_char = Some(self.input[self.index]);

    self.current_char
  }

  pub fn collect_tokens(&mut self) -> Result<Vec<token::Token>, diagnostic::Diagnostic> {
    let mut tokens = Vec::new();

    loop {
      match self.lex_token()? {
        Some(token) => tokens.push(token),
        None => break,
      };

      if tokens.len() > 100 {
        println!("{:?}", tokens);
        unreachable!("Too many tokens! Possible infinite loop?");
      }
    }

    Ok(tokens)
  }

  // TODO: Is this function needed? Why not just expand its contents for readability?
  /// Determine if the current character is unset, and therefore
  /// signifies the end of the input string.
  fn is_eof(&self) -> bool {
    self.current_char.is_none()
  }

  fn read_while(&mut self, predicate: fn(char) -> bool) -> String {
    let mut result = String::new();

    while let Some(character) = self.current_char {
      if !predicate(character) {
        break;
      }

      result.push(character);

      self.read_char();
    }

    result
  }

  fn read_identifier(&mut self) -> String {
    self.read_while(|character| -> bool {
      is_letter(character) || is_digit(character) || character == '_'
    })
  }

  fn read_number(&mut self) -> Result<u64, diagnostic::Diagnostic> {
    let number_result = self.read_while(is_digit).parse::<u64>();

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

    self.read_while(|character| -> bool { character != '\n' })
  }

  fn read_whitespace(&mut self) -> String {
    self.read_while(is_whitespace)
  }

  fn read_string(&mut self) -> String {
    // Skip the opening quote.
    self.read_char();

    let string = self.read_while(|character| -> bool { character != '"' });

    // FIXME: Need to ensure that EOF was not met (which is a possibility).

    // Skip the closing quote.
    self.read_char();

    string
  }

  /// Attempt to retrieve the next token.
  ///
  /// If the end of the input string has been reached, `None` will be
  /// returned. If the current character is neither an identifier nor a
  /// digit, an [`Illegal`] token with the encountered character as its
  /// value will be returned.
  fn lex_token(&mut self) -> Result<Option<token::Token>, diagnostic::Diagnostic> {
    if self.is_eof() {
      return Ok(None);
    }

    let current_char = self.current_char.unwrap();

    let token: token::Token = if is_whitespace(current_char) {
      token::Token::Whitespace(self.read_whitespace())
    } else {
      let final_token = match current_char {
        '#' => token::Token::Comment(self.read_comment()),
        '"' => token::Token::String(self.read_string()),
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
          // NOTE: Identifiers will never start with a digit.
          return if current_char == '_' || is_letter(current_char) {
            let identifier = self.read_identifier();

            match token::get_keyword_or_type_token(identifier.as_str()) {
              Some(keyword_token) => Ok(Some(keyword_token)),
              None => Ok(Some(token::Token::Identifier(identifier))),
            }
          } else if is_digit(current_char) {
            Ok(Some(token::Token::LiteralInt(self.read_number()?)))
          } else {
            let illegal_char = current_char;

            self.read_char();

            Ok(Some(token::Token::Illegal(illegal_char)))
          };
        }
      };

      // Skip simple matched token.
      self.read_char();

      final_token
    };

    Ok(Some(token))
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
    assert_eq!(lexer.current_char, Some('a'));
  }

  #[test]
  fn lex_identifier_single_char() {
    let mut lexer = Lexer::new(vec!['a']);

    assert_eq!(
      Ok(Some(token::Token::Identifier("a".to_string()))),
      lexer.lex_token()
    );
  }

  #[test]
  fn lex_identifier_after_before() {
    let mut lexer = Lexer::new(vec![' ', 'a', 'b', 'c', ' ']);

    assert_eq!(true, lexer.lex_token().is_ok());

    assert_eq!(
      Ok(Some(token::Token::Identifier("abc".to_string()))),
      lexer.lex_token()
    );
  }

  #[test]
  fn lex_identifier() {
    let mut lexer = Lexer::new(vec!['a', 'b', 'c']);

    assert_eq!(
      Ok(Some(token::Token::Identifier("abc".to_string()))),
      lexer.lex_token()
    );
  }

  #[test]
  fn lex_eof() {
    let mut lexer = Lexer::new(vec!['a']);

    assert_eq!(true, lexer.lex_token().is_ok());
    assert_eq!(Ok(None), lexer.lex_token());
  }

  #[test]
  fn lex_empty() {
    let mut lexer = Lexer::new(vec![]);

    assert_eq!(Ok(None), lexer.lex_token());
  }

  #[test]
  fn lex_illegal() {
    let mut lexer = Lexer::new(vec!['?']);

    assert_eq!(Ok(Some(token::Token::Illegal('?'))), lexer.lex_token());
  }

  #[test]
  fn read_char_empty() {
    let mut lexer = Lexer::new(vec![]);

    assert_eq!(None, lexer.read_char());
  }

  #[test]
  fn read_char_single() {
    let lexer = Lexer::new(vec!['a']);

    assert_eq!(Some('a'), lexer.current_char);
  }

  #[test]
  fn read_char_overflow() {
    let mut lexer = Lexer::new(vec!['a']);

    lexer.read_char();
    assert_eq!(0, lexer.index);
    assert_eq!(None, lexer.current_char);
  }

  #[test]
  fn is_whitespace() {
    assert_eq!(true, super::is_whitespace(' '));
    assert_eq!(false, super::is_whitespace('a'));
  }

  #[test]
  fn lex_comment() {
    let mut lexer = Lexer::from_str("#test");

    assert_eq!(
      Ok(Some(token::Token::Comment("test".to_string()))),
      lexer.lex_token()
    );
  }

  #[test]
  fn lex_comment_space() {
    let mut lexer = Lexer::from_str("#hello world");

    assert_eq!(
      Ok(Some(token::Token::Comment("hello world".to_string()))),
      lexer.lex_token()
    );
  }

  #[test]
  fn lex_comment_new_line() {
    let mut lexer = Lexer::from_str("#hello\n world");

    assert_eq!(
      Ok(Some(token::Token::Comment("hello".to_string()))),
      lexer.lex_token()
    );
  }

  #[test]
  fn lex_comment_before() {
    let mut lexer = Lexer::from_str("a#hello\n world");

    assert_eq!(true, lexer.lex_token().is_ok());

    assert_eq!(
      Ok(Some(token::Token::Comment("hello".to_string()))),
      lexer.lex_token()
    );
  }

  #[test]
  fn lex_string() {
    let mut lexer = Lexer::from_str("\"hello\"");

    assert_eq!(
      Ok(Some(token::Token::String("hello".to_string()))),
      lexer.lex_token()
    );
  }

  #[test]
  fn lex_number_single_digit() {
    let mut lexer = Lexer::from_str("1");

    assert_eq!(Ok(Some(token::Token::LiteralInt(1))), lexer.lex_token());
  }

  #[test]
  fn lex_number() {
    let mut lexer = Lexer::from_str("123");

    assert_eq!(Ok(Some(token::Token::LiteralInt(123))), lexer.lex_token());
  }

  #[test]
  fn collect_tokens() {
    let mut lexer = Lexer::from_str("let one = 1");
    let tokens_result = lexer.collect_tokens();

    assert_eq!(true, tokens_result.is_ok());
    assert_eq!(7, tokens_result.unwrap().len());
  }

  // TODO: Add tests for number-overflow cases.
}
