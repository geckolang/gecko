use crate::diagnostic;

pub type Token = (TokenKind, usize);

#[derive(PartialEq, Debug, Clone)]
pub enum TokenKind {
  /// A special token emitted when there are no more tokens to lex.
  EOF,
  Illegal(char),
  Identifier(String),
  Whitespace(String),
  Comment(String),
  LiteralString(String),
  LiteralInt(u64),
  LiteralBool(bool),
  LiteralChar(char),
  LiteralNullptr,
  KeywordFn,
  KeywordExtern,
  KeywordReturn,
  KeywordLet,
  KeywordIf,
  KeywordElse,
  KeywordBreak,
  KeywordContinue,
  KeywordUnsafe,
  KeywordEnum,
  KeywordStruct,
  KeywordMut,
  KeywordNew,
  KeywordLoop,
  KeywordAnd,
  KeywordOr,
  KeywordStatic,
  KeywordNand,
  KeywordNor,
  KeywordXor,
  KeywordType,
  TypeSignedInt8,
  TypeSignedInt16,
  TypeSignedInt32,
  TypeSignedInt64,
  TypeUnsignedInt8,
  TypeUnsignedInt16,
  TypeUnsignedInt32,
  TypeUnsignedInt64,
  TypeBool,
  TypeString,
  SymbolBraceL,
  SymbolBraceR,
  SymbolParenthesesL,
  SymbolParenthesesR,
  SymbolTilde,
  SymbolColon,
  SymbolAmpersand,
  SymbolComma,
  SymbolPlus,
  SymbolMinus,
  SymbolAsterisk,
  SymbolSlash,
  SymbolBang,
  SymbolEqual,
  SymbolSemiColon,
  SymbolLessThan,
  SymbolGreaterThan,
  SymbolBracketL,
  SymbolBracketR,
  SymbolDot,
  SymbolAt,
  SymbolBacktick,
}

impl std::fmt::Display for TokenKind {
  fn fmt(&self, formatter: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
    write!(formatter, "{:?}", self)
  }
}

pub struct Lexer {
  input: Vec<char>,
  index: usize,
  /// Represents the current character.
  ///
  /// If the input string was empty, or if the index is out of
  /// bounds, it will be `None`.
  current_char: Option<char>,
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

  /// Attempt to lex all possible tokens until reaching `EOF`.
  pub fn lex_all(&mut self) -> Result<Vec<Token>, diagnostic::Diagnostic> {
    let mut tokens = Vec::new();

    loop {
      let start_position = self.index;
      let token = self.lex_token()?;

      if token == TokenKind::EOF {
        break;
      }

      tokens.push((token, start_position));
    }

    Ok(tokens)
  }

  fn get_span(&self) -> Option<diagnostic::Span> {
    Some(self.index..self.index)
  }

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
        span: self.get_span(),
      });
    }

    Ok(number_result.unwrap())
  }

  fn read_comment(&mut self) -> String {
    // Skip the hash symbol.
    self.read_char();

    self.read_while(|character| character != '\n')
  }

  fn read_whitespace(&mut self) -> String {
    self.read_while(is_whitespace)
  }

  // TODO: Need tests implemented for this.
  fn read_string(&mut self) -> Result<String, diagnostic::Diagnostic> {
    // Skip the opening double-quote.
    self.read_char();

    let mut string = String::new();

    loop {
      string += self.read_while(|char| char != '"' && char != '\\').as_str();

      // We've reached the end of the string.
      if self.current_char == Some('"') {
        break;
      }

      // Otherwise, there is an escape sequence. Skip the escape character.
      self.read_char();

      // TODO: Escape sequences may also occur for chars (single-characters).
      string += match self.current_char {
        Some('n') => "\n",
        Some('t') => "\t",
        Some('r') => "\r",
        Some('\\') => "\\",
        // TODO: Are we missing any other important escape sequence codes?
        Some(char) => {
          return Err(diagnostic::Diagnostic {
            message: format!("`{}` is not a valid string escape sequence", char),
            severity: diagnostic::Severity::Error,
            span: self.get_span(),
          })
        }
        None => {
          return Err(diagnostic::Diagnostic {
            message: "unexpected end of input, expected character".to_string(),
            severity: diagnostic::Severity::Error,
            span: self.get_span(),
          })
        }
      };

      // Move on from the matched character (if any).
      self.read_char();
    }

    // TODO: We SHOULD be skipping the closing double-quote here (independent function).
    // No need to skip the closing double-quote.

    Ok(string)
  }

  fn read_character(&mut self) -> Result<char, diagnostic::Diagnostic> {
    // Skip the opening single-quote, and retrieve the character.
    let character = self.read_char();

    if character.is_none() {
      return Err(diagnostic::Diagnostic {
        message: "unexpected end of input, expected character".to_string(),
        severity: diagnostic::Severity::Error,
        span: self.get_span(),
      });
    }

    Ok(character.unwrap())
  }

  /// Attempt to retrieve the next token.
  ///
  /// If the end of the input string has been reached, `None` will be
  /// returned. If the current character is neither an identifier nor a
  /// digit, an [`Illegal`] token with the encountered character as its
  /// value will be returned.
  fn lex_token(&mut self) -> Result<TokenKind, diagnostic::Diagnostic> {
    if self.is_eof() {
      return Ok(TokenKind::EOF);
    }

    let current_char = self.current_char.unwrap();

    // TODO: Simplify this chunk of code.
    let token = if is_whitespace(current_char) {
      TokenKind::Whitespace(self.read_whitespace())
    } else {
      let final_token = match current_char {
        '#' => TokenKind::Comment(self.read_comment()),
        '"' => TokenKind::LiteralString(self.read_string()?),
        '\'' => TokenKind::LiteralChar(self.read_character()?),
        '{' => TokenKind::SymbolBraceL,
        '}' => TokenKind::SymbolBraceR,
        '(' => TokenKind::SymbolParenthesesL,
        ')' => TokenKind::SymbolParenthesesR,
        '~' => TokenKind::SymbolTilde,
        ':' => TokenKind::SymbolColon,
        '&' => TokenKind::SymbolAmpersand,
        ',' => TokenKind::SymbolComma,
        '+' => TokenKind::SymbolPlus,
        '-' => TokenKind::SymbolMinus,
        '*' => TokenKind::SymbolAsterisk,
        '/' => TokenKind::SymbolSlash,
        '!' => TokenKind::SymbolBang,
        '=' => TokenKind::SymbolEqual,
        ';' => TokenKind::SymbolSemiColon,
        '<' => TokenKind::SymbolLessThan,
        '>' => TokenKind::SymbolGreaterThan,
        '[' => TokenKind::SymbolBracketL,
        ']' => TokenKind::SymbolBracketR,
        '.' => TokenKind::SymbolDot,
        '@' => TokenKind::SymbolAt,
        '`' => TokenKind::SymbolBacktick,
        _ => {
          // NOTE: Identifiers will never start with a digit.
          return if current_char == '_' || is_letter(current_char) {
            let identifier = self.read_identifier();

            match match_token(identifier.as_str()) {
              Some(keyword_token) => Ok(keyword_token),
              None => Ok(TokenKind::Identifier(identifier)),
            }
          } else if is_digit(current_char) {
            Ok(TokenKind::LiteralInt(self.read_number()?))
          } else {
            let illegal_char = current_char;

            self.read_char();

            Ok(TokenKind::Illegal(illegal_char))
          };
        }
      };

      // Skip simple matched token (or in the case of a string, its closing quote).
      self.read_char();

      final_token
    };

    Ok(token)
  }
}

// TODO: Is this implementation practical? If not, simply remove.
impl Iterator for Lexer {
  type Item = Result<TokenKind, diagnostic::Diagnostic>;

  fn next(&mut self) -> Option<Self::Item> {
    let token = self.lex_token();

    if token.is_err() {
      return Some(Err(token.err().unwrap()));
    }

    Some(Ok(token.unwrap()))
  }
}

// TODO: Should these functions be moved into the implementation of `Lexer`?

fn match_token(identifier: &str) -> Option<TokenKind> {
  Some(match identifier {
    "fn" => TokenKind::KeywordFn,
    "extern" => TokenKind::KeywordExtern,
    "let" => TokenKind::KeywordLet,
    "return" => TokenKind::KeywordReturn,
    "if" => TokenKind::KeywordIf,
    "else" => TokenKind::KeywordElse,
    "break" => TokenKind::KeywordBreak,
    "continue" => TokenKind::KeywordContinue,
    "unsafe" => TokenKind::KeywordUnsafe,
    "enum" => TokenKind::KeywordEnum,
    "struct" => TokenKind::KeywordStruct,
    "mut" => TokenKind::KeywordMut,
    "new" => TokenKind::KeywordNew,
    "loop" => TokenKind::KeywordLoop,
    "and" => TokenKind::KeywordAnd,
    "or" => TokenKind::KeywordOr,
    "static" => TokenKind::KeywordStatic,
    "nand" => TokenKind::KeywordNand,
    "nor" => TokenKind::KeywordNor,
    "xor" => TokenKind::KeywordXor,
    "type" => TokenKind::KeywordType,
    "nullptr" => TokenKind::LiteralNullptr,
    "i8" => TokenKind::TypeSignedInt8,
    "i16" => TokenKind::TypeSignedInt16,
    "i32" => TokenKind::TypeSignedInt32,
    "i64" => TokenKind::TypeSignedInt64,
    "u8" => TokenKind::TypeUnsignedInt8,
    "u16" => TokenKind::TypeUnsignedInt16,
    "u32" => TokenKind::TypeUnsignedInt32,
    "u64" => TokenKind::TypeUnsignedInt64,
    "bool" => TokenKind::TypeBool,
    "str" => TokenKind::TypeString,
    "true" => TokenKind::LiteralBool(true),
    "false" => TokenKind::LiteralBool(false),
    _ => return None,
  })
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
      Ok(TokenKind::Identifier("a".to_string())),
      lexer.lex_token()
    );
  }

  #[test]
  fn lex_identifier_after_before() {
    let mut lexer = Lexer::new(vec![' ', 'a', 'b', 'c', ' ']);

    assert_eq!(true, lexer.lex_token().is_ok());

    assert_eq!(
      Ok(TokenKind::Identifier("abc".to_string())),
      lexer.lex_token()
    );
  }

  #[test]
  fn lex_identifier() {
    let mut lexer = Lexer::new(vec!['a', 'b', 'c']);

    assert_eq!(
      Ok(TokenKind::Identifier("abc".to_string())),
      lexer.lex_token()
    );
  }

  #[test]
  fn lex_eof() {
    let mut lexer = Lexer::new(vec!['a']);

    assert_eq!(true, lexer.lex_token().is_ok());
    assert_eq!(Ok(TokenKind::EOF), lexer.lex_token());
  }

  #[test]
  fn lex_empty() {
    let mut lexer = Lexer::new(vec![]);

    assert_eq!(Ok(TokenKind::EOF), lexer.lex_token());
  }

  #[test]
  fn lex_illegal() {
    let mut lexer = Lexer::new(vec!['?']);

    assert_eq!(Ok(TokenKind::Illegal('?')), lexer.lex_token());
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
      Ok(TokenKind::Comment("test".to_string())),
      lexer.lex_token()
    );
  }

  #[test]
  fn lex_comment_space() {
    let mut lexer = Lexer::from_str("#hello world");

    assert_eq!(
      Ok(TokenKind::Comment("hello world".to_string())),
      lexer.lex_token()
    );
  }

  #[test]
  fn lex_comment_new_line() {
    let mut lexer = Lexer::from_str("#hello\n world");

    assert_eq!(
      Ok(TokenKind::Comment("hello".to_string())),
      lexer.lex_token()
    );
  }

  #[test]
  fn lex_comment_before() {
    let mut lexer = Lexer::from_str("a#hello\n world");

    assert_eq!(true, lexer.lex_token().is_ok());

    assert_eq!(
      Ok(TokenKind::Comment("hello".to_string())),
      lexer.lex_token()
    );
  }

  #[test]
  fn lex_string() {
    let mut lexer = Lexer::from_str("\"hello\"");

    assert_eq!(
      Ok(TokenKind::LiteralString("hello".to_string())),
      lexer.lex_token()
    );
  }

  #[test]
  fn lex_number_single_digit() {
    let mut lexer = Lexer::from_str("1");

    assert_eq!(Ok(TokenKind::LiteralInt(1)), lexer.lex_token());
  }

  #[test]
  fn lex_number() {
    let mut lexer = Lexer::from_str("123");

    assert_eq!(Ok(TokenKind::LiteralInt(123)), lexer.lex_token());
  }

  #[test]
  fn lex_all() {
    let mut lexer = Lexer::from_str("let one = 1");
    let tokens_result = lexer.lex_all();

    assert_eq!(true, tokens_result.is_ok());
    assert_eq!(7, tokens_result.unwrap().len());
  }

  // TODO: Add tests for number-overflow cases.
}
