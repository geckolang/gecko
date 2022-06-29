pub type Token = (TokenKind, usize);

#[derive(PartialEq, Debug, Clone)]
pub enum TokenKind {
  /// A special token emitted when there are no more tokens to lex.
  EOF,
  Indent,
  Dedent,
  Illegal(char),
  Identifier(String),
  Whitespace(char),
  Comment(String),
  String(String),
  Int(u64),
  Bool(bool),
  Char(char),
  Nullptr,
  Func,
  Extern,
  Return,
  Let,
  If,
  Else,
  Break,
  Continue,
  Unsafe,
  Enum,
  Struct,
  Mut,
  New,
  Loop,
  And,
  Or,
  Static,
  Nand,
  Nor,
  Xor,
  Type,
  Impl,
  For,
  Trait,
  TypeInt8,
  TypeInt16,
  TypeInt32,
  TypeInt64,
  TypeUint8,
  TypeUint16,
  TypeUint32,
  TypeUint64,
  TypeBool,
  TypeString,
  TypeThis,
  TypeUnit,
  BraceL,
  BraceR,
  ParenthesesL,
  ParenthesesR,
  DollarSign,
  Colon,
  Ampersand,
  Comma,
  Plus,
  Minus,
  Asterisk,
  Slash,
  Bang,
  Equal,
  LessThan,
  GreaterThan,
  BracketL,
  BracketR,
  Dot,
  At,
  Backtick,
  Arrow,
  LessThanEqualTo,
  GreaterThanEqualTo,
  Equality,
  FatArrow,
  Ellipsis,
  Import,
  DoubleColon,
  QuestionMark,
  Sizeof,
  Pipe,
  Const,
  Pass,
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
  seen_only_whitespace_this_line: bool,
  indent_level: usize,
  indent_counter: usize,
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
      seen_only_whitespace_this_line: true,
      indent_level: 0,
      indent_counter: 0,
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
  pub fn lex_all(
    &mut self,
  ) -> Result<Vec<Token>, codespan_reporting::diagnostic::Diagnostic<usize>> {
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

  /// Determine if the current character is unset, and therefore
  /// signifies the end of the input string.
  fn is_eof(&self) -> bool {
    self.current_char.is_none()
  }

  /// Determine if there is a next character.
  fn is_next_eof(&self) -> bool {
    self.index + 1 >= self.input.len()
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

  fn read_number(&mut self) -> Result<u64, codespan_reporting::diagnostic::Diagnostic<usize>> {
    let number_result = self.read_while(is_digit).parse::<u64>();

    if let Err(_) = number_result {
      return Err(
        codespan_reporting::diagnostic::Diagnostic::error()
          .with_message("number might be too large or invalid"),
      );
    }

    Ok(number_result.unwrap())
  }

  fn read_comment(&mut self) -> String {
    // Skip the hash symbol.
    self.read_char();

    self.read_while(|character| character != '\n')
  }

  // TODO: Need tests implemented for this.
  fn read_string(&mut self) -> Result<String, codespan_reporting::diagnostic::Diagnostic<usize>> {
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

      // TODO: Escape sequences should also be possible for chars (single-characters).
      string += match self.current_char {
        Some('n') => "\n",
        Some('t') => "\t",
        Some('r') => "\r",
        Some('\\') => "\\",
        // REVIEW: Are we missing any other important escape sequence codes?
        Some(char) => {
          return Err(
            codespan_reporting::diagnostic::Diagnostic::error()
              .with_message(format!("`{}` is not a valid string escape sequence", char)),
          )
        }
        None => {
          return Err(
            codespan_reporting::diagnostic::Diagnostic::error()
              .with_message("unexpected end of input, expected character"),
          );
        }
      };

      // Move on from the matched character (if any).
      self.read_char();
    }

    // REVISE: We SHOULD be skipping the closing double-quote here (function independence).
    // No need to skip the closing double-quote.

    Ok(string)
  }

  fn read_character(&mut self) -> Result<char, codespan_reporting::diagnostic::Diagnostic<usize>> {
    // Skip the opening single-quote, and retrieve the character.
    let character = self.read_char();

    if character.is_none() {
      return Err(
        codespan_reporting::diagnostic::Diagnostic::error()
          .with_message("unexpected end of input, expected character"),
      );
    }

    Ok(character.unwrap())
  }

  fn peek_char(&self) -> Option<char> {
    if self.is_next_eof() {
      return None;
    }

    Some(self.input[self.index + 1])
  }

  fn is_indent(&self) -> bool {
    self.seen_only_whitespace_this_line
      && (self.current_char == Some(' ') && self.peek_char() == Some(' '))
  }

  /// Attempt to retrieve the next token.
  ///
  /// If the end of the input string has been reached, `None` will be
  /// returned. If the current character is neither an identifier nor a
  /// digit, an [`Illegal`] token with the encountered character as its
  /// value will be returned.
  fn lex_token(&mut self) -> Result<TokenKind, codespan_reporting::diagnostic::Diagnostic<usize>> {
    if self.is_eof() {
      if self.indent_level > 0 {
        self.indent_level -= 1;

        return Ok(TokenKind::Dedent);
      }

      return Ok(TokenKind::EOF);
    }

    // TODO: Cleanup logic. This buffer might cause bugs (has already caused one!).
    let mut current_char = self.current_char.unwrap();

    if !is_whitespace(current_char) && self.seen_only_whitespace_this_line {
      self.seen_only_whitespace_this_line = false;
    }
    // Reset the indent counter upon reaching the end of each line.
    else if current_char == '\n' {
      self.seen_only_whitespace_this_line = true;
      self.indent_counter = 0;
      self.read_char();

      return Ok(TokenKind::Whitespace(current_char));
    }

    while self.is_indent() {
      self.read_char();
      self.read_char();

      // TODO: Cleanup repeated logic.
      if self.is_eof() {
        return Ok(TokenKind::EOF);
      }

      current_char = self.current_char.unwrap();

      self.indent_counter += 1;
    }

    if self.indent_counter > self.indent_level {
      self.indent_level = self.indent_counter;

      return Ok(TokenKind::Indent);
    } else if self.indent_counter < self.indent_level {
      self.indent_level = self.indent_counter;

      return Ok(TokenKind::Dedent);
    }

    if is_whitespace(current_char) {
      self.read_char();

      return Ok(TokenKind::Whitespace(current_char));
    }

    let result = match current_char {
      '#' => TokenKind::Comment(self.read_comment()),
      '"' => TokenKind::String(self.read_string()?),
      '\'' => TokenKind::Char(self.read_character()?),
      '{' => TokenKind::BraceL,
      '}' => TokenKind::BraceR,
      '(' => TokenKind::ParenthesesL,
      ')' => TokenKind::ParenthesesR,
      '$' => TokenKind::DollarSign,
      '|' if self.peek_char() == Some('>') => {
        self.read_char();

        TokenKind::Pipe
      }
      ':' if self.peek_char() == Some(':') => {
        self.read_char();

        TokenKind::DoubleColon
      }
      ':' => TokenKind::Colon,
      '&' => TokenKind::Ampersand,
      ',' => TokenKind::Comma,
      '+' => TokenKind::Plus,
      '-' if self.peek_char() == Some('>') => {
        self.read_char();

        TokenKind::Arrow
      }
      '-' => TokenKind::Minus,
      '*' => TokenKind::Asterisk,
      '/' => TokenKind::Slash,
      '!' => TokenKind::Bang,
      '=' if self.peek_char() == Some('=') => {
        self.read_char();

        TokenKind::Equality
      }
      '=' if self.peek_char() == Some('>') => {
        self.read_char();

        TokenKind::FatArrow
      }
      '=' => TokenKind::Equal,
      '<' if self.peek_char() == Some('=') => {
        self.read_char();

        TokenKind::LessThanEqualTo
      }
      '<' => TokenKind::LessThan,
      '>' if self.peek_char() == Some('=') => {
        self.read_char();

        TokenKind::LessThanEqualTo
      }
      '>' => TokenKind::GreaterThan,
      '[' => TokenKind::BracketL,
      ']' => TokenKind::BracketR,
      '.' if self.peek_char() == Some('.') && self.peek_char() == Some('.') => {
        self.read_char();
        self.read_char();

        TokenKind::Ellipsis
      }
      '.' => TokenKind::Dot,
      '@' => TokenKind::At,
      '`' => TokenKind::Backtick,
      '?' => TokenKind::QuestionMark,
      _ => {
        // NOTE: Identifiers will never start with a digit.
        return if current_char == '_' || is_letter(current_char) {
          let identifier = self.read_identifier();

          match match_identifier(identifier.as_str()) {
            Some(keyword_token) => Ok(keyword_token),
            None => Ok(TokenKind::Identifier(identifier)),
          }
        } else if is_digit(current_char) {
          Ok(TokenKind::Int(self.read_number()?))
        } else {
          let illegal_char = current_char;

          self.read_char();

          Ok(TokenKind::Illegal(illegal_char))
        };
      }
    };

    self.read_char();

    Ok(result)
  }
}

// REVIEW: Is this implementation practical (even used)? If not, simply remove.
impl Iterator for Lexer {
  type Item = Result<TokenKind, codespan_reporting::diagnostic::Diagnostic<usize>>;

  fn next(&mut self) -> Option<Self::Item> {
    let token = self.lex_token();

    if token.is_err() {
      return Some(Err(token.err().unwrap()));
    }

    Some(Ok(token.unwrap()))
  }
}

// REVIEW: Should these functions be moved into the implementation of `Lexer`,
// ... as associated functions?

fn match_identifier(identifier: &str) -> Option<TokenKind> {
  Some(match identifier {
    "func" => TokenKind::Func,
    "extern" => TokenKind::Extern,
    "let" => TokenKind::Let,
    "return" => TokenKind::Return,
    "if" => TokenKind::If,
    "else" => TokenKind::Else,
    "break" => TokenKind::Break,
    "continue" => TokenKind::Continue,
    "unsafe" => TokenKind::Unsafe,
    "enum" => TokenKind::Enum,
    "struct" => TokenKind::Struct,
    "mut" => TokenKind::Mut,
    "new" => TokenKind::New,
    "loop" => TokenKind::Loop,
    "and" => TokenKind::And,
    "or" => TokenKind::Or,
    "static" => TokenKind::Static,
    "nand" => TokenKind::Nand,
    "nor" => TokenKind::Nor,
    "xor" => TokenKind::Xor,
    "type" => TokenKind::Type,
    "impl" => TokenKind::Impl,
    "for" => TokenKind::For,
    "trait" => TokenKind::Trait,
    "nullptr" => TokenKind::Nullptr,
    "I8" => TokenKind::TypeInt8,
    "I16" => TokenKind::TypeInt16,
    "Int" => TokenKind::TypeInt32,
    "I64" => TokenKind::TypeInt64,
    "U8" => TokenKind::TypeUint8,
    "U16" => TokenKind::TypeUint16,
    "U32" => TokenKind::TypeUint32,
    "U64" => TokenKind::TypeUint64,
    "Bool" => TokenKind::TypeBool,
    "Str" => TokenKind::TypeString,
    "This" => TokenKind::TypeThis,
    "Unit" => TokenKind::TypeUnit,
    "true" => TokenKind::Bool(true),
    "false" => TokenKind::Bool(false),
    "import" => TokenKind::Import,
    "sizeof" => TokenKind::Sizeof,
    "const" => TokenKind::Const,
    "pass" => TokenKind::Pass,
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
    let mut lexer = Lexer::new(vec!['|']);

    assert_eq!(Ok(TokenKind::Illegal('|')), lexer.lex_token());
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

    // TODO: Investigate.
    // assert_eq!(Some('a'), lexer.read_char());
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
      Ok(TokenKind::String("hello".to_string())),
      lexer.lex_token()
    );
  }

  #[test]
  fn lex_number_single_digit() {
    let mut lexer = Lexer::from_str("1");

    assert_eq!(Ok(TokenKind::Int(1)), lexer.lex_token());
  }

  #[test]
  fn lex_number() {
    let mut lexer = Lexer::from_str("123");

    assert_eq!(Ok(TokenKind::Int(123)), lexer.lex_token());
  }

  #[test]
  fn lex_indent() {
    let mut lexer = Lexer::from_str("  a  \n  b");

    assert_eq!(Ok(TokenKind::Indent), lexer.lex_token());

    assert_eq!(
      Ok(TokenKind::Identifier(String::from("a"))),
      lexer.lex_token()
    );

    assert_eq!(Ok(TokenKind::Whitespace(' ')), lexer.lex_token());
    assert_eq!(Ok(TokenKind::Whitespace(' ')), lexer.lex_token());
    assert_eq!(Ok(TokenKind::Whitespace('\n')), lexer.lex_token());

    assert_eq!(
      Ok(TokenKind::Identifier(String::from("b"))),
      lexer.lex_token()
    );
  }

  #[test]
  fn lex_dedent() {
    let mut lexer = Lexer::from_str("  a\nb");

    assert_eq!(Ok(TokenKind::Indent), lexer.lex_token());

    assert_eq!(
      Ok(TokenKind::Identifier(String::from("a"))),
      lexer.lex_token()
    );

    assert_eq!(Ok(TokenKind::Whitespace('\n')), lexer.lex_token());
    assert_eq!(Ok(TokenKind::Dedent), lexer.lex_token());

    assert_eq!(
      Ok(TokenKind::Identifier(String::from("b"))),
      lexer.lex_token()
    );
  }

  #[test]
  fn lex_dedent_eof() {
    let mut lexer = Lexer::from_str("  a\n    b\n");

    assert_eq!(Ok(TokenKind::Indent), lexer.lex_token());

    assert_eq!(
      Ok(TokenKind::Identifier(String::from("a"))),
      lexer.lex_token()
    );

    assert_eq!(Ok(TokenKind::Whitespace('\n')), lexer.lex_token());
    assert_eq!(Ok(TokenKind::Indent), lexer.lex_token());

    assert_eq!(
      Ok(TokenKind::Identifier(String::from("b"))),
      lexer.lex_token()
    );

    assert_eq!(Ok(TokenKind::Whitespace('\n')), lexer.lex_token());
    assert_eq!(Ok(TokenKind::Dedent), lexer.lex_token());
    assert_eq!(Ok(TokenKind::Dedent), lexer.lex_token());
    assert_eq!(Ok(TokenKind::EOF), lexer.lex_token());
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
