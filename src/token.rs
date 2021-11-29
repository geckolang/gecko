use crate::diagnostic;

#[derive(PartialEq, Debug, Clone)]
pub enum Token {
  Illegal(char),
  Identifier(String),
  LiteralInt(u64),
  LiteralBool(bool),
  KeywordPub,
  KeywordFn,
  KeywordExtern,
  KeywordModule,
  KeywordReturn,
  KeywordMut,
  KeywordUnsigned,
  KeywordLet,
  KeywordIf,
  KeywordElse,
  KeywordWhile,
  KeywordBreak,
  TypeInt16,
  TypeInt32,
  TypeInt64,
  TypeBool,
  SymbolBraceL,
  SymbolBraceR,
  SymbolParenthesesL,
  SymbolParenthesesR,
  SymbolTilde,
  SymbolColon,
  SymbolAmpersand,
  SymbolComma,
  SymbolPlus,
  SymbolEqual,
}

impl std::fmt::Display for Token {
  fn fmt(&self, formatter: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
    write!(formatter, "{:?}", self)
  }
}

pub fn get_keyword_or_type_token(identifier_str: &str) -> Result<Token, diagnostic::Diagnostic> {
  Ok(match identifier_str {
    "pub" => Token::KeywordPub,
    "fn" => Token::KeywordFn,
    "extern" => Token::KeywordExtern,
    "let" => Token::KeywordLet,
    "mut" => Token::KeywordMut,
    "unsigned" => Token::KeywordUnsigned,
    "module" => Token::KeywordModule,
    "return" => Token::KeywordReturn,
    "if" => Token::KeywordIf,
    "else" => Token::KeywordElse,
    "while" => Token::KeywordWhile,
    "break" => Token::KeywordBreak,
    "i16" => Token::TypeInt16,
    "i32" => Token::TypeInt32,
    "i64" => Token::TypeInt64,
    "bool" => Token::TypeBool,
    "true" => Token::LiteralBool(true),
    "false" => Token::LiteralBool(false),
    _ => {
      return Err(diagnostic::Diagnostic {
        message: format!(
          "identifier `{}` is not a keyword or built-in type",
          identifier_str
        ),
        severity: diagnostic::Severity::Internal,
      })
    }
  })
}
