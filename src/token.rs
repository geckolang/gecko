use crate::diagnostic;

#[derive(PartialEq, Debug, Clone)]
pub enum Token {
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
  TypeVoid,
  TypeInt32,
  TypeBool,
  SymbolBraceL,
  SymbolBraceR,
  SymbolParenthesesL,
  SymbolParenthesesR,
  SymbolTilde,
  SymbolSemicolon,
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
    "void" => Token::TypeVoid,
    "i32" => Token::TypeInt32,
    "bool" => Token::TypeBool,
    "true" => Token::LiteralBool(true),
    "false" => Token::LiteralBool(false),
    _ => {
      return Err(diagnostic::Diagnostic {
        message: format!("identifier `{}` is not a keyword", identifier_str),
        severity: diagnostic::Severity::Internal,
      })
    }
  })
}
