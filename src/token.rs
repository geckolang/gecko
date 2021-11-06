use crate::diagnostic;

#[derive(PartialEq, Debug, Clone)]
pub enum Token {
  Identifier(String),
  LiteralInt(Vec<char>),
  LiteralBool(bool),
  KeywordPub,
  KeywordFn,
  KeywordExtern,
  KeywordPackage,
  KeywordReturn,
  KeywordMut,
  TypeVoid,
  TypeInt32,
  TypeBool,
  SymbolBraceL,
  SymbolBraceR,
  SymbolParenthesesL,
  SymbolParenthesesR,
  SymbolTilde,
  SymbolSemiColon,
  SymbolColon,
  SymbolAmpersand,
  SymbolComma,
  SymbolPlus,
}

impl std::fmt::Display for Token {
  fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
    write!(f, "{:?}", self)
  }
}

pub fn get_keyword_or_type_token(identifier_str: &str) -> Result<Token, diagnostic::Diagnostic> {
  Ok(match identifier_str {
    "pub" => Token::KeywordPub,
    "fn" => Token::KeywordFn,
    "extern" => Token::KeywordExtern,
    "void" => Token::TypeVoid,
    "i32" => Token::TypeInt32,
    "bool" => Token::TypeBool,
    "package" => Token::KeywordPackage,
    "return" => Token::KeywordReturn,
    "true" => Token::LiteralBool(true),
    "false" => Token::LiteralBool(false),
    "mut" => Token::KeywordMut,
    _ => {
      return Err(diagnostic::Diagnostic {
        message: format!("identifier `{}` is not a keyword", identifier_str),
        severity: diagnostic::DiagnosticSeverity::Error,
      })
    }
  })
}
