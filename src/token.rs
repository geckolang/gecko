use crate::diagnostic;

#[derive(PartialEq, Debug, Clone)]
pub enum Token {
  Identifier(String),
  LiteralInt(Vec<char>),
  LiteralBool(bool),
  KeywordPub,
  KeywordFn,
  KeywordExtern,
  KeywordNamespace,
  KeywordReturn,
  TypeVoid,
  TypeInt32,
  SymbolBraceL,
  SymbolBraceR,
  SymbolParenthesesL,
  SymbolParenthesesR,
  SymbolTilde,
  SymbolSemiColon,
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
    "namespace" => Token::KeywordNamespace,
    "return" => Token::KeywordReturn,
    "true" => Token::LiteralBool(true),
    "false" => Token::LiteralBool(false),
    _ => {
      return Err(diagnostic::Diagnostic {
        message: format!("identifier `{}` is not a keyword", identifier_str),
        severity: diagnostic::DiagnosticSeverity::Error,
      })
    }
  })
}
