use crate::diagnostic;

#[derive(PartialEq, Debug)]
pub enum Token {
  EOF,
  Identifier(String),
  Integer(Vec<char>),
  KeywordPub,
  KeywordFn,
  KeywordExtern,
  TypeVoid,
  TypeIntSigned32,
  SymbolBraceL,
  SymbolBraceR,
  SymbolParenthesesL,
  SymbolParenthesesR,
}

pub fn get_keyword_or_type_token(identifier_str: &str) -> Result<Token, diagnostic::Diagnostic> {
  match identifier_str {
    "pub" => Ok(Token::KeywordPub),
    "fn" => Ok(Token::KeywordFn),
    "extern" => Ok(Token::KeywordExtern),
    "void" => Ok(Token::TypeVoid),
    "i32" => Ok(Token::TypeIntSigned32),
    _ => Err(diagnostic::Diagnostic {
      message: format!("identifier `{}` is not a keyword", identifier_str),
      severity: diagnostic::DiagnosticSeverity::Error,
    }),
  }
}
