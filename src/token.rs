use crate::diagnostic;

#[derive(PartialEq, Debug, Clone)]
pub enum Token {
  Identifier(String),
  Integer(Vec<char>),
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

pub fn get_keyword_or_type_token(identifier_str: &str) -> Result<Token, diagnostic::Diagnostic> {
  Ok(match identifier_str {
    "pub" => Token::KeywordPub,
    "fn" => Token::KeywordFn,
    "extern" => Token::KeywordExtern,
    "void" => Token::TypeVoid,
    "i32" => Token::TypeInt32,
    "namespace" => Token::KeywordNamespace,
    "return" => Token::KeywordReturn,
    _ => {
      return Err(diagnostic::Diagnostic {
        message: format!("identifier `{}` is not a keyword", identifier_str),
        severity: diagnostic::DiagnosticSeverity::Error,
      })
    }
  })
}
