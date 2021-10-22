use crate::diagnostic;

#[derive(PartialEq, Debug)]
pub enum Token {
  EOF,
  Identifier(String),
  Integer(Vec<char>),
  KeywordPub,
  KeywordFn,
  KeywordExtern,
  KeywordNamespace,
  TypeVoid,
  TypeInt32,
  SymbolBraceL,
  SymbolBraceR,
  SymbolParenthesesL,
  SymbolParenthesesR,
}

pub fn get_keyword_or_type_token(identifier_str: &str) -> Result<Token, diagnostic::Diagnostic> {
  Ok(match identifier_str {
    "pub" => Token::KeywordPub,
    "fn" => Token::KeywordFn,
    "extern" => Token::KeywordExtern,
    "void" => Token::TypeVoid,
    "i32" => Token::TypeInt32,
    "namespace" => Token::KeywordNamespace,
    _ => {
      return Err(diagnostic::Diagnostic {
        message: format!("identifier `{}` is not a keyword", identifier_str),
        severity: diagnostic::DiagnosticSeverity::Error,
      })
    }
  })
}
