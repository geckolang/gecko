#[derive(PartialEq, Debug, Clone)]
pub enum Token {
  Illegal(char),
  Identifier(String),
  Whitespace(String),
  Comment(String),
  String(String),
  LiteralInt(u64),
  LiteralBool(bool),
  KeywordPub,
  KeywordFn,
  KeywordExtern,
  KeywordModule,
  KeywordReturn,
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

pub fn get_keyword_or_type_token(identifier_str: &str) -> Option<Token> {
  Some(match identifier_str {
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
    _ => return None,
  })
}
