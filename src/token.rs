#[derive(PartialEq, Debug, Clone)]
pub enum Token {
  Illegal(char),
  Identifier(String),
  Whitespace(String),
  Comment(String),
  LiteralString(String),
  LiteralInt(u64),
  LiteralBool(bool),
  LiteralChar(char),
  KeywordPub,
  KeywordFn,
  KeywordExtern,
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
    "return" => Token::KeywordReturn,
    "if" => Token::KeywordIf,
    "else" => Token::KeywordElse,
    "while" => Token::KeywordWhile,
    "break" => Token::KeywordBreak,
    "i16" => Token::TypeInt16,
    "i32" => Token::TypeInt32,
    "i64" => Token::TypeInt64,
    "bool" => Token::TypeBool,
    "str" => Token::TypeString,
    "true" => Token::LiteralBool(true),
    "false" => Token::LiteralBool(false),
    _ => return None,
  })
}
