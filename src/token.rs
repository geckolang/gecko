#[derive(PartialEq, Debug, Clone)]
pub enum Token {
  /// A special token emitted when there are no more tokens to lex.
  EOF,
  Illegal(char),
  Identifier(String),
  Whitespace(String),
  Comment(String),
  LiteralString(String),
  LiteralInt(u64),
  LiteralBool(bool),
  LiteralChar(char),
  KeywordFn,
  KeywordExtern,
  KeywordReturn,
  KeywordLet,
  KeywordIf,
  KeywordElse,
  KeywordWhile,
  KeywordBreak,
  KeywordContinue,
  KeywordUnsafe,
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
  SymbolBracketL,
  SymbolBracketR,
}

impl std::fmt::Display for Token {
  fn fmt(&self, formatter: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
    write!(formatter, "{:?}", self)
  }
}
