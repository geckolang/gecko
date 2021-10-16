#[derive(PartialEq, Debug)]
pub enum Token {
  EOF,
  Identifier(Vec<char>),
  Integer(Vec<char>),
  Fn,
  Extern,
  BraceL,
  BraceR,
  ParenthesesL,
  ParenthesesR,
}

pub fn get_keyword_token(chars: &Vec<char>) -> Result<Token, String> {
  let identifier: String = chars.into_iter().collect();

  match &identifier[..] {
    "fn" => Ok(Token::Fn),
    "extern" => Ok(Token::Extern),
    _ => Err(String::from("Not a keyword")),
  }
}
