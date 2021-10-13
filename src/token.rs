#[derive(PartialEq)]
#[derive(Debug)]
pub enum Token {
  EOF,
  Identifier(Vec<char>),
  Integer(Vec<char>),
  Fn,
  BraceL(char),
  BraceR(char)
}

pub fn get_keyword_token(chars: &Vec<char>) -> Result<Token, String> {
  let identifier: String = chars.into_iter().collect();

  match &identifier[..] {
    "fn" => Ok(Token::Fn),
    _ => Err(String::from("Not a keyword"))
  }
}
