use regex::Regex;

#[derive(Debug)]
#[derive(Clone)]
pub enum Token<'a> {
    OpenBrace, 
    CloseBrace,
    OpenParenthesis,
    CloseParenthesis,
    Semicolon,
    IntKeyword,
    ReturnKeyword,
    Identifier(&'a str),
    IntegerLiteral(&'a str),
}

static INTEGER_REGEX: &str = r"^([0-9]+)((.|\s)*)";
static ID_REGEX: &str = r"^([A-Za-z_][A-Za-z0-9_]*)((.|\s)*)";


fn get_id(token: &str) -> Token {
    match token {
        "return" => Token::ReturnKeyword,
        "int" => Token::IntKeyword,
        _ => Token::Identifier(token)
    }
}

fn lex_complex_token(input: &str) -> (Token, &str) {
    if let Some(integer_value) = Regex::new(INTEGER_REGEX).unwrap().captures_at(input, 0) {
        (Token::IntegerLiteral(integer_value.get(1).unwrap().as_str()), integer_value.get(2).unwrap().as_str())
    } else if let Some(keyword) = Regex::new(ID_REGEX).unwrap().captures_at(input, 0) {
        (get_id(keyword.get(1).unwrap().as_str()), keyword.get(2).unwrap().as_str())
    } else {
        panic!("")
    }
}

fn lex_const_or_id(chars: &[u8]) -> Vec<Token> {
    let s = std::str::from_utf8(chars).unwrap();
    let (token, rest) = lex_complex_token(s);
    return vec![token].into_iter().chain(lex_rest(rest.as_bytes())).collect()
}

fn lex_rest(chars: &[u8]) -> Vec<Token> {
    match chars {
        [] => vec![],
        [b'{', rest @ ..] => vec![Token::OpenBrace].into_iter().chain(lex_rest(rest)).collect(),
        [b'}', rest @ ..] => vec![Token::CloseBrace].into_iter().chain(lex_rest(rest)).collect(),
        [b'(', rest @ ..] => vec![Token::OpenParenthesis].into_iter().chain(lex_rest(rest)).collect(),
        [b')', rest @ ..] => vec![Token::CloseParenthesis].into_iter().chain(lex_rest(rest)).collect(),
        [b';', rest @ ..] => vec![Token::Semicolon].into_iter().chain(lex_rest(rest)).collect(),
        [c, rest @ ..] => if c.is_ascii_whitespace() {
            lex_rest(rest)
        } else {
            lex_const_or_id(chars)
        }
    }
}

pub fn lex(s: &str) -> Vec<Token> {
    return lex_rest(s.as_bytes())
}
