use regex::Regex;

#[derive(Debug, Clone, PartialEq)]
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
    Negation,
    BitwiseComplement,
    LogicalNegation,
    Addition,
    Multiplication,
    Division,
    Modulo,
    LogicalAnd,
    LogicalOr,
    EqualTo,
    NotEqualTo,
    LessThan,
    LessThanOrEqualTo,
    GreaterThan,
    GreaterThanOrEqualTo,
    Assignment,
    Colon,
    QuestionMark,
    IfKeyword,
    ElseKeyword,
    ForKeyword,
    WhileKeyword,
    DoKeyword,
    BreakKeyword,
    ContinueKeyword,
}

static INTEGER_REGEX: &str = r"^([0-9]+)((.|\s)*)";
static ID_REGEX: &str = r"^([A-Za-z_][A-Za-z0-9_]*)((.|\s)*)";


fn get_id(token: &str) -> Token {
    match token {
        "return" => Token::ReturnKeyword,
        "int" => Token::IntKeyword,
        "if" => Token::IfKeyword,
        "else" => Token::ElseKeyword,
        "for" => Token::ForKeyword,
        "while" => Token::WhileKeyword,
        "do" => Token::DoKeyword,
        "break" => Token::BreakKeyword,
        "continue" => Token::ContinueKeyword,
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
        [b'!', b'=', rest @ ..] => vec![Token::NotEqualTo].into_iter().chain(lex_rest(rest)).collect(),
        [b'=', b'=', rest @ ..] => vec![Token::EqualTo].into_iter().chain(lex_rest(rest)).collect(),
        [b'=', rest @ ..] => vec![Token::Assignment].into_iter().chain(lex_rest(rest)).collect(),
        [b'-', rest @ ..] => vec![Token::Negation].into_iter().chain(lex_rest(rest)).collect(),
        [b'~', rest @ ..] => vec![Token::BitwiseComplement].into_iter().chain(lex_rest(rest)).collect(),    
        [b'!', rest @ ..] => vec![Token::LogicalNegation].into_iter().chain(lex_rest(rest)).collect(),
        [b'+', rest @ ..] => vec![Token::Addition].into_iter().chain(lex_rest(rest)).collect(),
        [b'*', rest @ ..] => vec![Token::Multiplication].into_iter().chain(lex_rest(rest)).collect(),
        [b'/', rest @ ..] => vec![Token::Division].into_iter().chain(lex_rest(rest)).collect(),
        [b'&', b'&', rest @ ..] => vec![Token::LogicalAnd].into_iter().chain(lex_rest(rest)).collect(),
        [b'|', b'|', rest @ ..] => vec![Token::LogicalOr].into_iter().chain(lex_rest(rest)).collect(),
        [b'<', b'=', rest @ ..] => vec![Token::LessThanOrEqualTo].into_iter().chain(lex_rest(rest)).collect(),
        [b'<', rest @ ..] => vec![Token::LessThan].into_iter().chain(lex_rest(rest)).collect(),
        [b'>', b'=', rest @ ..] => vec![Token::GreaterThanOrEqualTo].into_iter().chain(lex_rest(rest)).collect(),
        [b'>', rest @ ..] => vec![Token::GreaterThan].into_iter().chain(lex_rest(rest)).collect(),
        [b'?', rest @ ..] => vec![Token::QuestionMark].into_iter().chain(lex_rest(rest)).collect(),
        [b':', rest @ ..] => vec![Token::Colon].into_iter().chain(lex_rest(rest)).collect(),
        [b'%', rest @ ..] => vec![Token::Modulo].into_iter().chain(lex_rest(rest)).collect(),
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
