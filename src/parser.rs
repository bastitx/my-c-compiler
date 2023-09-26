use crate::ast::{self, BinaryOp, Expression};
use crate::lexer::Token;

fn parse_factor(tokens: Vec<Token>) -> (ast::Expression, Vec<Token>) {
    match tokens.as_slice() {
        [Token::IntegerLiteral(i), rest @ ..] => (ast::Expression::ConstExpression(ast::Const::Int(i.parse::<u32>().unwrap())), rest.to_vec()),
        [un_op @ (Token::BitwiseComplement | Token::LogicalNegation | Token:: Negation), rest @ ..] => {
            let (exp, rest) = parse_factor(rest.to_vec());
            let un_op = match un_op {
                Token::BitwiseComplement => ast::UnaryOp::Complement,
                Token::LogicalNegation => ast::UnaryOp::Not,
                Token::Negation => ast::UnaryOp::Negate,
                _ => panic!("Should not happen")
            };
            (ast::Expression::UnaryOp(un_op, Box::new(exp)), rest.to_vec())
        },
        [Token::OpenParenthesis, rest @ ..] => {
            let (exp, rest) = parse_expression(rest.to_vec());
            match rest.as_slice() {
                [Token::CloseParenthesis, rest @ ..] => (exp, rest.to_vec()),
                _ => panic!("Didn't find semi colon")
            }
        },
        _ => panic!("Unknown Expression")
    }
}

fn parse_term_with_factor(factor1: Expression, tokens: Vec<Token>) -> (ast::Expression, Vec<Token>) {
    match tokens.as_slice() {
        [t @ (Token::Multiplication | Token::Division), rest @ ..] => {
            let (factor2, rest) = parse_factor(rest.to_vec());
            let op = match t {
                Token::Multiplication => BinaryOp::Multiplication,
                Token::Division => BinaryOp::Division,
                _ => panic!("Should not happen")
            };
            let exp = ast::Expression::BinaryOp(op, Box::new(factor1), Box::new(factor2));
            let (exp, rest) = parse_term_with_factor(exp, rest.to_vec());
            (exp, rest.to_vec())
        }
        _ => (factor1, tokens)
    }
}

fn parse_term(tokens: Vec<Token>) -> (ast::Expression, Vec<Token>) {
    let (factor1, rest) = parse_factor(tokens);
    parse_term_with_factor(factor1, rest)
}

fn parse_expression_with_term(term1: Expression, tokens: Vec<Token>) -> (ast::Expression, Vec<Token>) {
    match tokens.as_slice() {
        [t @ (Token::Addition | Token::Negation), rest @ ..] => {
            let (term2, rest) = parse_term(rest.to_vec());
            let op = match t {
                Token::Addition => BinaryOp::Addition,
                Token::Negation => BinaryOp::Subtraction,
                _ => panic!("Should not happen")
            };
            let exp = ast::Expression::BinaryOp(op, Box::new(term1), Box::new(term2));
            let (exp, rest) = parse_expression_with_term(exp, rest.to_vec());
            (exp, rest.to_vec())
        }
        _ => (term1, tokens)
    }
}

fn parse_expression(tokens: Vec<Token>) -> (ast::Expression, Vec<Token>) {
    let (term1, rest) = parse_term(tokens);
    parse_expression_with_term(term1, rest)
}

fn parse_statement(tokens: Vec<Token>) -> (ast::Statement, Vec<Token>) {
    match tokens.as_slice() {
        [Token::ReturnKeyword, rest @ ..] => {
            let (exp, rest) = parse_expression(rest.to_vec());
            match rest.as_slice() {
                [Token::Semicolon, rest @ ..] => (ast::Statement::ReturnVal(exp), rest.to_vec()),
                _ => panic!("Didn't find semi colon")
            }
            
        },
        tokens => panic!("Statement {:?} not supported", tokens)
    }
}

fn parse_block_items(tokens: Vec<Token>) -> (Vec<ast::Statement>, Vec<Token>) {
    if let Some(Token::CloseBrace) = tokens.first() {
        (vec![], tokens)
    } else {
        let (next_statement, rest) = parse_statement(tokens);
        let (statements, rest) = parse_block_items(rest);
        (vec![next_statement].into_iter().chain(statements).collect(), rest)
    }
}

fn parse_block(tokens: Vec<Token>) -> (Vec<ast::Statement>, Vec<Token>) {
    match tokens.as_slice() {
        [Token::OpenBrace, rest @ ..] => {
            let (statements, rest) = parse_block_items(rest.to_vec());
            match rest.as_slice() {
                [Token::CloseBrace, rest @ ..] => (statements, rest.to_vec()),
                _ => panic!("Expected closing brace at end of block")
            }
        },
        _ => panic!("Expected block to begin with opening brace")
    }
}

fn parse_top_level(tokens: Vec<Token>) -> (ast::TopLevel, Vec<Token>) {
    match tokens.as_slice() {
        [Token::IntKeyword, Token::Identifier(i), Token::OpenParenthesis, Token::CloseParenthesis, rest @ ..] => {
            let (body, rest) = parse_block(rest.to_vec());
            let function = ast::TopLevel::Function { fun_type: ast::TypeDef::IntType, name: i.to_string(), body: body };
            (function, rest)
        },
        _ => panic!("Expected funtion statement")
    }
}

fn parse_top_levels(tokens: Vec<Token>) -> Vec<ast::TopLevel> {
    match tokens.as_slice() {
        [] => vec![],
        tokens => {
            let (top_level, rest) = parse_top_level(tokens.to_vec());
            let top_levels = parse_top_levels(rest);
            vec![top_level].into_iter().chain(top_levels).collect()
        }
    }
}

pub fn parse(tokens: Vec<Token>) -> ast::Program {
    ast::Program { block: parse_top_levels(tokens) }
}