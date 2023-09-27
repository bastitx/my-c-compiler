use crate::ast::{self, BinaryOp, Expression, BlockItem};
use crate::lexer::Token;

fn parse_factor<'a>(tokens: &'a[Token<'a>]) -> (ast::Expression, &'a[Token<'a>]) {
    match tokens {
        [Token::IntegerLiteral(i), rest @ ..] => (ast::Expression::ConstExpression(ast::Const::Int(i.parse::<u32>().unwrap())), rest),
        [un_op @ (Token::BitwiseComplement | Token::LogicalNegation | Token:: Negation), rest @ ..] => {
            let (exp, rest) = parse_factor(rest);
            let un_op = match un_op {
                Token::BitwiseComplement => ast::UnaryOp::Complement,
                Token::LogicalNegation => ast::UnaryOp::Not,
                Token::Negation => ast::UnaryOp::Negate,
                _ => panic!("Should not happen")
            };
            (ast::Expression::UnaryOp(un_op, Box::new(exp)), rest)
        },
        [Token::OpenParenthesis, rest @ ..] => {
            let (exp, rest) = parse_expression(rest);
            match rest {
                [Token::CloseParenthesis, rest @ ..] => (exp, rest),
                _ => panic!("Didn't find semi colon")
            }
        },
        [Token::Identifier(name), rest @ ..] => (ast::Expression::Var(name.to_string()), rest),
        [t, ..] => panic!("Unknown Expression {:?}", t),
        [] => panic!("Unexpected end of file")
    }
}

fn token_to_binary_op(token: &Token) -> BinaryOp {
    match token {
        Token::Addition => BinaryOp::Addition,
        Token::Negation => BinaryOp::Subtraction,
        Token::Multiplication => BinaryOp::Multiplication,
        Token::Division => BinaryOp::Division,
        Token::EqualTo => BinaryOp::EqualTo,
        Token::NotEqualTo => BinaryOp::NotEqualTo,
        Token::LogicalAnd => BinaryOp::LogicalAnd,
        Token::LogicalOr => BinaryOp::LogicalOr,
        Token::LessThan => BinaryOp::LessThan,
        Token::LessThanOrEqualTo => BinaryOp::LessThanOrEqualTo,
        Token::GreaterThan => BinaryOp::GreaterThan,
        Token::GreaterThanOrEqualTo => BinaryOp::GreaterThanOrEqualTo,
        _ => panic!("Token not a binary operation")
    }
}

fn parse_binary_expression<'a>(op_tokens: &'a[Token<'a>], next_level: &'a dyn Fn(&'a[Token<'a>]) -> (ast::Expression, &'a[Token<'a>])) -> Box<dyn Fn(&'a[Token<'a>]) -> (ast::Expression, &'a[Token<'a>]) + 'a> {
    let parse_expression_fn = move | tokens: &'a[Token<'a>] | -> (ast::Expression, &'a[Token<'a>]) {
        let (term1, rest) = next_level(tokens);
        fn parse_binary_expression_with_expression<'a>(op_tokens: &'a[Token<'a>], next_level: &dyn Fn(&'a[Token<'a>]) -> (ast::Expression, &'a[Token<'a>]), exp1:  Expression, tokens: &'a[Token<'a>]) -> (ast::Expression, &'a[Token<'a>]) {
            match tokens {
                [t , rest @ ..] if op_tokens.contains(t) => {
                    let (exp2, rest) = next_level(rest);
                    let op = token_to_binary_op(t);
                    let exp = ast::Expression::BinaryOp(op, Box::new(exp1), Box::new(exp2));
                    let (exp, rest) = parse_binary_expression_with_expression(op_tokens, next_level, exp, rest);
                    (exp, &rest)
                }
                _ => (exp1, tokens)
            }
        }
        parse_binary_expression_with_expression(op_tokens, next_level, term1, rest)
    };
    Box::new(parse_expression_fn)
}

fn parse_term<'a>(tokens: &'a[Token<'a>]) -> (ast::Expression, &'a[Token<'a>]) {
    parse_binary_expression(&[Token::Multiplication, Token::Division], &parse_factor)(tokens)
}

fn parse_additive_expression<'a>(tokens: &'a[Token<'a>]) -> (ast::Expression, &'a[Token<'a>]) {
    parse_binary_expression(&[Token::Addition, Token::Negation], &parse_term)(tokens)

}

fn parse_relational_expression<'a>(tokens: &'a[Token<'a>]) -> (ast::Expression, &'a[Token<'a>]) {
    parse_binary_expression(&[Token::LessThan, Token::LessThanOrEqualTo, Token::GreaterThan, Token::GreaterThanOrEqualTo], &parse_additive_expression)(tokens)
}

fn parse_equality_expression<'a>(tokens: &'a[Token<'a>]) -> (ast::Expression, &'a[Token<'a>]) {
    parse_binary_expression(&[Token::EqualTo, Token::NotEqualTo], &parse_relational_expression)(tokens)
}

fn parse_logical_and_expression<'a>(tokens: &'a[Token<'a>]) -> (ast::Expression, &'a[Token<'a>]) {
    parse_binary_expression(&[Token::LogicalAnd], &parse_equality_expression)(tokens)
}

fn parse_logical_or_expression<'a>(tokens: &'a[Token<'a>]) -> (ast::Expression, &'a[Token<'a>]) {
    parse_binary_expression(&[Token::LogicalOr], &parse_logical_and_expression)(tokens)
}

fn parse_expression<'a>(tokens: &'a[Token<'a>]) -> (ast::Expression, &'a[Token<'a>]) {
    match tokens {
        [Token::Identifier(name), Token::Assignment, rest @ ..] => {
            let (exp, rest) = parse_expression(rest);
            (ast::Expression::Assign(name.to_string(), Box::from(exp)), rest)
        }
        tokens => parse_logical_or_expression(tokens)
    }
}

fn parse_statement<'a>(tokens: &'a[Token<'a>]) -> (ast::Statement, &'a[Token<'a>]) {
    match tokens {
        [Token::ReturnKeyword, rest @ ..] => {
            let (exp, rest) = parse_expression(rest);
            match rest {
                [Token::Semicolon, rest @ ..] => (ast::Statement::ReturnVal(exp), rest),
                _ => panic!("Didn't find semi colon")
            }
        },
        [Token::IfKeyword, Token::OpenParenthesis, rest @ ..] => {
            let (exp, rest) = parse_expression(rest);
            match rest {
                [Token::CloseParenthesis, rest @ ..] => {
                    let (statement, rest) = parse_statement(rest);
                    let (else_statement, rest) = if let [Token::ElseKeyword, rest @ ..] = rest {
                        let (else_statement, rest) = parse_statement(rest);
                        (Some(Box::new(else_statement)), rest)
                    } else {
                        (None, rest)
                    };
                    (ast::Statement::Conditional(exp, Box::new(statement), else_statement), rest)
                }
                _ => panic!("Didn't find closing parenthesis")
            }
        },
        tokens => {
            let (exp, rest) = parse_expression(tokens);
            match rest {
                [Token::Semicolon, rest @ ..] => (ast::Statement::ExpressionStatement(exp), rest),
                _ => panic!("Didn't find semi colon")
            }
        }
    }
}

fn parse_block_item<'a>(tokens: &'a[Token<'a>]) -> (ast::BlockItem, &'a[Token<'a>]) {
    match tokens {
        [Token::IntKeyword, Token::Identifier(name), rest @ ..] => {
            match rest {
                [Token::Semicolon, rest @ ..] => (ast::BlockItem::Declaration(name.to_string(), None), rest),
                [Token::Assignment, rest @ ..] => {
                    let (exp, rest) = parse_expression(rest);
                    match rest {
                        [Token::Semicolon, rest @ ..] => (ast::BlockItem::Declaration(name.to_string(), Some(exp)), rest),
                        _ => panic!("Didn't find semi colon")
                    }
                },
                _ => panic!("Incorrect declaration")
            }
        },
        _ => {
            let (statement, rest) = parse_statement(tokens);
            (BlockItem::Statement(statement), rest)
        }
    }
}

fn parse_block_items<'a>(tokens: &'a[Token<'a>]) -> (Vec<ast::BlockItem>, &'a[Token<'a>]) {
    if let Some(Token::CloseBrace) = tokens.first() {
        (vec![], tokens)
    } else {
        let (next_block_item, rest) = parse_block_item(tokens);
        let (block_items, rest) = parse_block_items(rest);
        (vec![next_block_item].into_iter().chain(block_items).collect(), rest)
    }
}

fn parse_block<'a>(tokens: &'a[Token<'a>]) -> (Vec<ast::BlockItem>, &'a[Token<'a>]) {
    match tokens {
        [Token::OpenBrace, rest @ ..] => {
            let (block_items, rest) = parse_block_items(rest);
            match rest {
                [Token::CloseBrace, rest @ ..] => (block_items, rest),
                _ => panic!("Expected closing brace at end of block")
            }
        },
        _ => panic!("Expected block to begin with opening brace")
    }
}

fn parse_top_level<'a>(tokens: &'a[Token<'a>]) -> (ast::TopLevel, &'a[Token<'a>]) {
    match tokens {
        [Token::IntKeyword, Token::Identifier(i), Token::OpenParenthesis, Token::CloseParenthesis, rest @ ..] => {
            let (body, rest) = parse_block(rest);
            let function = ast::TopLevel::Function { fun_type: ast::TypeDef::IntType, name: i.to_string(), body: body };
            (function, rest)
        },
        _ => panic!("Expected funtion statement")
    }
}

fn parse_top_levels(tokens: &[Token]) -> Vec<ast::TopLevel> {
    match tokens {
        [] => vec![],
        tokens => {
            let (top_level, rest) = parse_top_level(tokens);
            let top_levels = parse_top_levels(rest);
            vec![top_level].into_iter().chain(top_levels).collect()
        }
    }
}

pub fn parse(tokens: Vec<Token>) -> ast::Program {
    ast::Program { block: parse_top_levels(tokens.as_slice()) }
}