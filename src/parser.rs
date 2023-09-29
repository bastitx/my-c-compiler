use std::vec;

use crate::ast::{self, BinaryOp, Expression, BlockItem, OptionalExpression};
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
        [Token::Identifier(name), Token::OpenParenthesis, rest @ ..] => parse_function_call(name, rest),
        [Token::Identifier(name), rest @ ..] => (ast::Expression::Var(name.to_string()), rest),
        [t, ..] => panic!("Unknown Expression {:?}", t),
        [] => panic!("Unexpected end of file")
    }
}

fn parse_function_call_parameters<'a>(tokens: &'a[Token<'a>]) -> (Vec<ast::Expression>, &'a[Token<'a>]) {
    let (exp, rest) = parse_expression(tokens);
    let (exps, rest) = parse_function_call_parameters_rest(rest);
    (vec![exp].into_iter().chain(exps).collect(), rest)
}

fn parse_function_call_parameters_rest<'a>(tokens: &'a[Token<'a>]) -> (Vec<ast::Expression>, &'a[Token<'a>]) {
    match tokens {
        [Token::CloseParenthesis, rest @ ..] => (vec![], rest),
        [Token::Comma, rest @ ..] => parse_function_call_parameters(rest),
        _ => panic!("Need to separate parameters by commas in function call")
    }
}

fn parse_function_call<'a>(name: &str, tokens: &'a[Token<'a>]) -> (ast::Expression, &'a[Token<'a>]) {
    let (parameters, rest) = match tokens {
        [Token::CloseParenthesis, rest @ ..] => (vec![], rest),
        tokens => parse_function_call_parameters(tokens)
    };
    (ast::Expression::FunCall(name.to_string(), parameters), rest)
}

const fn token_to_binary_op(token: &Token) -> BinaryOp {
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
        Token::Modulo => BinaryOp::Modulo,
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
    parse_binary_expression(&[Token::Multiplication, Token::Division, Token::Modulo], &parse_factor)(tokens)
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

fn parse_conditional_expression<'a>(tokens: &'a[Token<'a>]) -> (ast::Expression, &'a[Token<'a>]) {
    let (exp1, rest) = parse_logical_or_expression(tokens);
    match rest {
        [Token::QuestionMark, rest @ ..] => {
            let (exp2, rest) = parse_expression(rest);
            match rest {
                [Token::Colon, rest @ ..] => {
                    let (exp3, rest) = parse_conditional_expression(rest);
                    (ast::Expression::Conditional(Box::new(exp1), Box::new(exp2), Box::new(exp3)), rest)
                },
                _ => panic!("Expected colon")
            }
        }
        _ => (exp1, rest)
    }
}

fn parse_expression<'a>(tokens: &'a[Token<'a>]) -> (ast::Expression, &'a[Token<'a>]) {
    match tokens {
        [Token::Identifier(name), Token::Assignment, rest @ ..] => {
            let (exp, rest) = parse_expression(rest);
            (ast::Expression::Assign(name.to_string(), Box::from(exp)), rest)
        }
        tokens => parse_conditional_expression(tokens)
    }
}

fn parse_optional_expression<'a>(tokens: &'a[Token<'a>]) -> (ast::OptionalExpression, &'a[Token<'a>]) {
    match tokens {
        [Token::Semicolon | Token::CloseParenthesis, ..] => (ast::OptionalExpression::None, tokens),
        tokens => {
            let (exp, rest) = parse_expression(tokens);
            (ast::OptionalExpression::Some(exp), rest)
        }
    }
}

fn parse_if_statement<'a>(tokens: &'a[Token<'a>]) -> (ast::Statement, &'a[Token<'a>]) {
    let (exp, rest) = parse_expression(tokens);
    let (statement, rest) = match rest {
        [Token::CloseParenthesis, rest @ ..] => parse_statement(rest),
        _ => panic!("Didn't find closing parenthesis")
    };
    let (else_statement, rest) = if let [Token::ElseKeyword, rest @ ..] = rest {
        let (else_statement, rest) = parse_statement(rest);
        (Some(Box::new(else_statement)), rest)
    } else {
        (None, rest)
    };
    (ast::Statement::Conditional(exp, Box::new(statement), else_statement), rest)
}

fn parse_for_loop_rest<'a>(tokens: &'a[Token<'a>]) -> (ast::Expression, ast::OptionalExpression, ast::Statement, &'a[Token<'a>]) {
    let (exp2, rest) = parse_optional_expression(tokens);
    let exp2 = if let Some(exp) = exp2 {
        exp
    } else {
        ast::Expression::ConstExpression(ast::Const::Int(1))
    };
    let rest = match rest {
        [Token::Semicolon, rest @ ..] => rest,
        _ => panic!("Expected semi colon")
    };
    let (exp3, rest) = parse_optional_expression(rest);
    let rest = match rest {
        [Token::CloseParenthesis, rest @ ..] => rest,
        _ => panic!("Expected semi colon")
    };
    let (statement, rest) = parse_statement(rest);
    (exp2, exp3, statement, rest)
}

fn parse_for_loop<'a>(tokens: &'a[Token<'a>]) -> (ast::Statement, &'a[Token<'a>]) {
    let (exp1, rest) = parse_optional_expression(tokens);
    let rest = match rest {
        [Token::Semicolon, rest @ ..] => rest,
        _ => panic!("Expected semi colon")
    };
    let (exp2, exp3, statement, rest) = parse_for_loop_rest(rest);
    (ast::Statement::For(exp1, exp2, exp3, Box::new(statement)), rest)
}

fn parse_for_decl_loop<'a>(var_name: &str, tokens: &'a[Token<'a>]) -> (ast::Statement, &'a[Token<'a>]) {
    let (declaration, rest) = parse_declaration(var_name, tokens);
    let (exp2, exp3, statement, rest) = parse_for_loop_rest(rest);
    (ast::Statement::ForDecl(declaration, exp2, exp3, Box::new(statement)), rest)
}

fn parse_while_loop<'a>(tokens: &'a[Token<'a>]) -> (ast::Statement, &'a[Token<'a>]) {
    let (exp, rest) = parse_expression(tokens);
    let rest = match rest {
        [Token::CloseParenthesis, rest @ ..] => rest,
        _ => panic!("Expected semi colon")
    };
    let (statement, rest) = parse_statement(rest);
    (ast::Statement::While(exp, Box::new(statement)), rest)
}

fn parse_do_while_loop<'a>(tokens: &'a[Token<'a>]) -> (ast::Statement, &'a[Token<'a>]) {
    let (statement, rest) = parse_statement(tokens);
    let rest = match rest {
        [Token::WhileKeyword, Token::OpenParenthesis, rest @ ..] => rest,
        _ => panic!("Expected semi colon")
    };
    let (exp, rest) = parse_expression(rest);
    let rest = match rest {
        [Token::CloseParenthesis, Token::Semicolon, rest @ ..] => rest,
        _ => panic!("Expected closing parenthesis and semi colon")
    };
    (ast::Statement::DoWhile(Box::new(statement), exp), rest)
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
        [Token::IfKeyword, Token::OpenParenthesis, rest @ ..] => parse_if_statement(rest),
        [Token::OpenBrace, rest @ ..] => {
            let (block_items, rest) = parse_block_items(rest);
            match rest {
                [Token::CloseBrace, rest @ ..] => (ast::Statement::Compound(block_items), rest),
                _ => panic!("Expected closing brace at end of block")
            }
        },
        [Token::ForKeyword, Token::OpenParenthesis, Token::IntKeyword, Token::Identifier(name), rest @ ..] => parse_for_decl_loop(name, rest),
        [Token::ForKeyword, Token::OpenParenthesis, rest @ ..] => parse_for_loop(rest),
        [Token::WhileKeyword, Token::OpenParenthesis, rest @ ..] => parse_while_loop(rest),
        [Token::DoKeyword, rest @ ..] => parse_do_while_loop(rest),
        [Token::BreakKeyword, Token::Semicolon, rest @ ..] => (ast::Statement::Break, rest),
        [Token::ContinueKeyword, Token::Semicolon, rest @ ..] => (ast::Statement::Continue, rest),
        tokens => {
            let (optional_exp, rest) = parse_optional_expression(tokens);
            let rest = match rest {
                [Token::Semicolon, rest @ ..] => rest,
                _ => panic!("Expected semi colon")
            };
            (ast::Statement::Expression(optional_exp), rest)
        },
    }
}

fn parse_declaration<'a>(name: &str, tokens: &'a[Token<'a>]) -> (ast::Declaration, &'a[Token<'a>]) {
    match tokens {
        [Token::Semicolon, rest @ ..] => (ast::Declaration::Declaration(name.to_string(), OptionalExpression::None), rest),
        [Token::Assignment, rest @ ..] => {
            let (exp, rest) = parse_expression(rest);
            match rest {
                [Token::Semicolon, rest @ ..] => (ast::Declaration::Declaration(name.to_string(), OptionalExpression::Some(exp)), rest),
                _ => panic!("Didn't find semi colon")
            }
        },
        _ => panic!("Incorrect declaration with remaining tokens {:?}", tokens)
    }
}

fn parse_block_item<'a>(tokens: &'a[Token<'a>]) -> (ast::BlockItem, &'a[Token<'a>]) {
    match tokens {
        [Token::IntKeyword, Token::Identifier(name), rest @ ..] => {
            let (declaration, rest) = parse_declaration(name, rest);
            (ast::BlockItem::Declaration(declaration), rest)
        },
        _ => {
            let (statement, rest) = parse_statement(tokens);
            (BlockItem::Statement(statement), rest)
        }
    }
}

fn parse_block_items<'a>(tokens: &'a[Token<'a>]) -> (Vec<ast::BlockItem>, &'a[Token<'a>]) {
    match tokens {
        [Token::CloseBrace, ..] => (vec![], tokens),
        _ => {
            let (next_block_item, rest) = parse_block_item(tokens);
            let (block_items, rest) = parse_block_items(rest);
            (vec![next_block_item].into_iter().chain(block_items).collect(), rest)
        }
    }
}

fn parse_function_definition_parameters<'a>(tokens: &'a[Token<'a>]) -> (Vec<String>, &'a[Token<'a>]) {
    let (name, rest) = match tokens {
        [Token::IntKeyword, Token::Identifier(name), rest @ ..] => (name.to_string(), rest),
        _ => panic!("Unexpected token in function definition parameter list")
    };
    let (names, rest) = match rest {
        [Token::CloseParenthesis, rest @ ..] => (vec![], rest),
        [Token::Comma, rest @ ..] => parse_function_definition_parameters(rest),
        _ => panic!("Need to separate parameters by commas in function definition")
    };
    (vec![name].into_iter().chain(names).collect(), rest)
}

fn parse_function_definition<'a>(name: &str, tokens: &'a[Token<'a>]) -> (ast::TopLevel, &'a[Token<'a>]) {
    let (parameters, rest) = match tokens {
        [Token::CloseParenthesis, rest @ ..] => (vec![], rest),
        tokens => parse_function_definition_parameters(tokens)
    };
    match rest {
        [Token::Semicolon, rest @ ..] => {
            let function = ast::TopLevel::Function { fun_type: ast::TypeDef::IntType, name: name.to_string(), parameters, body: None };
            (function, rest)
        },
        [Token::OpenBrace, rest @ ..] => {
            let (body, rest) = parse_block_items(rest);
            let rest = match rest {
                [Token::CloseBrace, rest @ ..] => rest,
                _ => panic!("Expected close brace at end of function body")
            };
            let function = ast::TopLevel::Function { fun_type: ast::TypeDef::IntType, name: name.to_string(), parameters, body: Some(body) };
            (function, rest)
        },
        _ => panic!("Expected open brace at beginning of function body")
    }
}

fn parse_top_level<'a>(tokens: &'a[Token<'a>]) -> (ast::TopLevel, &'a[Token<'a>]) {
    match tokens {
        [Token::IntKeyword, Token::Identifier(i), Token::OpenParenthesis, rest @ ..] => parse_function_definition(i, rest),
        _ => panic!("Expected function statement")
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