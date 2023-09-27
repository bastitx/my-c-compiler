use crate::ast::{Program, TopLevel, TypeDef, Statement, Expression, Const, UnaryOp, BinaryOp};

fn generate_const_expression(c: &Const) -> String {
    match c {
        Const::Int(i) => format!("\tmovq\t${}, %rax\n", i)
    }
}

fn generate_unary_operation_expression(op: &UnaryOp, exp: &Expression) -> String {
    let l = match op {
        UnaryOp::Negate => String::from("\tneg\t%rax\n"),
        UnaryOp::Complement => String::from("\tnot\t%rax\n"),
        UnaryOp::Not => {
            let l1 = String::from("\tcmpq\t$0, %rax\n");
            let l2 = String::from("\tmovq\t$0, %rax\n");
            let l3 = String::from("\tsete\t%al\n");
            l1 + &l2 + &l3
        }
    };
    generate_expression(exp) + &l
}

fn generate_binary_operation_expression(op: &BinaryOp, exp1: &Expression, exp2: &Expression) -> String {
    let gen_exp2 = generate_expression(exp2);
    let push_exp2 = String::from("\tpushq\t%rax\n");
    let gen_exp1 = generate_expression(exp1);
    let pop_exp2 = String::from("\tpopq\t%rcx\n");
    let operation = match op {
        BinaryOp::Addition => String::from("\taddq\t%rcx, %rax\n"),
        BinaryOp::Multiplication => String::from("\timulq\t%rcx, %rax\n"),
        BinaryOp::Subtraction => String::from("\tsubq\t%rcx, %rax\n"),
        BinaryOp::Division => {
            let l1 = String::from("\tcqo\n"); // convert doubleword
            let l2 = String::from("\tidivq\t%rcx\n");
            l1 + &l2
        },
    };
    gen_exp2 + &push_exp2 + &gen_exp1 + &pop_exp2 + &operation
}

fn generate_expression(exp: &Expression) -> String {
    match exp {
        Expression::ConstExpression(c) => generate_const_expression(c),
        Expression::UnaryOp(op, exp) => generate_unary_operation_expression(op, exp),
        Expression::BinaryOp(op, exp1, exp2) => generate_binary_operation_expression(op, exp1, exp2),
    }
}

fn generate_return_val(exp: &Expression) -> String {
    generate_expression(exp) + "\tret\n"
}

fn generate_statement(statement: &Statement) -> String {
    match statement {
        Statement::ReturnVal(exp) => generate_return_val(exp),
        _ => panic!("Unsupported statement")
    }
}

fn generate_function_body(body: &Vec<Statement>) -> String {
    body.into_iter().fold(String::new(), |acc, x| acc + &generate_statement(&x))
}

fn generate_function(_: &TypeDef, name: &String, body: &Vec<Statement>) -> String {
    let l1 = format!("\t.global\t{}\n", name);
    let l2 = format!("\t.text\n");
    let l3 = format!("{}:\n", name);
    let lines_body = generate_function_body(body);
    return l1 + &l2 + &l3 + &lines_body
}

fn generate_block(tl: &TopLevel) -> String {
    match tl {
        TopLevel::Function { fun_type, name, body } => generate_function(fun_type, name, body)
    }
}

fn generate_blocks(block: Vec<TopLevel>) -> String {
    block.into_iter().fold(String::new(), |acc, x| acc + &generate_block(&x) )
}

pub fn generate(program: Program) -> String {
    generate_blocks(program.block)
}