use crate::ast::{Program, TopLevel, TypeDef, Statement, Expression, Const, UnaryOp};

fn generate_const_expression(c: &Const) -> String {
    match c {
        Const::Int(i) => format!("\tmov\t${}, %rax\n", i)
    }
}

fn generate_unary_operation_expression(op: &UnaryOp, exp: &Expression) -> String {
    let l = match op {
        UnaryOp::Negate => String::from("\tneg\t%rax\n"),
        UnaryOp::Complement => String::from("\tnot\t%rax\n"),
        UnaryOp::Not => {
            let l1 = String::from("\tcmp\t$0, %rax\n");
            let l2 = String::from("\tmov\t$0, %rax\n");
            let l3 = String::from("\tsete\t%al\n");
            l1 + &l2 + &l3
        }
    };
    generate_expression(exp) + &l
}

fn generate_expression(exp: &Expression) -> String {
    match exp {
        Expression::ConstExpression(c) => generate_const_expression(c),
        Expression::UnaryOp(op, exp) => generate_unary_operation_expression(op, exp)
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