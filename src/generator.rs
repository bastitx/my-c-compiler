use crate::ast::{Program, TopLevel, TypeDef, Statement, Expression, Const};

fn generate_return_val(exp: &Expression) -> String {
    match exp {
        Expression::ConstExpression(Const::Int(i)) => {
            let l1 = format!("\tmovl\t${}, %eax\n", i);
            let l2 = format!("\tret\n");
            l1 + &l2
        }
    }
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