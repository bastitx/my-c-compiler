use crate::ast::{BinaryOp, Const, Expression, Program, Statement, TopLevel, TypeDef, UnaryOp, BlockItem};
use context::Context;

pub mod context;

fn generate_const_expression(c: &Const) -> String {
    match c {
        Const::Int(i) => format!("\tmovq\t${}, %rax\n", i),
    }
}

fn generate_unary_operation_expression(
    op: &UnaryOp,
    exp: &Expression,
    context: &Context,
) -> String {
    let op = match op {
        UnaryOp::Negate => String::from("\tneg\t%rax\n"),
        UnaryOp::Complement => String::from("\tnot\t%rax\n"),
        UnaryOp::Not => {
            let l1 = String::from("\tcmpq\t$0, %rax\n");
            let l2 = String::from("\tmovq\t$0, %rax\n");
            let l3 = String::from("\tsete\t%al\n");
            l1 + &l2 + &l3
        }
    };
    generate_expression(exp, context) + &op
}

fn generate_comparison(set_instruction: &str) -> String {
    let l1 = String::from("\tcmpq\t%rcx, %rax\n");
    let l2 = String::from("\tmovq\t$0, %rax\n");
    let l3 = format!("\t{}\t%al\n", set_instruction);
    l1 + &l2 + &l3
}

fn generate_binary_operation_or(exp1: &Expression, exp2: &Expression, context: &Context) -> String {
    let clause_2_label = context.get_and_increase_label();
    let end_label = context.get_and_increase_label();
    [
        generate_expression(exp1, context),
        String::from("\tcmpq\t$0, %rax\n"),
        format!("\tje\t{}\n", clause_2_label),
        String::from("\tmovq\t$1, %rax\n"),
        format!("\tjmp\t{}\n", end_label),
        format!("{}:\n", clause_2_label),
        generate_expression(exp2, context),
        String::from("\tcmpq\t$0, %rax\n"),
        String::from("\tmovq\t$0, %rax\n"),
        String::from("\tsetne\t%al\n"),
        format!("{}:\n", end_label),
    ]
    .join("")
}

fn generate_binary_operation_and(
    exp1: &Expression,
    exp2: &Expression,
    context: &Context,
) -> String {
    let clause_2_label = context.get_and_increase_label();
    let end_label = context.get_and_increase_label();
    [
        generate_expression(exp1, context),
        String::from("\tcmpq\t$0, %rax\n"),
        format!("\tjne\t{}\n", clause_2_label),
        format!("\tjmp\t{}\n", end_label),
        format!("{}:\n", clause_2_label),
        generate_expression(exp2, context),
        String::from("\tcmpq\t$0, %rax\n"),
        String::from("\tmovq\t$0, %rax\n"),
        String::from("\tsetne\t%al\n"),
        format!("{}:\n", end_label),
    ]
    .join("")
}

fn generate_binary_operation_expression(
    op: &BinaryOp,
    exp1: &Expression,
    exp2: &Expression,
    context: &Context,
) -> String {
    let gen = [
        generate_expression(exp2, context),
        String::from("\tpushq\t%rax\n"),
        generate_expression(exp1, context),
        String::from("\tpopq\t%rcx\n"),
    ]
    .join("");
    let operation = match op {
        BinaryOp::Addition => String::from("\taddq\t%rcx, %rax\n"),
        BinaryOp::Multiplication => String::from("\timulq\t%rcx, %rax\n"),
        BinaryOp::Subtraction => String::from("\tsubq\t%rcx, %rax\n"),
        BinaryOp::Division => {
            [
                String::from("\tcqo\n"), // convert doubleword
                String::from("\tidivq\t%rcx\n"),
            ]
            .join("")
        }
        BinaryOp::EqualTo => generate_comparison("sete"),
        BinaryOp::NotEqualTo => generate_comparison("setne"),
        BinaryOp::GreaterThan => generate_comparison("setg"),
        BinaryOp::GreaterThanOrEqualTo => generate_comparison("setge"),
        BinaryOp::LessThan => generate_comparison("setl"),
        BinaryOp::LessThanOrEqualTo => generate_comparison("setle"),
        _ => panic!("Unsupported binary operation"),
    };
    gen + &operation
}

fn generate_var_assignment(var_name: &str, exp: &Expression, context: &Context) -> String {
    let offset = context.get_var_stack_index(var_name);
    [generate_expression(exp, context), format!("\tmovq\t%rax, {}(%rbp)\n", offset)].join("")
}

fn generate_var_reference(var_name: &str, context: &Context) -> String {
    let offset = context.get_var_stack_index(var_name);
    format!("\tmovq\t{}(%rbp), %rax\n", offset)
}

fn generate_conditional_expression(exp1: &Expression, exp2: &Expression, exp3: &Expression, context: &Context) -> String {
    let label_else = context.get_and_increase_label();
    let label_end = context.get_and_increase_label();
    [
        generate_expression(exp1, context),
        String::from("\tcmpq\t$0, %rax\n"),
        format!("\tje\t{}\n", label_else),
        generate_expression(exp2, context),
        format!("\tjmp\t{}\n", label_end),
        format!("{}:\n", label_else),
        generate_expression(exp3, context),
        format!("{}:\n", label_end),
    ].join("")
}

fn generate_expression(exp: &Expression, context: &Context) -> String {
    match exp {
        Expression::ConstExpression(c) => generate_const_expression(c),
        Expression::UnaryOp(op, exp) => generate_unary_operation_expression(op, exp, context),
        Expression::BinaryOp(BinaryOp::LogicalOr, exp1, exp2) => {
            generate_binary_operation_or(exp1, exp2, context)
        }
        Expression::BinaryOp(BinaryOp::LogicalAnd, exp1, exp2) => {
            generate_binary_operation_and(exp1, exp2, context)
        }
        Expression::BinaryOp(op, exp1, exp2) => {
            generate_binary_operation_expression(op, exp1, exp2, context)
        },
        Expression::Assign(name, exp) => generate_var_assignment(name, exp, context),
        Expression::Var(name) => generate_var_reference(name, context),
        Expression::Conditional(exp1, exp2, exp3) => generate_conditional_expression(exp1, exp2, exp3, context),
    }
}

fn generate_return_val(exp: &Expression, context: &Context) -> String {
    [
        generate_expression(exp, context),
        String::from("\tmovq\t%rbp, %rsp\n"),
        String::from("\tpopq\t%rbp\n"),
        String::from("\tret\n"),
    ]
    .join("")
}

fn generate_var_declaration(var_name: &str, exp: &Option<Expression>, context: &Context) -> String {
    context.declare_var(var_name);
    let init = if let Some(exp) = exp {
        generate_expression(exp, context)
    } else {
        String::from("\tmovq\t$0, %rax\n")
    };
    [init, String::from("\tpushq\t%rax\n")].join("")
}

fn generate_if_else(exp: &Expression, if_statement: &Box<Statement>, else_statement: &Option<Box<Statement>>, context: &Context) -> String {
    let label_else = context.get_and_increase_label();
    let label_end = context.get_and_increase_label();
    [
        generate_expression(exp, context),
        String::from("\tcmpq\t$0, %rax\n"),
        format!("\tje\t{}\n", label_else),
        generate_statement(if_statement, context),
        format!("\tjmp\t{}\n", label_end),
        format!("{}:\n", label_else),
        else_statement.as_ref().and_then(|x| Some(generate_statement(x, context))).unwrap_or(String::new()),
        format!("{}:\n", label_end),
    ].join("")
}

fn generate_statement(statement: &Statement, context: &Context) -> String {
    match statement {
        Statement::ReturnVal(exp) => generate_return_val(exp, context),
        Statement::ExpressionStatement(exp) => generate_expression(exp, context),
        Statement::Conditional(exp, if_statement, else_statement) => generate_if_else(exp, if_statement, else_statement, context)
    }
}

fn generate_block_item(block_item: &BlockItem, context: &Context) -> String {
    match block_item {
        BlockItem::Declaration(name, exp) => generate_var_declaration(name, exp, context),
        BlockItem::Statement(statement) => generate_statement(statement, context)
    }
}

fn generate_function_body(body: &[BlockItem], context: &Context) -> String {
    let body = if let Some(BlockItem::Statement(Statement::ReturnVal(_))) = body.last() {
        body.to_vec()
    } else {
        [body, &[BlockItem::Statement(Statement::ReturnVal(Expression::ConstExpression(Const::Int(0))))]].concat()
    };
    body.into_iter().fold(String::new(), |acc, x| {
        acc + &generate_block_item(&x, context)
    })
}

fn generate_function(
    _: &TypeDef,
    name: &String,
    body: &Vec<BlockItem>,
    context: &Context,
) -> String {
    [
        format!("\t.global\t{}\n", name),
        format!("\t.text\n"),
        format!("{}:\n", name),
        String::from("\tpush\t%rbp\n"),
        String::from("\tmovq\t%rsp, %rbp\n"),
        generate_function_body(body, context),
    ]
    .join("")
}

fn generate_block(tl: &TopLevel, context: &Context) -> String {
    match tl {
        TopLevel::Function {
            fun_type,
            name,
            body,
        } => generate_function(fun_type, name, body, context),
    }
}

fn generate_blocks(block: Vec<TopLevel>, context: &Context) -> String {
    block
        .into_iter()
        .fold(String::new(), |acc, x| acc + &generate_block(&x, context))
}

pub fn generate(program: Program) -> String {
    let context = Context::new();
    generate_blocks(program.block, &context)
}
