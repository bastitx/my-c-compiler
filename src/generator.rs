use crate::ast::{BinaryOp, Const, Expression, Program, Statement, TopLevel, TypeDef, UnaryOp, BlockItem, Declaration, OptionalExpression};
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
        BinaryOp::Modulo => {
            [
                String::from("\tcqo\n"), // convert doubleword
                String::from("\tidivq\t%rcx\n"),
                String::from("\tmovq\t%rdx, %rax\n")
            ]
            .join("")
        }
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

fn generate_function_call(name: &str, args: &[Expression], context: &Context) -> String {
    if !context.is_function_in_scope(name, args.len()) {
        panic!("Function {} with {} args is not in scope", name, args.len());
    }
    let args = args.into_iter().enumerate();
    let args_len = args.len();
    let stack_args = if args.len() > 6 {
        args.clone().rev().take(args_len - 6).fold(String::new(), |acc, (_, x)| {
            [
                acc,
                generate_expression(x, context),
                String::from("\tpushq\t%rax\n"),
            ].join("")
        })
    } else {
        String::new()
    };
    let stack_args_cleanup = if args.len() > 6 {
        let cleanup_len = args.len() - 6;
        format!("\taddq\t${}, %rsp\n", 8 * cleanup_len)
    } else {
        String::new()
    };
    let register_args = args.take(6).rev().fold(String::new(), |acc, (i, x)| {
        [
            acc,
            generate_expression(x, context),
            format!("\tmovq\t%rax, {}\n", parameter_index_to_register(i)),
        ].join("")
    });
    [
        stack_args,
        register_args,
        format!("\tcall\t{}\n", name),
        stack_args_cleanup
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
        Expression::FunCall(name, parameters) => generate_function_call(name, parameters, context)
    }
}

fn generate_optional_expression(exp: &OptionalExpression, context: &Context) -> String {
    if let Some(exp) = exp {
        generate_expression(exp, context)
    } else {
        String::new()
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

fn generate_declaration(decl: &Declaration, context: &Context) -> String {
    let Declaration::Declaration(var_name, exp) = decl;
    generate_var_declaration(var_name, exp, context)
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

fn generate_while_loop(exp: &Expression, statement: &Statement, context: &Context) -> String {
    let label_while = context.get_and_increase_label();
    let label_after_statement = context.get_and_increase_label();
    let label_end = context.get_and_increase_label();
    let context = Context::new_loop(context, Some(label_end.clone()), Some(label_after_statement.clone()));
    [
        format!("{}:\n", label_while),
        generate_expression(exp, &context),
        String::from("\tcmpq\t$0, %rax\n"),
        format!("\tje\t{}\n", label_end),
        generate_statement(statement, &context),
        format!("{}:\n", label_after_statement),
        format!("\tjmp\t{}\n", label_while),
        format!("{}:\n", label_end),
        generate_stack_cleanup(&context)
    ].join("")
}

fn generate_do_while_loop(statement: &Statement, exp: &Expression, context: &Context) -> String {
    let label_do = context.get_and_increase_label();
    let label_after_statement = context.get_and_increase_label();
    let label_end = context.get_and_increase_label();
    let context = Context::new_loop(context, Some(label_end.clone()), Some(label_after_statement.clone()));
    [
        format!("{}:\n", label_do),
        generate_statement(statement, &context),
        format!("{}:\n", label_after_statement),
        generate_expression(exp, &context),
        String::from("\tcmpq\t$0, %rax\n"),
        format!("\tje\t{}\n", label_end),
        format!("\tjmp\t{}\n", label_do),
        format!("{}:\n", label_end),
        generate_stack_cleanup(&context)
    ].join("")
}

fn generate_for_loop(exp1: &OptionalExpression, exp2: &Expression, exp3: &OptionalExpression, statement: &Statement, context: &Context) -> String {
    let label_for = context.get_and_increase_label();
    let label_after_statement = context.get_and_increase_label();
    let label_end = context.get_and_increase_label();
    let context = Context::new_loop(context, Some(label_end.clone()), Some(label_after_statement.clone()));
    [
        generate_optional_expression(exp1, &context),
        format!("{}:\n", label_for),
        generate_expression(exp2, &context),
        String::from("\tcmpq\t$0, %rax\n"),
        format!("\tje\t{}\n", label_end),
        generate_statement(statement, &context),
        format!("{}:\n", label_after_statement),
        generate_optional_expression(exp3, &context),
        format!("\tjmp\t{}\n", label_for),
        format!("{}:\n", label_end),
        generate_stack_cleanup(&context)
    ].join("")
}

fn generate_for_decl_loop(decl: &Declaration, exp2: &Expression, exp3: &OptionalExpression, statement: &Statement, context: &Context) -> String {
    let label_for = context.get_and_increase_label();
    let label_after_statement = context.get_and_increase_label();
    let label_end = context.get_and_increase_label();
    let context = Context::new_loop(context, Some(label_end.clone()), Some(label_after_statement.clone()));
    [
        generate_declaration(decl, &context),
        format!("{}:\n", label_for),
        generate_expression(exp2, &context),
        String::from("\tcmpq\t$0, %rax\n"),
        format!("\tje\t{}\n", label_end),
        generate_statement(statement, &context),
        format!("{}:\n", label_after_statement),
        generate_optional_expression(exp3, &context),
        format!("\tjmp\t{}\n", label_for),
        format!("{}:\n", label_end),
        generate_stack_cleanup(&context)
    ].join("")
}

fn generate_continue(context: &Context) -> String {
    if let Context { current_continue: Some(label), .. } = context{
        format!("\tjmp\t{}\n", label)
    } else {
        panic!("Illegal continue statement. Not in loop")
    }
}

fn generate_break(context: &Context) -> String {
    if let Context { current_break: Some(label), .. } = context{
        format!("\tjmp\t{}\n", label)
    } else {
        panic!("Illegal break statement. Not in loop")
    }
}

fn generate_statement(statement: &Statement, context: &Context) -> String {
    match statement {
        Statement::ReturnVal(exp) => generate_return_val(exp, context),
        Statement::Expression(exp) => generate_optional_expression(exp, context),
        Statement::Conditional(exp, if_statement, else_statement) => generate_if_else(exp, if_statement, else_statement, context),
        Statement::Compound(block_items) => {
            let context = Context::new(Some(context));
            generate_block_items(block_items, &context)
        },
        Statement::For(exp1, exp2, exp3, statement) => generate_for_loop(exp1, exp2, exp3, statement, context),
        Statement::ForDecl(decl, exp2, exp3, statement) => generate_for_decl_loop(decl, exp2, exp3, statement, context),
        Statement::Break => generate_break(context),
        Statement::Continue => generate_continue(context),
        Statement::While(exp, statement) => generate_while_loop(exp, statement, context),
        Statement::DoWhile(statement, exp) => generate_do_while_loop(statement, exp, context),
    }
}

fn generate_block_item(block_item: &BlockItem, context: &Context) -> String {
    match block_item {
        BlockItem::Declaration(Declaration::Declaration(name, exp)) => generate_var_declaration(name, exp, context),
        BlockItem::Statement(statement) => generate_statement(statement, context)
    }
}

fn generate_stack_cleanup(context: &Context) -> String {
    let bytes_to_deallocate = 8 * context.get_current_scope_size();
    format!("\taddq\t${}, %rsp\n", bytes_to_deallocate)
}

fn generate_block_items(block_items: &[BlockItem], context: &Context) -> String {
    block_items.into_iter().fold(String::new(), |acc, x| {
        acc + &generate_block_item(&x, &context)
    }) + &generate_stack_cleanup(&context)
}

fn generate_function_body(body: &[BlockItem], context: &Context) -> String {
    // TODO remove the generate return val? It was added so that we return 0 when there is no return statement
    generate_block_items(body, context) + &generate_return_val(&Expression::ConstExpression(Const::Int(0)), context)
}

const fn parameter_index_to_register(i: usize) -> &'static str {
    match i {
        0 => "%rdi",
        1 => "%rsi",
        2 => "%rdx",
        3 => "%rcx",
        4 => "%r8",
        5 => "%r9",
        _ => panic!("Parameters at position 7 and above are on the stack")
    }
}

fn generate_parameter_declarations(parameters: &[String], context: &Context) -> String {
    if parameters.len() > 6 {
        parameters[6..].into_iter().enumerate().for_each(|(i, x)| {
            context.declare_var_with_offset(x, 16 + (i as i32) * 8);
        });
    }
    parameters.into_iter().take(6).enumerate().fold(String::new(), |acc, (i, x)| {
        context.declare_var(x);
        acc + &format!("\tpushq\t{}\n", parameter_index_to_register(i))
    })
}

fn generate_function(
    _: &TypeDef,
    name: &String,
    parameters: &[String],
    body: &Option<Vec<BlockItem>>,
    context: &Context,
) -> String {
    if let Some(body) = body {
        context.define_function(name, parameters.len());
        let context = Context::new_function(context);
        [
            format!("\t.global\t{}\n", name),
            format!("{}:\n", name),
            String::from("\tpushq\t%rbp\n"),
            String::from("\tmovq\t%rsp, %rbp\n"),
            generate_parameter_declarations(parameters, &context),
            generate_function_body(body, &context),
        ]
        .join("")
    } else {
        context.declare_function(name, parameters.len());
        String::new()
    }
}

fn generate_top_level(tl: &TopLevel, context: &Context) -> String {
    match tl {
        TopLevel::Function {
            fun_type,
            name,
            parameters,
            body
        } => generate_function(fun_type, name, parameters, body, context),
    }
}

fn generate_top_levels(block: Vec<TopLevel>, context: &Context) -> String {
    block
        .into_iter()
        .fold(String::new(), |acc, x| acc + &generate_top_level(&x, context))
}

pub fn generate(program: Program) -> String {
    let context = Context::new(None);
    [
        format!("\t.text\n"),
        generate_top_levels(program.block, &context),
    ].join("")
}
