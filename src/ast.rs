#[derive(Clone)]
#[derive(Debug)]
pub enum TypeDef {
    IntType
}

#[derive(Clone)]
#[derive(Debug)]
pub enum Const {
    Int(u32)
}

#[derive(Clone)]
#[derive(Debug)]
pub enum Expression {
    ConstExpression(Const)
}

#[derive(Clone)]
#[derive(Debug)]
pub enum Statement {
    ExpressionStatement(Expression),
    ReturnVal(Expression)
}

#[derive(Clone)]
#[derive(Debug)]
pub enum TopLevel {
    Function {
        fun_type: TypeDef,
        name: String,
        body: Vec<Statement>
    }
}

#[derive(Clone)]
#[derive(Debug)]
pub struct Program {
    pub block: Vec<TopLevel>
}