#[derive(Clone, Debug)]
pub enum TypeDef {
    IntType
}

#[derive(Clone, Debug)]
pub enum Const {
    Int(u32)
}

#[derive(Clone, Debug)]
pub enum UnaryOp {
    Negate,
    Complement,
    Not,
}

#[derive(Clone, Debug)]
pub enum BinaryOp {
    Addition,
    Subtraction,
    Multiplication,
    Division,
    EqualTo,
    NotEqualTo,
    LogicalAnd,
    LogicalOr,
    LessThan,
    LessThanOrEqualTo,
    GreaterThan,
    GreaterThanOrEqualTo,
}

#[derive(Clone, Debug)]
pub enum Expression{
    Assign(String, Box<Expression>), // Box needed because of recursion
    Var(String),
    ConstExpression(Const),
    UnaryOp(UnaryOp, Box<Expression>),
    BinaryOp(BinaryOp, Box<Expression>, Box<Expression>),
}

#[derive(Clone, Debug)]
pub enum Statement {
    ExpressionStatement(Expression),
    ReturnVal(Expression),
    Conditional(Expression, Box<Statement>, Option<Box<Statement>>),
}

#[derive(Clone, Debug)]
pub enum BlockItem {
    Declaration(String, Option<Expression>), // Optional Expression is initialization
    Statement(Statement),
}

#[derive(Clone, Debug)]
pub enum TopLevel {
    Function {
        fun_type: TypeDef,
        name: String,
        body: Vec<BlockItem>
    }
}

#[derive(Clone, Debug)]
pub struct Program {
    pub block: Vec<TopLevel>
}