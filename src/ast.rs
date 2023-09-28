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
    Modulo,
}

#[derive(Clone, Debug)]
pub enum Expression{
    Assign(String, Box<Expression>), // Box needed because of recursion
    Var(String),
    ConstExpression(Const),
    UnaryOp(UnaryOp, Box<Expression>),
    BinaryOp(BinaryOp, Box<Expression>, Box<Expression>),
    Conditional(Box<Expression>, Box<Expression>, Box<Expression>),
}

pub type OptionalExpression = Option<Expression>;

#[derive(Clone, Debug)]
pub enum Statement {
    Expression(OptionalExpression),
    ReturnVal(Expression),
    Conditional(Expression, Box<Statement>, Option<Box<Statement>>),
    Compound(Vec<BlockItem>),
    For(Option<Expression>, Expression, Option<Expression>, Box<Statement>),
    ForDecl(Declaration, Expression, Option<Expression>, Box<Statement>),
    While(Expression, Box<Statement>),
    DoWhile(Box<Statement>, Expression),
    Break,
    Continue,
}

#[derive(Clone, Debug)]
pub enum Declaration {
    Declaration(String, OptionalExpression),
}

#[derive(Clone, Debug)]
pub enum BlockItem {
    Declaration(Declaration),
    Statement(Statement),
}

#[derive(Clone, Debug)]
pub enum TopLevel {
    Function {
        fun_type: TypeDef,
        name: String,
        body: Statement
    }
}

#[derive(Clone, Debug)]
pub struct Program {
    pub block: Vec<TopLevel>
}