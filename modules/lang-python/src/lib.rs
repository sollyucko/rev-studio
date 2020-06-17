use num_bigint::BigInt;
use rev_studio_arch_pvm::Instruction;

#[must_use]
pub fn convert_instructions_to_ast(instructions: &[Instruction]) -> Suite {
    todo!("{:?}", instructions)
}

// Based on:
// - <https://docs.python.org/3/reference/grammar.html>
// - <https://docs.python.org/3/library/functions.html#__import__>
// - <https://docs.python.org/3/library/dis.html>
// - <https://github.com/python/cpython/blob/master/Python/ast.c>
// - <https://github.com/python/cpython/blob/3.7/Python/ast.c>
// - <https://github.com/python/cpython/blob/master/Include/Python-ast.h>
// - <https://docs.python.org/3/library/operator.html>
// - <https://docs.python.org/3/c-api/code.html>

pub struct Name(pub String);

pub struct DottedName(pub Vec<Name>);

pub struct WithAsClause<T> {
    pub value: T,
    pub as_clause: Option<Name>,
}

pub enum AugAssignable {
    Name(Name),
    Subscript {
        expr: Box<Expression>,
        subscript: Box<Expression>,
    },
    Attribute {
        expr: Box<Expression>,
        name: Name,
    },
}

pub enum Assignable {
    Name(Name),
    Subscript {
        expr: Box<Expression>,
        subscript: Box<Expression>,
    },
    Attribute {
        expr: Box<Expression>,
        name: Name,
    },
    Starred(Box<Expression>),
    List(Vec<Assignable>),
    Tuple(Vec<Assignable>),
}

pub struct ConditionedSuite {
    pub condition: Expression,
    pub body: Suite,
}

pub struct Decorator {
    pub name: DottedName,
    pub args: Option<Arguments>,
}

pub enum Expression {
    Assignment(Assignable, Box<Expression>),
    IfElse(Box<Expression>, Box<Expression>, Box<Expression>),
    Lambda(Parameters<Name>, Box<Expression>),
    UnaryOp(UnaryOp, Box<Expression>),
    BinaryOp(Box<Expression>, BinaryOp, Box<Expression>),
    Call(Box<Expression>, Arguments),
    Subscript(Box<Expression>, Subscript),
    Attribute(Box<Expression>, Name),
    Tuple(Vec<Expression>),
    List(Vec<Expression>),
    Set(Vec<Expression>),
    Dict(Vec<DictEntry>),
    TupleComprehension(Comprehension<IterableItem>),
    ListComprehension(Comprehension<IterableItem>),
    SetComprehension(Comprehension<IterableItem>),
    DictComprehension(Comprehension<DictItem>),
    Name(Name),
    Number(BigInt),
    String(String),
    Ellipsis,
    None,
    Bool(bool),
}

pub enum UnaryOp {
    LogicalNot,
    Plus,
    Minus,
    BitwiseNot,
    Await,
}

pub enum BinaryOp {
    LogicalOr,
    LogicalAnd,
    Lt,
    Le,
    Eq,
    Ne,
    Gt,
    Ge,
    In,
    NotIn,
    Is,
    IsNot,
    BitwiseOr,
    BitwiseXor,
    BitwiseAnd,
    LeftShift,
    RightShift,
    Add,
    Sub,
    Mul,
    MatMul,
    TrueDiv,
    Mod,
    FloorDiv,
    Pow,
}

pub enum Subscript {
    Expression(Box<Expression>),
    Slice(Box<Expression>, Box<Expression>, Box<Expression>),
}

pub struct DictEntry {
    pub key: Box<Expression>,
    pub value: Box<Expression>,
}

pub struct Comprehension<T> {
    pub is_async: bool,
    pub entry: T,
    pub clauses: Vec<ComprehensionClause>,
}

pub enum ComprehensionClause {
    For(Assignable, Box<Expression>),
    If(Box<Expression>),
}

pub enum IterableItem {
    Expression(Box<Expression>),
    Star(Box<Expression>),
}

pub enum DictItem {
    Entry(DictEntry),
    StarStar(Box<Expression>),
}

pub struct Suite(Vec<Statement>);

pub enum Statement {
    Expression(Expression),
    Assignment(Annotatable<Assignable>, Expression),
    AugAssignment(AugAssignable, Expression),
    Del(Vec<Name>),
    Pass,
    Break,
    Continue,
    Return(Expression),
    Raise(Raise),
    Import(Import),
    Global(Vec<Name>),
    Nonlocal(Vec<Name>),
    Assert {
        value: Expression,
        message: Option<Box<Expression>>,
    },
    If {
        ifs: Vec<ConditionedSuite>,
        else_: Suite,
    },
    While {
        while_: ConditionedSuite,
        else_: Suite,
    },
    For {
        is_async: bool,
        assignables: Vec<Assignable>,
        expr: Assignable,
        body: Suite,
        else_: Suite,
    },
    Try {
        body: Suite,
        excepts: Vec<WithAsClause<Expression>>,
        else_: Suite,
        finally_: Suite,
    },
    With {
        is_async: bool,
        items: Vec<WithAsClause<Expression>>,
        body: Suite,
    },
    Def {
        decorators: Vec<Decorator>,
        is_async: bool,
        name: Name,
        parameters: Parameters<Annotatable<Name>>,
        return_annotation: Option<Box<Expression>>,
        body: Suite,
    },
    Class {
        decorators: Vec<Decorator>,
        name: Name,
        arguments: Arguments,
        body: Suite,
    },
}

#[allow(clippy::pub_enum_variant_names)]
pub enum Raise {
    ReRaise,
    Raise(Expression),
    RaiseFrom(Expression, Expression),
}

pub enum Import {
    Import(Vec<WithAsClause<DottedName>>),
    ImportFrom {
        level: usize,
        module_name: Option<DottedName>,
        names: Vec<WithAsClause<Name>>,
    },
}

pub struct Parameters<T> {
    pub pos_only_args: Vec<T>,
    pub pos_or_kw_args: Vec<T>,
    pub varargs: Option<T>,
    pub kw_only_args: Vec<T>,
    pub kwvarargs: Option<T>,
}

pub struct Annotatable<T> {
    pub name: T,
    pub annotation: Option<Box<Expression>>, // this doesn't need to be boxed, but it saves a ton of space when not used
}

pub struct Arguments {
    pub positional: Vec<PositionalArgument>,
    pub keyword: Vec<KeywordArgument>,
}

pub enum PositionalArgument {
    Expression(Expression),
    Star(Expression),
}

pub enum KeywordArgument {
    Expression(Name, Expression),
    StarStar(Expression),
}

#[cfg(test)]
mod tests {
    #[test]
    fn it_works() {
        assert_eq!(2 + 2, 4);
    }
}
