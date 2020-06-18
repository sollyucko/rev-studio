use num_bigint::BigInt;
use rev_studio_arch_pvm::Instruction;
use std::sync::Arc;

#[must_use]
pub fn convert_instructions_to_ast(instructions: &[Instruction]) -> Suite {
    todo!("{:?}", instructions)
}

type BoxedSlice<T> = Box<[T]>;

// Based on:
// - <https://docs.python.org/3/reference/grammar.html>
// - <https://docs.python.org/3/library/functions.html#__import__>
// - <https://docs.python.org/3/library/dis.html>
// - <https://github.com/python/cpython/blob/master/Python/ast.c>
// - <https://github.com/python/cpython/blob/3.7/Python/ast.c>
// - <https://github.com/python/cpython/blob/master/Include/Python-ast.h>
// - <https://docs.python.org/3/library/operator.html>
// - <https://docs.python.org/3/c-api/code.html>

pub struct Name(pub Arc<String>);

pub struct DottedName(pub BoxedSlice<Name>);

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
    List(BoxedSlice<Assignable>),
    Tuple(BoxedSlice<Assignable>),
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
    Lambda(Box<Parameters<Name>>, Box<Expression>),
    UnaryOp(UnaryOp, Box<Expression>),
    BinaryOp(Box<Expression>, BinaryOp, Box<Expression>),
    Call(Box<Expression>, Arguments),
    Subscript(Box<Expression>, Subscript),
    Attribute(Box<Expression>, Name),
    Tuple(BoxedSlice<Expression>),
    List(BoxedSlice<Expression>),
    Set(BoxedSlice<Expression>),
    Dict(BoxedSlice<DictEntry>),
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
    pub clauses: BoxedSlice<ComprehensionClause>,
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

pub struct Suite(BoxedSlice<Statement>);

pub enum Statement {
    Expression(Box<Expression>),
    Assignment(Annotatable<Assignable>, Box<Expression>),
    AugAssignment(AugAssignable, Box<Expression>),
    Del(BoxedSlice<Name>),
    Pass,
    Break,
    Continue,
    Return(Box<Expression>),
    Raise(Raise),
    Import(Import),
    Global(BoxedSlice<Name>),
    Nonlocal(BoxedSlice<Name>),
    Assert {
        value: Box<Expression>,
        message: Option<Box<Expression>>,
    },
    If {
        ifs: BoxedSlice<ConditionedSuite>,
        else_: Suite,
    },
    While {
        while_: Box<ConditionedSuite>,
        else_: Suite,
    },
    For {
        is_async: bool,
        assignables: BoxedSlice<Assignable>,
        expr: Assignable,
        body: Suite,
        else_: Suite,
    },
    Try {
        body: Suite,
        excepts: BoxedSlice<WithAsClause<Expression>>,
        else_: Suite,
        finally_: Suite,
    },
    With {
        is_async: bool,
        items: BoxedSlice<WithAsClause<Expression>>,
        body: Suite,
    },
    Def {
        decorators: BoxedSlice<Decorator>,
        is_async: bool,
        name: Name,
        parameters: Parameters<Annotatable<Name>>,
        return_annotation: Option<Box<Expression>>,
        body: Suite,
    },
    Class {
        decorators: BoxedSlice<Decorator>,
        name: Name,
        arguments: Arguments,
        body: Suite,
    },
}

#[allow(clippy::pub_enum_variant_names)]
pub enum Raise {
    ReRaise,
    Raise(Box<Expression>),
    RaiseFrom(Box<Expression>, Box<Expression>),
}

pub enum Import {
    Import(BoxedSlice<WithAsClause<DottedName>>),
    ImportFrom {
        level: usize,
        module_name: Option<DottedName>,
        names: BoxedSlice<WithAsClause<Name>>,
    },
}

pub struct Parameters<T> {
    pub pos_only_args: BoxedSlice<T>,
    pub pos_or_kw_args: BoxedSlice<T>,
    pub varargs: Option<T>,
    pub kw_only_args: BoxedSlice<T>,
    pub kwvarargs: Option<T>,
}

pub struct Annotatable<T> {
    pub name: T,
    pub annotation: Option<Box<Expression>>,
}

pub struct Arguments {
    pub positional: BoxedSlice<PositionalArgument>,
    pub keyword: BoxedSlice<KeywordArgument>,
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
    use crate::*;
    use std::mem::size_of;

    #[test]
    fn print_sizes() {
        println!("Expression: {}", size_of::<Expression>());
        println!("Statement: {}", size_of::<Statement>());
        println!("Name: {}", size_of::<Name>());
        println!("BoxedSlice<Name>: {}", size_of::<Vec<Name>>());
        println!("Parameters<Name>: {}", size_of::<Parameters<Name>>());
        println!("Assignable: {}", size_of::<Assignable>());
        println!("Comprehension<DictItem>: {}", size_of::<Comprehension<DictItem>>());
        println!("Arguments: {}", size_of::<Arguments>());
    }
}
