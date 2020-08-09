use num_bigint::BigInt;
use std::fmt;
use std::sync::Arc;

// Based on:
// - <https://docs.python.org/3/reference/grammar.html>
// - <https://docs.python.org/3/library/functions.html#__import__>
// - <https://docs.python.org/3/library/dis.html>
// - <https://github.com/python/cpython/blob/master/Python/ast.c>
// - <https://github.com/python/cpython/blob/3.7/Python/ast.c>
// - <https://github.com/python/cpython/blob/master/Include/Python-ast.h>
// - <https://docs.python.org/3/library/operator.html>
// - <https://docs.python.org/3/c-api/code.html>

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct Name(pub Arc<String>);
/// ```
/// use std::sync::Arc;
/// use rev_studio_lang_python::Name;
///
/// assert_eq!(Name(Arc::new("a".to_owned())).to_string(), "a");
/// ```
impl fmt::Display for Name {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{}", *self.0)
    }
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct DottedName(pub Box<[Name]>);
/// ```
/// use std::sync::Arc;
/// use rev_studio_lang_python::{DottedName, Name};
///
/// let a = Name(Arc::new("a".to_owned()));
/// let b = Name(Arc::new("b".to_owned()));
/// let c = Name(Arc::new("c".to_owned()));
/// assert_eq!(DottedName(Box::new([a.clone()])).to_string(), "a");
/// assert_eq!(DottedName(Box::new([a.clone(), b.clone()])).to_string(), "a.b");
/// assert_eq!(DottedName(Box::new([a, b, c])).to_string(), "a.b.c");
/// ```
impl fmt::Display for DottedName {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        assert!(!self.0.is_empty()); // I don't know of a use-case for an empty DottedName
        write!(f, "{}", self.0[0])?;
        for name in &self.0[1..] {
            write!(f, ".{}", name)?;
        }
        Ok(())
    }
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct WithAsClause<T> {
    pub value: T,
    pub as_clause: Option<Name>,
}
/// ```
/// use std::sync::Arc;
/// use rev_studio_lang_python::{Name, WithAsClause};
///
/// let a = Name(Arc::new("a".to_owned()));
/// let b = Name(Arc::new("b".to_owned()));
/// assert_eq!(WithAsClause { value: a.clone(), as_clause: None }.to_string(), "a");
/// assert_eq!(WithAsClause { value: a, as_clause: Some(b) }.to_string(), "a as b");
/// ```
impl<T> fmt::Display for WithAsClause<T>
where
    T: fmt::Display,
{
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{}", self.value)?;
        if let Some(name) = &self.as_clause {
            write!(f, " as {}", name)?;
        }
        Ok(())
    }
}

#[derive(Clone, Debug, PartialEq, Eq)]
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

#[derive(Clone, Debug, PartialEq, Eq)]
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
    List(Box<[Assignable]>),
    Tuple(Box<[Assignable]>),
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct ConditionedSuite {
    pub condition: Expression,
    pub body: Suite,
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct Decorator {
    pub name: DottedName,
    pub args: Option<Arguments>,
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub enum Expression {
    Assignment(Assignable, Box<Expression>),
    IfElse(Box<Expression>, Box<Expression>, Box<Expression>),
    Lambda(Box<Parameters<Name>>, Box<Expression>),
    UnaryOp(UnaryOp, Box<Expression>),
    BinaryOp(Box<Expression>, BinaryOp, Box<Expression>),
    Call(Box<Expression>, Arguments),
    Subscript(Box<Expression>, Subscript),
    Attribute(Box<Expression>, Name),
    Tuple(Box<[Expression]>),
    List(Box<[Expression]>),
    Set(Box<[Expression]>),
    Dict(Box<[DictEntry]>),
    TupleComprehension(Comprehension<IterableItem>),
    ListComprehension(Comprehension<IterableItem>),
    SetComprehension(Comprehension<IterableItem>),
    DictComprehension(Comprehension<DictItem>),
    Name(Name),
    Number(BigInt),
    String(Box<str>),
    Ellipsis,
    None,
    Bool(bool),
    Arc(Arc<Expression>),
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub enum UnaryOp {
    LogicalNot,
    Plus,
    Minus,
    BitwiseNot,
    Await,
}
/// ```
/// use rev_studio_lang_python::UnaryOp;
///
/// assert_eq!(UnaryOp::LogicalNot.to_string(), "!");
/// assert_eq!(UnaryOp::Plus.to_string(), "+");
/// assert_eq!(UnaryOp::Minus.to_string(), "-");
/// assert_eq!(UnaryOp::BitwiseNot.to_string(), "~");
/// assert_eq!(UnaryOp::Await.to_string(), "await ");
/// ```
impl fmt::Display for UnaryOp {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(
            f,
            "{}",
            match self {
                Self::LogicalNot => "!",
                Self::Plus => "+",
                Self::Minus => "-",
                Self::BitwiseNot => "~",
                Self::Await => "await ",
            }
        )
    }
}

#[derive(Clone, Debug, PartialEq, Eq)]
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
/// ```
/// use rev_studio_lang_python::BinaryOp;
///
/// assert_eq!(BinaryOp::LogicalOr.to_string(), " or ");
/// assert_eq!(BinaryOp::LogicalAnd.to_string(), " and ");
/// assert_eq!(BinaryOp::Lt.to_string(), " < ");
/// assert_eq!(BinaryOp::Le.to_string(), " <= ");
/// assert_eq!(BinaryOp::Eq.to_string(), " == ");
/// assert_eq!(BinaryOp::Ne.to_string(), " != ");
/// assert_eq!(BinaryOp::Gt.to_string(), " > ");
/// assert_eq!(BinaryOp::Ge.to_string(), " >= ");
/// assert_eq!(BinaryOp::In.to_string(), " in ");
/// assert_eq!(BinaryOp::NotIn.to_string(), " not in ");
/// assert_eq!(BinaryOp::Is.to_string(), " is ");
/// assert_eq!(BinaryOp::IsNot.to_string(), " is not ");
/// assert_eq!(BinaryOp::BitwiseOr.to_string(), " | ");
/// assert_eq!(BinaryOp::BitwiseXor.to_string(), " ^ ");
/// assert_eq!(BinaryOp::BitwiseAnd.to_string(), " & ");
/// assert_eq!(BinaryOp::LeftShift.to_string(), " << ");
/// assert_eq!(BinaryOp::RightShift.to_string(), " >> ");
/// assert_eq!(BinaryOp::Add.to_string(), " + ");
/// assert_eq!(BinaryOp::Sub.to_string(), " - ");
/// assert_eq!(BinaryOp::Mul.to_string(), " * ");
/// assert_eq!(BinaryOp::MatMul.to_string(), " @ ");
/// assert_eq!(BinaryOp::TrueDiv.to_string(), " / ");
/// assert_eq!(BinaryOp::Mod.to_string(), " % ");
/// assert_eq!(BinaryOp::FloorDiv.to_string(), " // ");
/// assert_eq!(BinaryOp::Pow.to_string(), " ** ");
/// ```
impl fmt::Display for BinaryOp {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(
            f,
            " {} ",
            match self {
                Self::LogicalOr => "or",
                Self::LogicalAnd => "and",
                Self::Lt => "<",
                Self::Le => "<=",
                Self::Eq => "==",
                Self::Ne => "!=",
                Self::Gt => ">",
                Self::Ge => ">=",
                Self::In => "in",
                Self::NotIn => "not in",
                Self::Is => "is",
                Self::IsNot => "is not",
                Self::BitwiseOr => "|",
                Self::BitwiseXor => "^",
                Self::BitwiseAnd => "&",
                Self::LeftShift => "<<",
                Self::RightShift => ">>",
                Self::Add => "+",
                Self::Sub => "-",
                Self::Mul => "*",
                Self::MatMul => "@",
                Self::TrueDiv => "/",
                Self::Mod => "%",
                Self::FloorDiv => "//",
                Self::Pow => "**",
            }
        )
    }
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub enum Subscript {
    Expression(Box<Expression>),
    Slice(Box<Expression>, Box<Expression>, Box<Expression>),
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct DictEntry {
    pub key: Box<Expression>,
    pub value: Box<Expression>,
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct Comprehension<T> {
    pub is_async: bool,
    pub entry: T,
    pub clauses: Box<[ComprehensionClause]>,
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub enum ComprehensionClause {
    For(Assignable, Box<Expression>),
    If(Box<Expression>),
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub enum IterableItem {
    Expression(Box<Expression>),
    Star(Box<Expression>),
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub enum DictItem {
    Entry(DictEntry),
    StarStar(Box<Expression>),
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct Suite(pub Box<[Statement]>);

#[derive(Clone, Debug, PartialEq, Eq)]
pub enum Statement {
    Expression(Box<Expression>),
    Assignment(Annotatable<Assignable>, Box<Expression>),
    AugAssignment(AugAssignable, Box<Expression>),
    Del(Box<[Name]>),
    Pass,
    Break,
    Continue,
    Return(Box<Expression>),
    Raise(Raise),
    Import(Import),
    Global(Box<[Name]>),
    Nonlocal(Box<[Name]>),
    Assert {
        value: Box<Expression>,
        message: Option<Box<Expression>>,
    },
    If {
        ifs: Box<[ConditionedSuite]>,
        else_: Suite,
    },
    While {
        while_: Box<ConditionedSuite>,
        else_: Suite,
    },
    For {
        is_async: bool,
        assignables: Box<[Assignable]>,
        expr: Assignable,
        body: Suite,
        else_: Suite,
    },
    Try {
        body: Suite,
        excepts: Box<[WithAsClause<Expression>]>,
        else_: Suite,
        finally_: Suite,
    },
    With {
        is_async: bool,
        items: Box<[WithAsClause<Expression>]>,
        body: Suite,
    },
    Def {
        decorators: Box<[Decorator]>,
        is_async: bool,
        name: Name,
        parameters: Box<Parameters<Annotatable<Name>>>,
        return_annotation: Option<Box<Expression>>,
        body: Suite,
    },
    Class {
        decorators: Box<[Decorator]>,
        name: Name,
        arguments: Arguments,
        body: Suite,
    },
}

#[allow(clippy::pub_enum_variant_names)]
#[derive(Clone, Debug, PartialEq, Eq)]
pub enum Raise {
    ReRaise,
    Raise(Box<Expression>),
    RaiseFrom(Box<Expression>, Box<Expression>),
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub enum Import {
    Import(Box<[WithAsClause<DottedName>]>),
    ImportFrom {
        level: usize,
        module_name: Option<DottedName>,
        names: Box<[WithAsClause<Name>]>,
    },
}
impl fmt::Display for Import {
    fn fmt(&self, f: &mut Formatter) -> fmt::Result {
        match self {
            Import(imports),
        }
    }
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct Parameters<T> {
    pub pos_only_args: Box<[T]>,
    pub pos_or_kw_args: Box<[T]>,
    pub varargs: Option<T>,
    pub kw_only_args: Box<[T]>,
    pub kwvarargs: Option<T>,
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct Annotatable<T> {
    pub name: T,
    pub annotation: Option<Box<Expression>>,
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct Arguments {
    pub positional: Box<[PositionalArgument]>,
    pub keyword: Box<[KeywordArgument]>,
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub enum PositionalArgument {
    Expression(Expression),
    Star(Expression),
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub enum KeywordArgument {
    Expression(Name, Expression),
    StarStar(Expression),
}

// trait WalkExprsMut {
// /// Call `func` on each node, in order of evaluation
// fn walk_exprs_mut(&mut self, func: impl FnMut(&mut Expression));
// }

// impl WalkExprsMut for Expression {
// fn walk_exprs_mut(&mut self, func: impl FnMut(&mut Expression)) {
// // Allow generics + capture
// macro_rules! v {
// ($($ident:ident),*) => {
// { $($ident.walk_exprs_mut(func);)* }
// }
// }

// match self {
// Self::Assignment(a, b) => v!(b, a),
// Self::IfElse(a, b, c) => todo!(),

// _ => todo!(),
// }
// func(self)
// }
// }
