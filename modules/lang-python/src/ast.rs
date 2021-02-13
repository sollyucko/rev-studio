use fmt_utils::{Repeated, Separated};
use num_bigint::BigInt;
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
/// use fmt_utils::fmt_to_cleared_string;
/// use rev_studio_lang_python::Name;
///
/// let mut buf = String::new();
///
/// assert_eq!(fmt_to_cleared_string(&mut buf, Name(Arc::new("a".to_owned())), &fast_fmt::Display), "a");
/// ```
impl fast_fmt::Fmt<fast_fmt::Display> for Name {
    fn fmt<W: fast_fmt::Write>(
        &self,
        writer: &mut W,
        strategy: &fast_fmt::Display,
    ) -> Result<(), W::Error> {
        self.0.fmt(writer, strategy)
    }

    fn size_hint(&self, strategy: &fast_fmt::Display) -> usize {
        self.0.size_hint(strategy)
    }
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct DottedName(pub Box<[Name]>);
/// ```
/// use std::sync::Arc;
/// use fmt_utils::fmt_to_cleared_string;
/// use rev_studio_lang_python::{DottedName, Name};
///
/// let mut buf = String::new();
///
/// let a = Name(Arc::new("a".to_owned()));
/// let b = Name(Arc::new("b".to_owned()));
/// let c = Name(Arc::new("c".to_owned()));
/// assert_eq!(fmt_to_cleared_string(&mut buf, DottedName(Box::new([a.clone()])), &fast_fmt::Display), "a");
/// assert_eq!(fmt_to_cleared_string(&mut buf, DottedName(Box::new([a.clone(), b.clone()])), &fast_fmt::Display), "a.b");
/// assert_eq!(fmt_to_cleared_string(&mut buf, DottedName(Box::new([a, b, c])), &fast_fmt::Display), "a.b.c");
/// ```
impl fast_fmt::Fmt<fast_fmt::Display> for DottedName {
    fn fmt<W: fast_fmt::Write>(
        &self,
        writer: &mut W,
        strategy: &fast_fmt::Display,
    ) -> Result<(), W::Error> {
        Separated {
            sep: '.',
            iter: &*self.0,
        }
        .fmt(writer, strategy)
    }

    fn size_hint(&self, strategy: &fast_fmt::Display) -> usize {
        Separated {
            sep: '.',
            iter: &*self.0,
        }
        .size_hint(strategy)
    }
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct WithAsClause<T> {
    pub value: T,
    pub as_clause: Option<Name>,
}
/// ```
/// use std::sync::Arc;
/// use fmt_utils::fmt_to_cleared_string;
/// use rev_studio_lang_python::{Name, WithAsClause};
///
/// let mut buf = String::new();
///
/// let a = Name(Arc::new("a".to_owned()));
/// let b = Name(Arc::new("b".to_owned()));
/// assert_eq!(fmt_to_cleared_string(&mut buf, WithAsClause { value: a.clone(), as_clause: None }, &fast_fmt::Display), "a");
/// assert_eq!(fmt_to_cleared_string(&mut buf, WithAsClause { value: a, as_clause: Some(b) }, &fast_fmt::Display), "a as b");
/// ```
impl<T> fast_fmt::Fmt<fast_fmt::Display> for WithAsClause<T>
where
    T: fast_fmt::Fmt<fast_fmt::Display>,
{
    fn fmt<W: fast_fmt::Write>(
        &self,
        writer: &mut W,
        strategy: &fast_fmt::Display,
    ) -> Result<(), W::Error> {
        self.value.fmt(writer, strategy)?;
        if let Some(name) = &self.as_clause {
            writer.write_str(" as ")?;
            name.fmt(writer, strategy)?;
        }
        Ok(())
    }

    fn size_hint(&self, strategy: &fast_fmt::Display) -> usize {
        self.value.size_hint(strategy)
            + if let Some(name) = &self.as_clause {
                4 + name.0.len()
            } else {
                0
            }
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

#[derive(Copy, Clone, Debug, PartialEq, Eq)]
pub enum UnaryOp {
    LogicalNot,
    Plus,
    Minus,
    BitwiseNot,
    Await,
}
/// ```
/// use fmt_utils::fmt_to_cleared_string;
/// use rev_studio_lang_python::UnaryOp;
///
/// let mut buf = String::new();
///
/// assert_eq!(fmt_to_cleared_string(&mut buf, UnaryOp::LogicalNot, &fast_fmt::Display), "!");
/// assert_eq!(fmt_to_cleared_string(&mut buf, UnaryOp::Plus, &fast_fmt::Display), "+");
/// assert_eq!(fmt_to_cleared_string(&mut buf, UnaryOp::Minus, &fast_fmt::Display), "-");
/// assert_eq!(fmt_to_cleared_string(&mut buf, UnaryOp::BitwiseNot, &fast_fmt::Display), "~");
/// assert_eq!(fmt_to_cleared_string(&mut buf, UnaryOp::Await, &fast_fmt::Display), "await ");
/// ```
impl fast_fmt::Fmt<fast_fmt::Display> for UnaryOp {
    fn fmt<W: fast_fmt::Write>(
        &self,
        writer: &mut W,
        _strategy: &fast_fmt::Display,
    ) -> Result<(), W::Error> {
        writer.write_str(self.to_str())
    }

    fn size_hint(&self, _strategy: &fast_fmt::Display) -> usize {
        self.to_str().len()
    }
}
impl UnaryOp {
    const fn to_str(self) -> &'static str {
        match self {
            Self::LogicalNot => "!",
            Self::Plus => "+",
            Self::Minus => "-",
            Self::BitwiseNot => "~",
            Self::Await => "await ",
        }
    }
}

#[derive(Copy, Clone, Debug, PartialEq, Eq)]
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
/// use fmt_utils::fmt_to_cleared_string;
/// use rev_studio_lang_python::BinaryOp;
///
/// let mut buf = String::new();
///
/// assert_eq!(fmt_to_cleared_string(&mut buf, BinaryOp::LogicalOr, &fast_fmt::Display), " or ");
/// assert_eq!(fmt_to_cleared_string(&mut buf, BinaryOp::LogicalAnd, &fast_fmt::Display), " and ");
/// assert_eq!(fmt_to_cleared_string(&mut buf, BinaryOp::Lt, &fast_fmt::Display), " < ");
/// assert_eq!(fmt_to_cleared_string(&mut buf, BinaryOp::Le, &fast_fmt::Display), " <= ");
/// assert_eq!(fmt_to_cleared_string(&mut buf, BinaryOp::Eq, &fast_fmt::Display), " == ");
/// assert_eq!(fmt_to_cleared_string(&mut buf, BinaryOp::Ne, &fast_fmt::Display), " != ");
/// assert_eq!(fmt_to_cleared_string(&mut buf, BinaryOp::Gt, &fast_fmt::Display), " > ");
/// assert_eq!(fmt_to_cleared_string(&mut buf, BinaryOp::Ge, &fast_fmt::Display), " >= ");
/// assert_eq!(fmt_to_cleared_string(&mut buf, BinaryOp::In, &fast_fmt::Display), " in ");
/// assert_eq!(fmt_to_cleared_string(&mut buf, BinaryOp::NotIn, &fast_fmt::Display), " not in ");
/// assert_eq!(fmt_to_cleared_string(&mut buf, BinaryOp::Is, &fast_fmt::Display), " is ");
/// assert_eq!(fmt_to_cleared_string(&mut buf, BinaryOp::IsNot, &fast_fmt::Display), " is not ");
/// assert_eq!(fmt_to_cleared_string(&mut buf, BinaryOp::BitwiseOr, &fast_fmt::Display), " | ");
/// assert_eq!(fmt_to_cleared_string(&mut buf, BinaryOp::BitwiseXor, &fast_fmt::Display), " ^ ");
/// assert_eq!(fmt_to_cleared_string(&mut buf, BinaryOp::BitwiseAnd, &fast_fmt::Display), " & ");
/// assert_eq!(fmt_to_cleared_string(&mut buf, BinaryOp::LeftShift, &fast_fmt::Display), " << ");
/// assert_eq!(fmt_to_cleared_string(&mut buf, BinaryOp::RightShift, &fast_fmt::Display), " >> ");
/// assert_eq!(fmt_to_cleared_string(&mut buf, BinaryOp::Add, &fast_fmt::Display), " + ");
/// assert_eq!(fmt_to_cleared_string(&mut buf, BinaryOp::Sub, &fast_fmt::Display), " - ");
/// assert_eq!(fmt_to_cleared_string(&mut buf, BinaryOp::Mul, &fast_fmt::Display), " * ");
/// assert_eq!(fmt_to_cleared_string(&mut buf, BinaryOp::MatMul, &fast_fmt::Display), " @ ");
/// assert_eq!(fmt_to_cleared_string(&mut buf, BinaryOp::TrueDiv, &fast_fmt::Display), " / ");
/// assert_eq!(fmt_to_cleared_string(&mut buf, BinaryOp::Mod, &fast_fmt::Display), " % ");
/// assert_eq!(fmt_to_cleared_string(&mut buf, BinaryOp::FloorDiv, &fast_fmt::Display), " // ");
/// assert_eq!(fmt_to_cleared_string(&mut buf, BinaryOp::Pow, &fast_fmt::Display), " ** ");
/// ```
impl fast_fmt::Fmt<fast_fmt::Display> for BinaryOp {
    fn fmt<W: fast_fmt::Write>(
        &self,
        writer: &mut W,
        _strategy: &fast_fmt::Display,
    ) -> Result<(), W::Error> {
        writer.write_str(self.to_str())
    }

    fn size_hint(&self, _strategy: &fast_fmt::Display) -> usize {
        self.to_str().len()
    }
}
impl BinaryOp {
    const fn to_str(self) -> &'static str {
        match self {
            Self::LogicalOr => " or ",
            Self::LogicalAnd => " and ",
            Self::Lt => " < ",
            Self::Le => " <= ",
            Self::Eq => " == ",
            Self::Ne => " != ",
            Self::Gt => " > ",
            Self::Ge => " >= ",
            Self::In => " in ",
            Self::NotIn => " not in ",
            Self::Is => " is ",
            Self::IsNot => " is not ",
            Self::BitwiseOr => " | ",
            Self::BitwiseXor => " ^ ",
            Self::BitwiseAnd => " & ",
            Self::LeftShift => " << ",
            Self::RightShift => " >> ",
            Self::Add => " + ",
            Self::Sub => " - ",
            Self::Mul => " * ",
            Self::MatMul => " @ ",
            Self::TrueDiv => " / ",
            Self::Mod => " % ",
            Self::FloorDiv => " // ",
            Self::Pow => " ** ",
        }
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
pub struct ImportList(pub Box<[WithAsClause<DottedName>]>);
/// ```
/// use std::sync::Arc;
/// use fmt_utils::fmt_to_cleared_string;
/// use rev_studio_lang_python::{DottedName, ImportList, Name, WithAsClause};
///
/// let mut buf = String::new();
///
/// assert_eq!(fmt_to_cleared_string(&mut buf, ImportList(Box::new([
///     WithAsClause { value: DottedName(Box::new([Name(Arc::new(String::from("abc")))])), as_clause: None},
///     WithAsClause { value: DottedName(Box::new([Name(Arc::new(String::from("de")))])), as_clause: Some(Name(Arc::new(String::from("f"))))},
/// ])), &fast_fmt::Display), "abc, de as f");
/// ```
impl fast_fmt::Fmt<fast_fmt::Display> for ImportList {
    fn fmt<W: fast_fmt::Write>(
        &self,
        writer: &mut W,
        strategy: &fast_fmt::Display,
    ) -> Result<(), W::Error> {
        Separated {
            sep: ", ",
            iter: &*self.0,
        }
        .fmt(writer, strategy)
    }

    fn size_hint(&self, strategy: &fast_fmt::Display) -> usize {
        Separated {
            sep: ", ",
            iter: &*self.0,
        }
        .size_hint(strategy)
    }
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub enum ImportListOrStar {
    Star,
    Names(ImportList),
}
/// ```
/// use std::sync::Arc;
/// use fmt_utils::fmt_to_cleared_string;
/// use rev_studio_lang_python::{DottedName, ImportList, ImportListOrStar, Name, WithAsClause};
///
/// let mut buf = String::new();
///
/// assert_eq!(fmt_to_cleared_string(&mut buf, ImportListOrStar::Star, &fast_fmt::Display), "*");
/// assert_eq!(fmt_to_cleared_string(&mut buf, ImportListOrStar::Names(ImportList(Box::new([
///     WithAsClause { value: DottedName(Box::new([Name(Arc::new(String::from("abc")))])), as_clause: None},
///     WithAsClause { value: DottedName(Box::new([Name(Arc::new(String::from("de")))])), as_clause: Some(Name(Arc::new(String::from("f"))))},
/// ]))), &fast_fmt::Display), "abc, de as f");
/// ```
impl fast_fmt::Fmt<fast_fmt::Display> for ImportListOrStar {
    fn fmt<W: fast_fmt::Write>(
        &self,
        writer: &mut W,
        strategy: &fast_fmt::Display,
    ) -> Result<(), W::Error> {
        match self {
            Self::Star => writer.write_char('*'),
            Self::Names(names) => names.fmt(writer, strategy),
        }
    }

    fn size_hint(&self, strategy: &fast_fmt::Display) -> usize {
        match self {
            Self::Star => 1,
            Self::Names(names) => names.size_hint(strategy),
        }
    }
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub enum Import {
    Import(ImportList),
    ImportFrom {
        level: usize,
        module_name: Option<DottedName>,
        names: ImportListOrStar,
    },
}
/// ```
/// use std::sync::Arc;
/// use fmt_utils::fmt_to_cleared_string;
/// use rev_studio_lang_python::{DottedName, Import, ImportList, ImportListOrStar, Name, WithAsClause};
///
/// let mut buf = String::new();
///
/// assert_eq!(fmt_to_cleared_string(&mut buf, Import::Import(ImportList(Box::new([WithAsClause { value: DottedName(Box::new([Name(Arc::new(String::from("foo")))])), as_clause: None}]))), &fast_fmt::Display), "import foo");
/// assert_eq!(fmt_to_cleared_string(&mut buf, Import::ImportFrom {
///     level: 0,
///     module_name: Some(DottedName(Box::new([Name(Arc::new(String::from("foo")))]))),
///     names: ImportListOrStar::Star,
/// }, &fast_fmt::Display), "from foo import *");
/// assert_eq!(fmt_to_cleared_string(&mut buf, Import::ImportFrom {
///     level: 1,
///     module_name: Some(DottedName(Box::new([Name(Arc::new(String::from("foo")))]))),
///     names: ImportListOrStar::Star,
/// }, &fast_fmt::Display), "from .foo import *");
/// assert_eq!(fmt_to_cleared_string(&mut buf, Import::ImportFrom {
///     level: 2,
///     module_name: Some(DottedName(Box::new([Name(Arc::new(String::from("foo")))]))),
///     names: ImportListOrStar::Star,
/// }, &fast_fmt::Display), "from ..foo import *");
/// assert_eq!(fmt_to_cleared_string(&mut buf, Import::ImportFrom {
///     level: 1,
///     module_name: None,
///     names: ImportListOrStar::Star,
/// }, &fast_fmt::Display), "from . import *");
/// assert_eq!(fmt_to_cleared_string(&mut buf, Import::ImportFrom {
///     level: 2,
///     module_name: None,
///     names: ImportListOrStar::Star,
/// }, &fast_fmt::Display), "from .. import *");
/// ```
impl fast_fmt::Fmt<fast_fmt::Display> for Import {
    fn fmt<W: fast_fmt::Write>(
        &self,
        writer: &mut W,
        strategy: &fast_fmt::Display,
    ) -> Result<(), W::Error> {
        match self {
            Self::Import(names) => {
                writer.write_str("import ")?;
                names.fmt(writer, strategy)?;
            }
            Self::ImportFrom {
                level,
                module_name: Some(module_name),
                names,
            } => {
                writer.write_str("from ")?;
                Repeated {
                    value: '.',
                    count: *level,
                }
                .fmt(writer, strategy)?;
                module_name.fmt(writer, strategy)?;
                writer.write_str(" import ")?;
                names.fmt(writer, strategy)?;
            }
            Self::ImportFrom {
                level,
                module_name: None,
                names,
            } => {
                writer.write_str("from ")?;
                Repeated {
                    value: '.',
                    count: *level,
                }
                .fmt(writer, strategy)?;
                writer.write_str(" import ")?;
                names.fmt(writer, strategy)?;
            }
        }
        Ok(())
    }

    fn size_hint(&self, strategy: &fast_fmt::Display) -> usize {
        match self {
            Self::Import(names) => 7 + names.size_hint(strategy),
            Self::ImportFrom {
                level,
                module_name: Some(module_name),
                names,
            } => 5 + *level + module_name.size_hint(strategy) + 8 + names.size_hint(strategy),
            Self::ImportFrom {
                level,
                module_name: None,
                names,
            } => 5 + *level + 8 + names.size_hint(strategy),
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

trait WalkExprsMutRev {
    fn walk_exprs_mut_rev(&mut self) -> Box<dyn Iterator<Item = &mut Expression>>;
}

impl WalkExprsMutRev for Expression {
    
    
    fn walk_exprs_mut_rev(&mut self) {
        // Allow generics + capture
        macro_rules! v {
            ($($ident:ident),*) => {
                { $($ident.walk_exprs_mut(func);)* }
            }
        }

        match self {
            Self::Assignment(a, b) => Box::new(a.walk_exprs_mut_rev().chain(b.walk_exprs_mut_rev()),
            Self::IfElse(a, b, c) => todo!(),
            _ => todo!(),
        }
        func(self)
    }
}
