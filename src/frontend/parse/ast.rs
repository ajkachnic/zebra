// TODO: Optimize representation of AST nodes.
use crate::frontend::lex::Span;
use crate::frontend::lex::TokenType;
use crate::types::Type;
use crate::types::TypedVariable;

use std::rc::Rc;

#[derive(PartialEq, Debug, Clone)]
pub struct Module {
    pub name: String,
    pub items: Vec<Item>,
}

#[derive(Clone, Debug, PartialEq)]
pub struct Spanned<T> {
    pub span: Span,
    pub inner: T,
}

impl<T> Spanned<T> {
    pub fn new(span: Span, inner: T) -> Self {
        Self { span, inner }
    }

    pub fn map<N, F>(self, func: F) -> Spanned<N>
    where
        F: Fn(T) -> N,
    {
        Spanned::new(self.span, func(self.inner))
    }
}


#[derive(Clone, Debug, PartialEq)]
/// Zebra's concept of a top-level item.
pub enum Item {
    Import(Import),
    Function(Function),
    Struct(Struct),
    Interface(Interface),
    Module(Module),
}

#[derive(Clone, Debug, PartialEq)]
pub struct Import {
    pub path: String,
}

pub type Statement = Spanned<StatementKind>;

#[derive(Clone, Debug, PartialEq)]
pub enum StatementKind {
    Expression(ExpressionKind),
    Assignment {
        name: String,
        expr: Expression,
    },
    VariableDeclaration {
        name: Spanned<String>,
        ty: Option<Spanned<Type>>,
        expr: Option<Expression>,
    },
    Return(Option<Expression>),
}

/// Type information about a function
#[derive(Clone, Debug, PartialEq)]
pub struct FunctionPrototype {
    pub name: String,
    pub args: Vec<TypedVariable>,
    pub return_type: Type,
}

#[derive(Clone, Debug, PartialEq)]
pub struct Function {
    pub body: Vec<Statement>,
    pub proto: FunctionPrototype,
}

#[derive(Clone, Debug, PartialEq)]
pub struct Struct {
    name: String,
    fields: Vec<TypedVariable>,
}

#[derive(Clone, Debug, PartialEq)]
pub struct Interface {
    name: String,
    methods: Vec<Function>,
}

pub type Expression = Spanned<ExpressionKind>;

#[derive(Clone, Debug, PartialEq)]
pub enum ExpressionKind {
    Binary {
        lhs: Box<Expression>,
        rhs: Box<Expression>,
        op: TokenType,
    },
    Unary {
        expr: Box<Expression>,
        op: TokenType,
    },
    Variable {
        name: String,
    },
    Call {
        name: String,
        args: Vec<Expression>,
    },
    Number(f64),
    String(String),
    Boolean(bool),
    Identifier(String),
}
