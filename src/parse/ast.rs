// TODO: Optimize representation of AST nodes.
use crate::common::Span;
use crate::parse::token::TokenType;
use crate::types::Type;
use crate::types::TypedVariable;

use ordered_float::OrderedFloat;

#[salsa::accumulator]
pub struct Diagnostics(super::parser::ParseError);

#[salsa::interned]
pub struct QualifiedName {
    components: Vec<String>,
}

#[salsa::tracked]
pub struct Module {
    pub name: QualifiedName,
    #[return_ref]
    pub items: Vec<Item>,
}

#[derive(Clone, Debug, PartialEq, Eq)]
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

#[derive(Clone, Debug, PartialEq, Eq)]
/// Zebra's concept of a top-level item.
pub enum Item {
    Import(Import),
    Function(Function),
    Struct(Struct),
    Interface(Interface),
    Module(Module),
}

#[salsa::tracked]
pub struct Import {
    pub path: QualifiedName,
}

pub type Statement = Spanned<StatementKind>;

#[derive(Clone, Debug, PartialEq, Eq)]
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
#[salsa::tracked]
pub struct FunctionPrototype {
    #[id]
    pub name: FunctionId,

    name_span: Span,

    #[return_ref]
    pub args: Vec<TypedVariable>,

    #[return_ref]
    pub return_type: Type,
}

#[salsa::tracked]
pub struct Function {
    #[return_ref]
    pub body: Vec<Statement>,

    pub proto: FunctionPrototype,
}

#[salsa::interned]
pub struct FunctionId {
    #[return_ref]
    name: String,
}

#[salsa::interned]
pub struct StructId {
    #[return_ref]
    pub name: String,
}

#[salsa::tracked]
pub struct Struct {
    #[id]
    pub name: StructId,

    #[return_ref]
    fields: Vec<TypedVariable>,
}

#[salsa::tracked]
pub struct Interface {
    #[id]
    name: InterfaceId,

    #[return_ref]
    methods: Vec<Function>,
}

#[salsa::interned]
pub struct InterfaceId {
    #[return_ref]
    pub name: String,
}

pub type Expression = Spanned<ExpressionKind>;

#[derive(Clone, Debug, PartialEq, Eq)]
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
    Number(OrderedFloat<f64>),
    String(String),
    Boolean(bool),
    Identifier(String),
}
