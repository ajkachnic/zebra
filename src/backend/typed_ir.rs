use std::collections::HashMap;

use crate::frontend::lex::Span;
use crate::frontend::lex::TokenType;
use crate::frontend::parse::ast;
use crate::types::Type;
use crate::types::TypedVariable;

#[derive(Debug, Clone, PartialEq)]
pub struct TypedSpanned<T> {
    pub span: Span,
    pub ty: Type,
    pub inner: T,
}

pub struct Module {
    pub name: String,

    pub items: Vec<Item>,

    pub symbols: HashMap<String, SymbolType>,
}

pub enum Item {
    Function(Function),
    Struct(Struct),
    Interface(Interface),
}

pub enum Statement {
    Expression(Expression),
    Assignment {
        name: TypedVariable,
        expr: Expression,
    },
    VariableDeclaration {
        name: TypedVariable,
        expr: Option<Expression>,
    },
    Return(Option<Expression>),
}

pub struct Function {
    pub proto: ast::FunctionPrototype,
    pub body: Vec<Statement>,
}

pub type Expression = TypedSpanned<ExpressionKind>;

#[derive(Debug, Clone)]
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
}

pub struct ResolvedSymbol {
    pub name: String,
    pub ty: Type,

    /// Index of the module that this symbol was defined in
    pub source_module: usize,
}

pub enum SymbolType {
    FunctionProto(ast::FunctionPrototype),
    Struct(Struct),
    Interface(Interface),
}

/// The type information for a function, used for type checking
pub struct FunctionHeader {
    pub name: String,
    pub args: Vec<TypedVariable>,
    pub return_type: Type,
}

pub struct Struct {
    pub name: String,
    pub fields: Vec<TypedVariable>,
}

pub struct Interface {
    pub name: String,
    pub methods: Vec<FunctionHeader>,
}
