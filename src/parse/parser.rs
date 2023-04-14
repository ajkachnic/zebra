use miette::Diagnostic;
use num::FromPrimitive;
use num_derive::FromPrimitive;
use thiserror::Error;

use super::ast;
use super::token::{Token, TokenType};
use crate::common::Span;
use crate::types;
use crate::Db;

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, FromPrimitive)]
enum Precedence {
    None,
    Assignment,
    Or,
    And,
    Equality,
    Comparison,
    Term,
    Factor,
    Unary,
    Call,
    Primary,
}

impl Precedence {
    /// Return the next higher precedence
    pub fn next(self) -> Self {
        if self == Precedence::Primary {
            self
        } else {
            Precedence::from_u8(self as u8 + 1).unwrap()
        }
    }
}

impl Default for Precedence {
    fn default() -> Self {
        Self::None
    }
}

type PrefixFn = fn(&mut Parser, can_assign: bool) -> ParseResult<ast::Expression>;
type InfixFn =
    fn(&mut Parser, can_assign: bool, lhs: ast::Expression) -> ParseResult<ast::Expression>;

struct ParseRule {
    prefix: Option<PrefixFn>,
    infix: Option<InfixFn>,
    precedence: Precedence,
}

macro_rules! parse_rule {
    ($prefix:ident, none, $precedence:expr) => {
        $crate::parse::parser::ParseRule::new(
            std::option::Option::Some(|compiler, can_assign| compiler.$prefix(can_assign)),
            None,
            $precedence,
        )
    };
    (none, $infix:ident, $precedence:expr) => {
        $crate::parse::parser::ParseRule::new(
            None,
            std::option::Option::Some(|compiler, can_assign, lhs| compiler.$infix(can_assign, lhs)),
            $precedence,
        )
    };
    ($prefix:ident, $infix:ident, $precedence:expr) => {
        $crate::parse::parser::ParseRule::new(
            std::option::Option::Some(|compiler, can_assign| compiler.$prefix(can_assign)),
            std::option::Option::Some(|compiler, can_assign, lhs| {
                compiler.parse_$infix(can_assign, lhs)
            }),
            $precedence,
        )
    };
}

impl Default for ParseRule {
    fn default() -> Self {
        Self {
            prefix: None,
            infix: None,
            precedence: Precedence::None,
        }
    }
}

impl ParseRule {
    fn new(prefix: Option<PrefixFn>, infix: Option<InfixFn>, precedence: Precedence) -> Self {
        Self {
            prefix,
            infix,
            precedence,
        }
    }
    pub fn rule(t: TokenType) -> ParseRule {
        match t {
            TokenType::LeftParen => parse_rule!(grouping, none, Precedence::Call),
            TokenType::Number => parse_rule!(literal, none, Precedence::Term),
            TokenType::Identifier => parse_rule!(literal, none, Precedence::Term),

            TokenType::Plus => parse_rule!(none, binary, Precedence::Term),
            TokenType::Minus => parse_rule!(none, binary, Precedence::Term),
            TokenType::Star => parse_rule!(none, binary, Precedence::Factor),
            TokenType::Slash => parse_rule!(none, binary, Precedence::Factor),
            _ => Default::default(),
        }
    }
}

/// Represents a failure in parsing.
///
/// Generally, try to add more specific errors wherever possible.
/// The more applicable an error is, the better.
#[derive(Debug, Clone, Error, Diagnostic, PartialEq, Eq)]
pub enum ParseError {
    #[error("Invalid infix operator: {token:?}")]
    InvalidInfixOperator {
        #[source_code]
        src: String,

        #[label = "Here"]
        span: Span,

        token: TokenType,
    },
    #[error("Unexpected end of file")]
    UnexpectedEOF {
        #[source_code]
        src: String,

        #[label = "Here"]
        span: Span,
    },

    #[error("Expected expression")]
    ExpectedExpression {
        #[source_code]
        src: String,

        #[label = "Here"]
        span: Span,
    },

    #[error("Expected token-type {expected:?}, found {found:?}")]
    ExpectedFound {
        #[source_code]
        src: String,

        #[label = "Here"]
        span: Span,

        expected: TokenType,
        found: TokenType,
    },
}

impl ParseError {
    pub fn recoverable(&self) -> bool {
        match self {
            ParseError::UnexpectedEOF { .. } => false,
            _ => true,
        }
    }
}

type ParseResult<T> = Result<T, ParseError>;

enum TokenPosition {
    Current,
    Previous,
}

/// Parser parses Zebra code, given tokens and source code
///
/// There are parsing methods named `parse_x`. Each method tries to parse
/// `x` at the current position. If this succeeds, the method will return
/// `Ok(x)`. If it doesn't, it will return `Err(ParseError)`.
///
/// Some errors are recoverable, and parsing can continue afterwards.
/// Call `ParseError::recoverable` to check this, and then call `recover()`
/// on the Parser struct to move forward to a valid position.
pub struct Parser<'src> {
    tokens: Vec<Token<'src>>,
    source: &'src str,
    position: usize,

    spans: Vec<Span>,

    db: &'src dyn Db,
}

impl<'s> Parser<'s> {
    pub fn new(db: &'s dyn Db, source: &'s str, tokens: Vec<Token<'s>>) -> Self {
        Self {
            tokens,
            source,
            position: 0,

            spans: Vec::with_capacity(8),

            db,
        }
    }
    fn guard_eof(&mut self) -> ParseResult<()> {
        if self.position >= self.tokens.len() {
            Err(ParseError::UnexpectedEOF {
                src: self.source.to_string(),
                span: self.tokens[self.position - 1].span,
            })
        } else {
            Ok(())
        }
    }

    fn advance(&mut self) {
        self.position += 1;
    }

    fn consume(&mut self, t: TokenType) -> ParseResult<Token<'s>> {
        if self.current_unchecked().t == t {
            let token = self.current_unchecked();
            self.advance();
            Ok(token)
        } else {
            Err(ParseError::ExpectedFound {
                src: self.source.to_string(),
                span: self.current_unchecked().span,
                expected: t,
                found: self.current_unchecked().t,
            })
        }
    }

    /// Like `consume` but it takes a map function that creates an error when it fails
    ///
    /// Useful for asserting a token but providing a better error
    fn consume_better(
        &mut self,
        t: TokenType,
        err: fn(&mut Parser) -> ParseError,
    ) -> ParseResult<Token<'s>> {
        if self.current_unchecked().t == t {
            let token = self.current_unchecked();
            self.advance();
            Ok(token)
        } else {
            Err(err(self))
        }
    }

    fn current(&self) -> Option<Token<'s>> {
        if self.position >= self.tokens.len() {
            None
        } else {
            Some(self.tokens[self.position])
        }
    }

    fn current_unchecked(&self) -> Token<'s> {
        self.tokens[self.position]
    }

    fn previous(&self) -> Token<'s> {
        self.tokens[self.position - 1]
    }

    fn push_span(&mut self, p: TokenPosition) {
        let s = match p {
            TokenPosition::Current => self.current_unchecked().span,
            TokenPosition::Previous => self.previous().span,
        };
        self.spans.push(s);
    }

    fn pop_span(&mut self, p: TokenPosition) -> Span {
        let span = self.spans.pop().unwrap();
        let s = match p {
            TokenPosition::Current => self.current_unchecked().span,
            TokenPosition::Previous => self.previous().span,
        };

        return Span {
            start: span.start,
            end: s.end,
        };
    }

    fn debug_state(&self, msg: &str) {
        println!(
            "{}:\n  prev {:?}\n  current {:?}",
            msg,
            self.previous(),
            self.current()
        );
    }

    pub fn is_eof(&self) -> bool {
        self.position >= self.tokens.len()
    }

    pub fn parse_item(&mut self) -> ParseResult<ast::Item> {
        self.guard_eof()?;

        let token = &self.tokens[self.position];

        match token.t {
            TokenType::Fn => Ok(ast::Item::Function(self.parse_function()?)),
            _ => todo!(),
        }
    }

    fn parse_block(&mut self) -> ParseResult<Vec<ast::Statement>> {
        self.consume(TokenType::LeftBrace)?;
        self.guard_eof()?;

        let mut stmts = Vec::new();

        while self.position < self.tokens.len()
            && self.current_unchecked().t != TokenType::RightBrace
        {
            let stmt = self.parse_statement()?;
            stmts.push(stmt);
            self.consume(TokenType::Separator)?;
        }

        self.consume(TokenType::RightBrace)?;

        Ok(stmts)
    }

    /// Parse a full function with it's prototype and body
    ///
    /// Invariant: current token is `fun` keyword
    fn parse_function(&mut self) -> ParseResult<ast::Function> {
        let proto = self.parse_function_prototype()?;
        let body = self.parse_block()?;

        Ok(ast::Function::new(self.db, body, proto))
    }

    /// Parse the prototype of a function (the name and it's arguments).
    ///
    /// Used for parsing full function declarations and just prototypes
    /// in interface implementations.
    ///
    /// `fun helloWorld(a string, b bool) void`
    ///
    /// Invariant: current token is `fun` keyword
    pub fn parse_function_prototype(&mut self) -> ParseResult<ast::FunctionPrototype> {
        self.advance();
        self.guard_eof()?;
        let name = self.consume(TokenType::Identifier)?;

        self.consume(TokenType::LeftParen)?;

        let mut args = Vec::new();

        while self.position < self.tokens.len() {
            if self.current_unchecked().t == TokenType::RightParen {
                break;
            }

            args.push(self.parse_typed_variable()?);

            if self.current().map(|t| t.t) == Some(TokenType::RightParen) {
                break;
            }

            self.consume(TokenType::Comma)?;
        }

        self.consume(TokenType::RightParen)?;

        // TODO: Allow omitting return type and assuming void
        if let Some(TokenType::Separator) = self.current().map(|t| t.t) {
            eprintln!("parse_function_prototype: skipping separator");
            // Lexer puts a separator between ')' and an ident
            self.advance();
        }

        let return_type = self.parse_type()?;

        Ok(ast::FunctionPrototype::new(
            self.db,
            ast::FunctionId::new(self.db, name.text.to_string()),
            name.span,
            args,
            return_type,
        ))
    }

    fn parse_type(&mut self) -> ParseResult<types::Type> {
        // self.advance();
        self.guard_eof()?;

        let current = self.current_unchecked();

        let ty = match current.t {
            // Array type
            TokenType::LeftBracket => {
                self.advance();
                let inner = self.parse_type()?;
                self.consume(TokenType::RightBracket)?;

                types::Type::Array {
                    ty: Box::new(inner),
                }
            }
            TokenType::Identifier => {
                let ty = match current.text {
                    "string" => types::Type::Base(types::BaseType::String),
                    "number" => types::Type::Base(types::BaseType::Number),
                    "bool" => types::Type::Base(types::BaseType::Boolean),
                    "void" => types::Type::Base(types::BaseType::Void),
                    _ => todo!(),
                };
                self.advance();
                ty
            }
            _ => {
                eprintln!("cant parse token: {:?}", current);
                todo!()
            }
        };

        if let Some(TokenType::Question) = self.current().map(|t| t.t) {
            Ok(types::Type::Nullable(Box::new(ty)))
        } else {
            Ok(ty)
        }
    }

    fn parse_typed_variable(&mut self) -> ParseResult<types::TypedVariable> {
        let name = self.consume(TokenType::Identifier)?;
        let ty = self.parse_type()?;

        Ok(types::TypedVariable::new(name.to_string(), ty))
    }

    pub(crate) fn parse_statement(&mut self) -> ParseResult<ast::Statement> {
        self.guard_eof()?;

        let expr = self.parse_expression()?;
        Ok(ast::Spanned::new(
            expr.span,
            ast::StatementKind::Expression(expr.inner),
        ))
    }

    fn parse_expression(&mut self) -> ParseResult<ast::Expression> {
        self.parse_precedence(Precedence::None)
    }

    fn parse_precedence(&mut self, precedence: Precedence) -> ParseResult<ast::Expression> {
        // Guard and then advance, so `previous()` returns the left-hand side
        self.guard_eof()?;
        self.advance();

        let Some(prefix_rule) = ParseRule::rule(self.previous().t).prefix else {
          return Err(ParseError::ExpectedExpression {
              src: self.source.to_string(),
              span: self.previous().span,
          });
        };

        let can_assign = precedence <= Precedence::Assignment;
        let mut expr = prefix_rule(self, can_assign)?;

        while let Some(token) = self.current() && precedence <= ParseRule::rule(token.t).precedence {
          self.advance();

          self.debug_state("PREV RECURSE");

          let Some(infix_rule) = ParseRule::rule(token.t).infix else {
            return Err(ParseError::InvalidInfixOperator {
                src: self.source.to_string(),
                span: self.previous().span,
                token: self.previous().t,
            });
          };

          expr = infix_rule(self, can_assign, expr)?;
        }

        Ok(expr)
    }

    // Parsers for rules
    fn binary(&mut self, _can_assign: bool, lhs: ast::Expression) -> ParseResult<ast::Expression> {
        let op = self.previous();

        let precedence = ParseRule::rule(op.t).precedence.next();
        let rhs = self.parse_precedence(precedence)?;

        let span = lhs.span + rhs.span;

        Ok(ast::Spanned::new(
            span,
            ast::ExpressionKind::Binary {
                lhs: Box::new(lhs),
                rhs: Box::new(rhs),
                op: op.t,
            },
        ))
    }

    fn literal(&mut self, _can_assign: bool) -> ParseResult<ast::Expression> {
        let prev = self.previous();
        match prev.t {
            TokenType::Number => Ok(ast::Spanned::new(
                prev.span,
                ast::ExpressionKind::Number(prev.text.parse().unwrap()),
            )),
            TokenType::Identifier => Ok(ast::Spanned::new(
                prev.span,
                ast::ExpressionKind::Identifier(prev.text.to_string()),
            )),
            _ => {
                unreachable!()
            }
        }
    }

    fn grouping(&mut self, _can_assign: bool) -> ParseResult<ast::Expression> {
        let expr = self.parse_expression()?;

        self.consume(TokenType::RightParen)?;

        Ok(expr)
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    macro_rules! parse_test {
        ($method:ident, $input:expr, $expected:expr) => {
            let db = crate::db::Database::default();
            let tokens = crate::parse::scanner::tokenize($input)?;
            let mut parser = crate::parse::parser::Parser::new(&db, $input, tokens);

            let actual = crate::parse::parser::Parser::$method(&mut parser)?;

            assert_eq!(actual, $expected);
        };
    }

    macro_rules! spanned {
        ($range:expr, $expr:expr) => {
            crate::parse::ast::Spanned::new($range.into(), $expr)
        };
    }

    mod expr {
        use super::*;
        use ordered_float::OrderedFloat;

        #[test]
        fn simple() -> miette::Result<()> {
            parse_test!(
                parse_expression,
                "1 + 1",
                ast::Spanned::new(
                    Span::new(0, 5),
                    ast::ExpressionKind::Binary {
                        lhs: Box::new(spanned!(
                            0..1,
                            ast::ExpressionKind::Number(OrderedFloat(1.0))
                        )),
                        rhs: Box::new(spanned!(
                            4..5,
                            ast::ExpressionKind::Number(OrderedFloat(1.0))
                        )),
                        op: TokenType::Plus,
                    }
                )
            );

            Ok(())
        }

        #[test]
        fn precedence() -> miette::Result<()> {
            parse_test!(
                parse_expression,
                "1 + 2 * 3",
                spanned!(
                    0..9,
                    ast::ExpressionKind::Binary {
                        lhs: Box::new(spanned!(
                            0..1,
                            ast::ExpressionKind::Number(OrderedFloat(1.0))
                        )),
                        rhs: Box::new(spanned!(
                            4..9,
                            ast::ExpressionKind::Binary {
                                lhs: Box::new(spanned!(
                                    4..5,
                                    ast::ExpressionKind::Number(OrderedFloat(2.0))
                                )),
                                rhs: Box::new(spanned!(
                                    8..9,
                                    ast::ExpressionKind::Number(OrderedFloat(3.0))
                                )),
                                op: TokenType::Star,
                            }
                        )),
                        op: TokenType::Plus,
                    }
                )
            );

            Ok(())
        }
    }
}
