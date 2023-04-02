pub mod ast;

use miette::Diagnostic;
use num::FromPrimitive;
use num_derive::FromPrimitive;
use thiserror::Error;

use crate::frontend::lex::Span;
use crate::frontend::lex::Token;
use crate::frontend::lex::TokenType;
use crate::types;
use crate::types::TypedVariable;

use self::ast::Spanned;

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
        $crate::frontend::parse::ParseRule::new(
            std::option::Option::Some(|compiler, can_assign| compiler.$prefix(can_assign)),
            None,
            $precedence,
        )
    };
    (none, $infix:ident, $precedence:expr) => {
        $crate::frontend::parse::ParseRule::new(
            None,
            std::option::Option::Some(|compiler, can_assign, lhs| compiler.$infix(can_assign, lhs)),
            $precedence,
        )
    };
    ($prefix:ident, $infix:ident, $precedence:expr) => {
        $crate::frontend::parse::ParseRule::new(
            std::option::Option::Some(|compiler, can_assign| compiler.$prefix(can_assign)),
            std::option::Option::Some(|compiler, can_assign, lhs| compiler.$infix(can_assign, lhs)),
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

type ParseResult<T> = Result<T, ParseError>;

#[derive(Debug, Clone, Error, Diagnostic)]
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

pub struct Parser<'s> {
    tokens: Vec<Token<'s>>,
    items: Vec<ast::Item>,
    idx: usize,
    source: &'s str,
}

impl<'s> Parser<'s> {
    pub fn new(source: &'s str, tokens: Vec<Token<'s>>) -> Self {
        Self {
            tokens,
            items: Vec::new(),
            idx: 0,
            source,
        }
    }

    fn advance(&mut self) {
        self.idx += 1;
    }

    fn current(&self) -> Option<Token<'s>> {
        if self.idx >= self.tokens.len() {
            None
        } else {
            Some(self.tokens[self.idx])
        }
    }

    fn current_unchecked(&self) -> Token<'s> {
        self.tokens[self.idx]
    }

    fn previous(&self) -> Token<'s> {
        self.tokens[self.idx - 1]
    }

    fn peek(&self) -> Option<Token<'s>> {
        self.tokens.get(self.idx + 1).copied()
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

    fn check_eof(&mut self) -> ParseResult<()> {
        if self.idx >= self.tokens.len() {
            Err(ParseError::UnexpectedEOF {
                src: self.source.to_string(),
                span: self.tokens[self.idx - 1].span,
            })
        } else {
            Ok(())
        }
    }

    fn parse(&mut self) -> ParseResult<Vec<ast::Item>> {
        while self.idx < self.tokens.len() {
            let item = self.parse_item()?;
            self.items.push(item);
        }

        Ok(self.items.clone())
    }

    fn parse_item(&mut self) -> ParseResult<ast::Item> {
        self.check_eof()?;

        let token = &self.tokens[self.idx];

        match token.t {
            TokenType::Fn => Ok(ast::Item::Function(self.parse_function()?)),
            _ => todo!(),
            // _ => Ok(ast::Item::S(self.parse_expression()?)),
        }
    }

    fn parse_block(&mut self) -> ParseResult<Vec<ast::Statement>> {
        self.consume(TokenType::LeftBrace)?;
        self.check_eof()?;

        let mut stmts = Vec::new();

        while self.idx < self.tokens.len() && self.current_unchecked().t != TokenType::RightBrace {
            let stmt = self.parse_statement()?;
            stmts.push(stmt);
            self.consume(TokenType::Separator)?;
        }

        self.consume(TokenType::RightBrace)?;

        Ok(stmts)
    }

    pub(crate) fn parse_statement(&mut self) -> ParseResult<ast::Statement> {
        self.check_eof()?;
        match self.current_unchecked().t {
            _ => {
                let expr = self.parse_expression()?;
                Ok(Spanned::new(
                    expr.span,
                    ast::StatementKind::Expression(expr.inner),
                ))
            }
        }
    }

    /// Parse a full function with it's prototype and body
    ///
    /// Invariant: current token is `fun` keyword
    fn parse_function(&mut self) -> ParseResult<ast::Function> {
        let proto = self.parse_function_prototype()?;
        let body = self.parse_block()?;

        Ok(ast::Function { proto, body })
    }

    /// Parse the prototype of a function (the name and it's arguments).
    ///
    /// Used for parsing full function declarations and just prototypes
    /// in interface implementations.
    ///
    /// Invariant: current token is `fun` keyword
    pub fn parse_function_prototype(&mut self) -> ParseResult<ast::FunctionPrototype> {
        self.advance();
        self.check_eof()?;
        let name = self.consume(TokenType::Identifier)?;

        self.consume(TokenType::LeftParen)?;

        let mut args = Vec::new();

        while self.idx < self.tokens.len() {
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
        /* if let Some(TokenType::Separator) = self.current().map(|t| t.t) {
            eprintln!("parse_function_prototype: skipping separator");
            // Lexer puts a separator between ')' and an ident
            self.advance();
        } */

        let return_type = self.parse_type()?;

        Ok(ast::FunctionPrototype {
            name: name.to_string(),
            args,
            return_type,
        })
    }

    fn parse_typed_variable(&mut self) -> ParseResult<types::TypedVariable> {
        let name = self.consume(TokenType::Identifier)?;
        let ty = self.parse_type()?;

        Ok(TypedVariable::new(name.to_string(), ty))
    }

    fn parse_type(&mut self) -> ParseResult<types::Type> {
        // self.advance();
        self.check_eof()?;
        println!("{:?}", self.current());

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

    pub(crate) fn parse_expression(&mut self) -> ParseResult<ast::Expression> {
        self.parse_precedence(Precedence::None)
    }

    fn parse_precedence(&mut self, precedence: Precedence) -> ParseResult<ast::Expression> {
        self.check_eof()?;

        self.advance();

        if let Some(prefix_rule) = ParseRule::rule(self.previous().t).prefix {
            let can_assign = precedence <= Precedence::Assignment;
            let mut expr = prefix_rule(self, can_assign)?;

            while let Some(current) = self.current() && precedence <= ParseRule::rule(current.t).precedence {
                self.advance();
                // self.check_eof()?;
                let infix_rule = ParseRule::rule(self.previous().t).infix;

                if let Some(infix_rule) = infix_rule {
                    expr = infix_rule(self, can_assign, expr)?;
                } else {
                    return Err(ParseError::InvalidInfixOperator {
                        src: self.source.to_string(),
                        span: self.previous().span,
                        token: self.previous().t,
                    });
                }
            }

            Ok(expr)
        } else {
            println!("expected expression: {:?}", self.current_unchecked());
            Err(ParseError::ExpectedExpression {
                src: self.source.to_string(),
                span: self.current_unchecked().span,
            })
        }
    }

    fn grouping(&mut self, _can_assign: bool) -> ParseResult<ast::Expression> {
        let expr = self.parse_expression()?;

        self.consume(TokenType::RightParen)?;

        Ok(expr)
    }

    fn literal(&mut self, _can_assign: bool) -> ParseResult<ast::Expression> {
        match self.previous() {
            Token {
                t: TokenType::Number,
                text,
                span,
            } => Ok(Spanned::new(
                span,
                ast::ExpressionKind::Number(text.parse().unwrap()),
            )),
            Token {
                t: TokenType::Identifier,
                text,
                span,
            } => Ok(Spanned::new(
                span,
                ast::ExpressionKind::Identifier(text.to_string()),
            )),
            _ => {
                unreachable!()
            }
        }
    }

    fn binary(&mut self, _can_assign: bool, lhs: ast::Expression) -> ParseResult<ast::Expression> {
        let op = self.previous();

        let precedence = ParseRule::rule(op.t).precedence.next();
        let rhs = self.parse_precedence(precedence)?;

        let span = lhs.span + rhs.span;

        Ok(Spanned::new(
            span,
            ast::ExpressionKind::Binary {
                lhs: Box::new(lhs),
                rhs: Box::new(rhs),
                op: op.t,
            },
        ))
    }

    // fn parse_atom(&mut self) -> ParseResult<ast::Expression> {
    //     self.check_eof()?;

    //     let tok = self.tokens[self.idx].clone();
    //     self.idx += 1;

    //     if tok.t == TokenType::Minus {
    //         let atom = self.parse_atom()?;
    //         return Ok(Spanned::new(
    //             tok.span,
    //             ast::ExpressionKind::Unary {
    //                 op: tok.t,
    //                 expr: Box::new(atom),
    //             },
    //         ));
    //     }

    //     self.check_eof()?;

    //     let kind = match tok.t {
    //         TokenType::Number => ast::ExpressionKind::Number(tok.text.parse().unwrap()),
    //         TokenType::String => ast::ExpressionKind::String(tok.text.to_string()),
    //         TokenType::True => ast::ExpressionKind::Boolean(true),
    //         TokenType::False => ast::ExpressionKind::Boolean(false),
    //         TokenType::Identifier => ast::ExpressionKind::Variable {
    //             name: tok.text.to_string(),
    //         },
    //         _ => panic!("Invalid atom: {:?}", tok.t),
    //     };
    //     Ok(Spanned::new(tok.span, kind))
    // }

    // fn parse_expression(&mut self) -> ParseResult<ast::Expression> {
    //     let next = &self.tokens[self.idx];
    //     self.idx += 1;

    //     self.parse_atom()

    //     // match next.kind {
    //     //     TokenType::Number => self.parse_number(),
    //     //     TokenType::String => self.parse_string(),
    //     //     TokenType::Boolean => self.parse_boolean(),
    //     //     TokenType::Identifier => self.parse_identifier(),
    //     //     TokenType::Operator(op) => self.parse_operator(op),
    //     //     _ => Err(ParseError::InvalidInfixOperator {
    //     //         src: self.source.to_string(),
    //     //         span: token.span,
    //     //         token: token.t,
    //     //     }),
    //     // }
    // }
}

#[cfg(test)]
mod tests {
    use crate::frontend::lex::{Span, TokenType};
    use crate::frontend::parse::ast;
    use crate::types;

    macro_rules! parse_test {
        ($method:ident, $input:expr, $expected:expr) => {
            let tokens = crate::frontend::lex::tokenize($input)?;
            let mut parser = crate::frontend::parse::Parser::new($input, tokens);

            let actual = crate::frontend::parse::Parser::$method(&mut parser)?;

            assert_eq!(actual, $expected);
        };
    }

    macro_rules! spanned {
        ($range:expr, $expr:expr) => {
            crate::frontend::parse::ast::Spanned::new($range.into(), $expr)
        };
    }

    mod expr {
        use super::*;

        #[test]
        fn literal() -> miette::Result<()> {
            parse_test!(
                parse_expression,
                "11",
                spanned!(0..2, ast::ExpressionKind::Number(11.0))
            );

            Ok(())
        }

        #[test]
        fn simple() -> miette::Result<()> {
            parse_test!(
                parse_expression,
                "1 + 1",
                ast::Spanned::new(
                    Span::new(0, 5),
                    ast::ExpressionKind::Binary {
                        lhs: Box::new(spanned!(0..1, ast::ExpressionKind::Number(1.0))),
                        rhs: Box::new(spanned!(4..5, ast::ExpressionKind::Number(1.0))),
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
                        lhs: Box::new(spanned!(0..1, ast::ExpressionKind::Number(1.0))),
                        rhs: Box::new(spanned!(
                            4..9,
                            ast::ExpressionKind::Binary {
                                lhs: Box::new(spanned!(4..5, ast::ExpressionKind::Number(2.0))),
                                rhs: Box::new(spanned!(8..9, ast::ExpressionKind::Number(3.0))),
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

    mod function_proto {
        use super::*;

        #[test]
        fn simple() -> miette::Result<()> {
            parse_test!(
                parse_function_prototype,
                "fn proto() void",
                ast::FunctionPrototype {
                    name: "proto".to_string(),
                    args: vec![],
                    return_type: types::Type::Base(types::BaseType::Void)
                }
            );

            Ok(())
        }

        #[test]
        fn two_args() -> miette::Result<()> {
            parse_test!(
                parse_function_prototype,
                "fn add(a number, b number) number",
                ast::FunctionPrototype {
                    name: "add".to_string(),
                    args: vec![
                        types::TypedVariable::new("a".to_string(), types::BaseType::Number.into()),
                        types::TypedVariable::new("b".to_string(), types::BaseType::Number.into()),
                    ],
                    return_type: types::BaseType::Number.into()
                }
            );

            Ok(())
        }

        #[test]
        fn arrays() -> miette::Result<()> {
            parse_test!(
                parse_function_prototype,
                "fn add(a [number], b [number], len number) [number]?",
                ast::FunctionPrototype {
                    name: "add".to_string(),
                    args: vec![
                        types::TypedVariable::new(
                            "a".to_string(),
                            types::Type::from(types::BaseType::Number).array()
                        ),
                        types::TypedVariable::new(
                            "b".to_string(),
                            types::Type::from(types::BaseType::Number).array()
                        ),
                        types::TypedVariable::new(
                            "len".to_string(),
                            types::BaseType::Number.into()
                        ),
                    ],
                    return_type: types::Type::from(types::BaseType::Number)
                        .array()
                        .nullable()
                }
            );

            Ok(())
        }
    }

    mod function {
        use super::*;

        #[test]
        fn simple() -> miette::Result<()> {
            parse_test!(
                parse_function,
                "fn add(a number, b number) number{ a + b }",
                ast::Function {
                    body: vec![spanned!(
                        35..40,
                        ast::StatementKind::Expression(ast::ExpressionKind::Binary {
                            lhs: Box::new(spanned!(
                                35..36,
                                ast::ExpressionKind::Identifier("a".to_string())
                            )),
                            rhs: Box::new(spanned!(
                                39..40,
                                ast::ExpressionKind::Identifier("b".to_string())
                            )),
                            op: TokenType::Plus,
                        })
                    )],
                    proto: ast::FunctionPrototype {
                        name: "add".to_string(),
                        args: vec![
                            types::TypedVariable::new(
                                "a".to_string(),
                                types::BaseType::Number.into()
                            ),
                            types::TypedVariable::new(
                                "b".to_string(),
                                types::BaseType::Number.into()
                            ),
                        ],
                        return_type: types::BaseType::Number.into()
                    },
                }
            );

            Ok(())
        }
    }
}
