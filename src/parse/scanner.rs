use miette::Diagnostic;
use thiserror::Error;

pub use super::token::{Token, TokenType};
use crate::common::Span;

pub struct Reader<'input> {
    source: &'input str,
    index: usize,
    start: usize,
}

impl<'input> Reader<'input> {
    pub fn new(source: &'input str) -> Self {
        Self {
            source,
            start: 0,
            index: 0,
        }
    }

    fn peek(&self) -> Option<char> {
        self.source.chars().nth(self.index)
    }

    fn lookback(&self) -> Option<char> {
        self.source.chars().nth(self.index - 1)
    }

    fn has_next(&self) -> bool {
        self.index < self.source.len()
    }

    fn next(&mut self) {
        if self.has_next() {
            self.index += 1;
        }
    }

    fn pop_span(&mut self) -> Span {
        let span = Span::new(self.start, self.index);
        self.start = self.index;
        span
    }

    fn pop_token(&mut self, t: TokenType) -> Token<'input> {
        let span = self.pop_span();
        Token::new(t, &self.source[span.start..span.end], span)
    }

    fn pop_token_next(&mut self, t: TokenType) -> Token<'input> {
        self.next();
        self.pop_token(t)
    }

    fn take_while<F>(&mut self, cond: F) -> &str
    where
        F: Fn(char) -> bool,
    {
        while self.has_next() && (self.peek().map_or(false, &cond) || self.lookback() == Some('\\'))
        {
            self.next();
        }
        return self.take();
    }

    fn take(&self) -> &str {
        return &self.source[self.start..self.index];
    }
}

#[derive(Debug, Clone, Error, Diagnostic, PartialEq, Eq)]
pub enum LexError {
    #[error("Invalid numeric literal")]
    #[diagnostic(code(lex::invalid_numeric_literal))]
    InvalidNumericLiteral {
        #[source_code]
        src: String,

        #[label = "Found here"]
        span: Span,
    },
}

macro_rules! next_matches {
    ($r:ident, if $c:literal $t1:ident else $t2:ident) => {{
        $r.next();
        if $r.peek() == Some($c) {
            $r.pop_token_next(TokenType::$t1)
        } else {
            $r.pop_token(TokenType::$t2)
        }
    }};
    () => {};
}

/// Check if the prior token generated can end an expression,
/// and add a separator if it can
fn ensure_separator<'s>(tokens: &mut Vec<Token<'s>>, reader: &mut Reader<'s>) {
    match tokens.last() {
        Some(tok) => match tok.t {
            TokenType::RightParen
            | TokenType::RightBrace
            | TokenType::RightBracket
            | TokenType::Number
            | TokenType::String
            | TokenType::Identifier
            | TokenType::True
            | TokenType::False
            | TokenType::Null => tokens.push(reader.pop_token(TokenType::Separator)),
            _ => {}
        },
        None => (),
    }
}

pub fn tokenize<'src>(input: &'src str) -> Result<Vec<Token<'src>>, LexError> {
    let mut tokens = Vec::new();
    let mut reader = Reader::new(input);

    while reader.has_next() {
        let c = reader.peek().unwrap();

        match c {
            '(' => tokens.push(reader.pop_token_next(TokenType::LeftParen)),
            ')' => {
                ensure_separator(&mut tokens, &mut reader);
                tokens.push(reader.pop_token_next(TokenType::RightParen))
            }
            '{' => tokens.push(reader.pop_token_next(TokenType::LeftBrace)),
            '}' => {
                ensure_separator(&mut tokens, &mut reader);
                tokens.push(reader.pop_token_next(TokenType::RightBrace))
            }
            '[' => tokens.push(reader.pop_token_next(TokenType::LeftBracket)),
            ']' => {
                ensure_separator(&mut tokens, &mut reader);
                tokens.push(reader.pop_token_next(TokenType::RightBracket))
            }
            ',' => tokens.push(reader.pop_token_next(TokenType::Comma)),
            '.' => tokens.push(reader.pop_token_next(TokenType::Dot)),
            ';' => tokens.push(reader.pop_token_next(TokenType::Semicolon)),
            '?' => tokens.push(reader.pop_token_next(TokenType::Question)),

            '\n' => {
                ensure_separator(&mut tokens, &mut reader);
                reader.next();
                reader.pop_span();
            }

            '\t' | ' ' => {
                reader.next();
                reader.pop_span();
            }

            // One or two character tokens
            ':' => tokens.push(next_matches!(reader, if '=' ColonEqual else Colon)),
            '+' => tokens.push(next_matches!(reader, if '=' PlusEqual else Plus)),
            '-' => tokens.push({
                reader.next();
                if reader.peek() == Some('=') {
                    reader.pop_token_next(TokenType::MinusEqual)
                } else if reader.peek() == Some('>') {
                    reader.pop_token_next(TokenType::MinusGreater)
                } else {
                    reader.pop_token(TokenType::Minus)
                }
            }),
            '*' => tokens.push(next_matches!(reader, if '=' StarEqual else Star)),
            '/' => tokens.push(next_matches!(reader, if '=' SlashEqual else Slash)),
            '!' => tokens.push(next_matches!(reader, if '=' BangEqual else Bang)),
            '>' => tokens.push(next_matches!(reader, if '=' GreaterEqual else Greater)),
            '<' => tokens.push(next_matches!(reader, if '=' LessEqual else Less)),

            '=' => tokens.push({
                reader.next();
                if reader.peek() == Some('=') {
                    reader.pop_token_next(TokenType::EqualEqual)
                } else if reader.peek() == Some('>') {
                    reader.pop_token_next(TokenType::EqualGreater)
                } else {
                    reader.pop_token(TokenType::Equal)
                }
            }),

            // String literals
            '"' => {
                reader.next();
                reader.take_while(|c| c != '"');
                tokens.push(reader.pop_token(TokenType::String));
            }

            // Identifiers
            'a'..='z' | 'A'..='Z' | '_' => {
                // ensure_separator(&mut tokens, &mut reader);
                reader.take_while(|c| c.is_ascii_alphanumeric() || c == '_');
                let ident = reader.take();
                let t = ident.try_into().unwrap_or(TokenType::Identifier);
                tokens.push(reader.pop_token(t));
            }

            // Literals
            '0'..='9' => {
                let numeral = reader.take_while(|c| c.is_ascii_digit() || c == '.');
                let r = numeral.parse::<f64>();
                match r {
                    Ok(_) => tokens.push(reader.pop_token(TokenType::Number)),
                    Err(_) => {
                        return Err(LexError::InvalidNumericLiteral {
                            src: input.to_string(),
                            span: reader.pop_span(),
                        })
                    }
                }
            }

            _ => todo!(),
        }
    }

    Ok(tokens)
}
