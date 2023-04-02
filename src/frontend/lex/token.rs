use crate::common::Span;

#[derive(Debug, Copy, Clone, PartialEq, Eq)]
pub enum TokenType {
    String,
    Identifier,

    // Single character tokens
    LeftParen,
    RightParen,
    LeftBrace,
    RightBrace,
    LeftBracket,
    RightBracket,
    Comma,
    Dot,
    Semicolon,
    Question,

    // One or two character tokens
    Plus,
    PlusEqual,
    Minus,
    MinusGreater,
    MinusEqual,
    Slash,
    SlashEqual,
    Star,
    StarEqual,
    Bang,
    BangEqual,
    Equal,
    EqualEqual,
    EqualGreater,
    Colon,
    ColonEqual,
    Less,
    LessEqual,
    Greater,
    GreaterEqual,

    // Literals
    Number,

    // Keywords
    And,
    Else,
    False,
    For,
    Fn,
    If,
    Interface,
    Impl,
    Import,
    Module,
    Null,
    Or,
    Return,
    Public,
    Struct,
    True,
    Type,
    While,

    Separator,
}

impl TryFrom<&str> for TokenType {
    type Error = ();
    fn try_from(value: &str) -> Result<Self, Self::Error> {
        match value {
            "and" => Ok(Self::And),
            "else" => Ok(Self::Else),
            "false" => Ok(Self::False),
            "for" => Ok(Self::For),
            "fn" => Ok(Self::Fn),
            "if" => Ok(Self::If),
            "interface" => Ok(Self::Interface),
            "impl" => Ok(Self::Impl),
            "import" => Ok(Self::Import),
            "module" => Ok(Self::Module),
            "null" => Ok(Self::Null),
            "or" => Ok(Self::Or),
            "return" => Ok(Self::Return),
            "public" => Ok(Self::Public),
            "struct" => Ok(Self::Struct),
            "true" => Ok(Self::True),
            "type" => Ok(Self::Type),
            "while" => Ok(Self::While),
            _ => Err(()),
        }
    }
}

/// Represents a token from our shell input
#[derive(Debug, Copy, Clone, PartialEq, Eq)]
#[salsa::interned]
pub struct Token {
    pub(crate) t: TokenType,

    #[return_ref]
    pub(crate) text: String,

    pub(crate) span: Span,
}

// impl<'src> ToString for Token<'src> {
//    fn to_string(&self) -> String {
//        return self.text.to_string()
//    }
//}

impl<'src> Token<'src> {
    pub fn new(t: TokenType, text: &'src str, span: Span) -> Self {
        Self { t, text, span }
    }
}

impl std::fmt::Display for Token<'_> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.text)
    }
}
