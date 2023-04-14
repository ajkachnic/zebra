//! The *parse* stage in the compiler
//!
//! This module contains the AST, parser, and scanner for Zebra code, but only exposes the AST and a `parse_module` function. The internals are not tracked by Salsa, only the `parse_module` function is. This achieves a reasonable level of granularity, because parsing is fast enough that caching individual steps could do more harm than good.
use std::path::PathBuf;

use miette::Diagnostic;
use thiserror::Error;

use self::ast::QualifiedName;

pub mod ast;
mod parser;
mod scanner;
mod token;

#[salsa::input]
pub struct SourceProgram {
    #[return_ref]
    pub text: String,

    #[return_ref]
    pub file_name: Option<PathBuf>,
}

#[derive(Debug, Clone, PartialEq, Eq, Diagnostic, Error)]
pub enum ParseError {
    #[error("")]
    ParserError(#[from] parser::ParseError),

    #[error("")]
    ScannerError(#[from] scanner::LexError),


}

#[salsa::tracked]
pub fn parse_module(db: &dyn crate::Db, name: QualifiedName, source: SourceProgram) -> Result<ast::Module, ParseError> {
    let source_text = source.text(db);

    let tokens = scanner::tokenize(source_text)?;

    let mut parser = parser::Parser::new(db, source_text, tokens);
    let mut items = Vec::new();

    while !parser.is_eof() {
        items.push(parser.parse_item()?)
    }


    let module = ast::Module::new(db, name, items);

    Ok(module)
}
