#![feature(let_chains)]

use std::path::PathBuf;

use types::Type;

mod backend;
mod common;
mod frontend;
mod types;

pub mod db;
pub mod parse;

#[salsa::jar(db = Db)]
pub struct Jar(
    parse::parse_module,
    parse::ast::Diagnostics,
    parse::SourceProgram,
    parse::ast::QualifiedName,
    parse::ast::Module,
    parse::ast::Import,
    parse::ast::Interface,
    parse::ast::InterfaceId,
    parse::ast::Function,
    parse::ast::FunctionId,
    parse::ast::FunctionPrototype,
    parse::ast::Struct,
    parse::ast::StructId,
);

pub trait Db: salsa::DbWithJar<Jar> {}

impl<DB> Db for DB where DB: ?Sized + salsa::DbWithJar<Jar> {}
