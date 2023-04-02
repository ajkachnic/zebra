#![feature(let_chains)]

mod backend;
mod common;
mod db;
mod frontend;
mod types;

#[salsa::jar(db = Db)]
pub struct Jar(
    db::SourceProgram,
);

pub trait Db: salsa::DbWithJar<Jar> {}

impl<DB> Db for DB where DB: ?Sized + salsa::DbWithJar<Jar> {}

fn main() {
    let base = db::Database::default();
    loop {
        // Basic repl
        let mut input = String::new();
        std::io::stdin().read_line(&mut input).unwrap();

        let source = db::SourceProgram::new(&mut base, input);

        let tokens = frontend::lex::tokenize(&base, source).unwrap();
        for token in &tokens {
            println!("{:?}", token);
        }

        // let mut parser = frontend::parse::Parser::new(&input, tokens);
        // let proto = parser.parse_function_prototype().unwrap();

        // println!("{proto:?}");
    }
}
