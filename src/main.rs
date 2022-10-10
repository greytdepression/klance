#![feature(if_let_guard)]

mod lexer;
mod parser;
mod util;
mod datastructure;

use lexer::Lexer;
use parser::Parser;

pub use util::{Span, Error};

fn main() {
    let source = include_str!("../samples/test1.klc");
    let mut lexer = Lexer::new(source);

    let tokens = lexer.lex();

    for tk in &tokens {
        println!("{:?}", tk);
    }

    println!("\n\n{}", source);

    let mut parser = Parser::new(tokens);

    let parsed_struct = parser.parse();

    util::print_errors(source, parser.errors());

    println!("\n\n{:?}", parsed_struct);
}
