#![feature(if_let_guard)]

mod lexer;
mod parser;
mod util;

use lexer::Lexer;
use parser::Parser;

pub use util::{Span, Error};

fn main() {
    let source = include_str!("../samples/test1.klc");
    let mut lexer = Lexer::new(source);

    let tokens = lexer.lex();

    println!("{}", source);
    for tk in &tokens {
        println!("{:?}", tk);
    }

    println!("\n\nParsing token stream!\n\n");

    let mut parser = Parser::new(tokens);

    let parsed_struct = parser.parse();

    for error in parser.errors() {
        println!("{:?}", error);
    }

    println!("{:?}", parsed_struct);
}
