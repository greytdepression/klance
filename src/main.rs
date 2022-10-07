

mod lexer;
mod parser;

use lexer::Lexer;
use parser::Parser;

fn main() {
    let source = include_str!("../samples/test1.klc");
    let mut lexer = Lexer::new(source);

    let (tokens, errors) = lexer.lex();

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
