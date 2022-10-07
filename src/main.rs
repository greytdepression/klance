

mod lexer;

use lexer::Lexer;

fn main() {
    let source = include_str!("../samples/test1.klc");
    let mut lexer = Lexer::new(source);

    let (tokens, errors) = lexer.lex();

    println!("{}", source);
    for tk in &tokens {
        println!("{:?}", tk);
    }
}
