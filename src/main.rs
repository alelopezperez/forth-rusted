use std::io::Write;

use interpreter::{Interpreter, Lexer};

mod interpreter;
fn main() {
    let args = std::env::args().collect::<Vec<_>>();

    match args.len() {
        1 => {
            cli_interpreter();
        }
        2 => {
            println!("RUN FILE MODE");
        }
        0 => {
            eprintln!("error running the program");
        }
        _ => {
            eprintln!("usage: only one source file<>");
        }
    }
}

fn cli_interpreter() {
    let mut line_buffer = String::new();
    let mut interpreter = Interpreter::new();
    while std::io::stdin().read_line(&mut line_buffer).is_ok() {
        print!("{}[2J", 27 as char);
        let tokens = Lexer::new(line_buffer.trim()).collect::<Vec<_>>();
        let tokens = tokens
            .into_iter()
            .map(|token| token.unwrap())
            .collect::<Vec<_>>();

        let stack = interpreter.proccess_token(tokens);
        for elem in stack {
            print!("{} ", elem);
        }
        println!("<- Top");
        std::io::stdout().flush().unwrap();

        line_buffer.clear();
    }
}
