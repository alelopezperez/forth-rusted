use lib_interpreter::{self, Interpreter, Lexer};
use wasm_bindgen::prelude::*;

use std::{fmt::Write, process::Output};

#[wasm_bindgen]
struct ForthInterpreter {
    interpreter: Interpreter,
    output: String,
}

#[wasm_bindgen]
impl ForthInterpreter {
    #[wasm_bindgen(constructor)]
    pub fn new() -> Self {
        Self {
            interpreter: Interpreter::new(),
            output: String::new(),
        }
    }

    #[wasm_bindgen]
    pub fn exec(&mut self, line: String) -> String {
        let lex = Lexer::new(&line);
        let tokens = lex.collect::<Vec<_>>();
        let tokens = tokens
            .into_iter()
            .map(|token| token.unwrap())
            .collect::<Vec<_>>();
        let result = self
            .interpreter
            .proccess_token(tokens, &mut self.output, false);

        let out = self.output.clone();

        let output = self
            .interpreter
            .stack
            .iter()
            .fold(String::new(), |mut acc, curr| {
                let _ = write!(acc, "{} ", curr);
                acc
            });
        let output = output + "<- top\n";
        match result {
            Ok(status) => format!("{output}\t {} {}", out, status),
            Err(status) => format!("\t {}", status),
        }
    }
}
