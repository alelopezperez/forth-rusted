use std::{char, collections::VecDeque, fmt::Display};

#[derive(Debug)]
pub enum Token {
    Number(Number),
    ArithmeticOperators(ArithmeticOperators),
}

#[derive(Debug)]
pub enum ArithmeticOperators {
    Plus,
    Minus,
}

#[derive(Debug)]
pub enum Number {
    SignedInteger(i64),
    UsignedInteger(u64),
    Float(f64),
}

impl Display for Token {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Token::ArithmeticOperators(operator) => write!(f, "{}", operator),
            Token::Number(num) => write!(f, "{}", num),
        }
    }
}

impl Display for ArithmeticOperators {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            ArithmeticOperators::Plus => write!(f, "+"),
            ArithmeticOperators::Minus => write!(f, "-"),
        }
    }
}

impl Display for Number {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Number::SignedInteger(num) => write!(f, "{}", num),
            Number::UsignedInteger(num) => write!(f, "{}", num),
            Number::Float(num) => write!(f, "{}", num),
        }
    }
}

pub struct Lexer<'a> {
    rest: &'a str,
}

impl<'a> Lexer<'a> {
    pub fn new(input: &'a str) -> Self {
        Self { rest: input }
    }

    fn parse_num(&mut self, digit: char) -> Result<Number, ()> {
        let mut number = String::from(digit);

        let mut num_iter = self.rest.chars();

        for letter in num_iter.by_ref() {
            if letter.is_whitespace() {
                break;
            }
            if letter.is_ascii_digit() {
                number.push(letter);
            } else {
                return Err(());
            }
        }
        self.rest = num_iter.as_str();

        Ok(Number::UsignedInteger(number.parse().unwrap()))
    }
}

impl<'a> Iterator for Lexer<'a> {
    type Item = Result<Token, ()>;

    fn next(&mut self) -> Option<Self::Item> {
        let mut input_iter = self.rest.chars();
        let letter = input_iter.next()?;

        self.rest = input_iter.as_str();

        match letter {
            '+' => Some(Ok(Token::ArithmeticOperators(ArithmeticOperators::Plus))),
            '-' => Some(Ok(Token::ArithmeticOperators(ArithmeticOperators::Minus))),
            '0'..='9' => {
                let num = self.parse_num(letter);
                match num {
                    Ok(num) => Some(Ok(Token::Number(num))),
                    Err(_) => Some(Err(())),
                }
            }
            letter if letter.is_whitespace() => {
                println!("Skip");
                self.next()
            }
            _ => Some(Err(())),
        }
    }
}

pub enum InterpreterStatus {
    Ok,
    Error(InterpreterError),
}
pub enum InterpreterError {
    Overflow,
    Underflow,
}

pub struct Interpreter {
    stack: VecDeque<Token>,
}

impl Interpreter {
    pub fn new() -> Self {
        Self {
            stack: VecDeque::new(),
        }
    }
    fn arithmetic_operations(
        op: ArithmeticOperators,
        left: Token,
        right: Token,
    ) -> Result<Token, InterpreterError> {
        todo!()
    }
    pub fn proccess_token(&mut self, tokens: Vec<Token>) -> (&VecDeque<Token>, InterpreterStatus) {
        let mut status = InterpreterStatus::Ok;
        for token in tokens {
            match token {
                Token::Number(_) => self.stack.push_back(token),
                Token::ArithmeticOperators(op) => {
                    if self.stack.len() == 2 {
                        let left = self.stack.pop_back().unwrap();
                        let right = self.stack.pop_back().unwrap();
                        let result = Self::arithmetic_operations(op, left, right);
                        match result {
                            Ok(result) => self.stack.push_back(result),
                            Err(error) => status = InterpreterStatus::Error(error),
                        }
                    }
                }
            }
        }
        (&self.stack, status)
    }
}
