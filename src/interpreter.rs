use std::{
    char,
    collections::{HashMap, VecDeque},
    fmt::Display,
};

#[derive(Debug, PartialEq, Clone)]
pub enum Token {
    Number(Number),
    ArithmeticOperators(ArithmeticOperators),
    Word(String),
    Colon,
    ApostroColon(String),
    Semicolon,
    Dot,
    Equal,
}

#[derive(Debug, PartialEq, Clone)]
pub enum ArithmeticOperators {
    Plus,
    Minus,
}

#[derive(Debug, PartialEq, Clone)]
pub enum Number {
    SignedInteger(i128),
    Float(f64),
}

impl Display for Token {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Token::ArithmeticOperators(operator) => write!(f, "{}", operator),
            Token::Number(num) => write!(f, "{}", num),
            Token::Colon => write!(f, ":",),
            Token::Word(word) => write!(f, "{}", word),
            Token::Semicolon => todo!(),
            Token::Dot => todo!(),
            Token::ApostroColon(_) => todo!(),
            Token::Equal => todo!(),
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

        Ok(Number::SignedInteger(number.parse().unwrap()))
    }

    fn parse_word(&mut self, letter: char) -> Result<String, ()> {
        let mut word_iter = self.rest.chars();

        let word = std::iter::once(letter)
            .chain(word_iter.by_ref().take_while(|w| !w.is_whitespace()))
            .collect::<String>();

        self.rest = word_iter.as_str();

        Ok(word)
    }
}

impl<'a> Iterator for Lexer<'a> {
    type Item = Result<Token, ()>;

    fn next(&mut self) -> Option<Self::Item> {
        let mut input_iter = self.rest.char_indices();
        let (pik, letter) = input_iter.next()?;

        self.rest = input_iter.as_str();

        match letter {
            '+' => Some(Ok(Token::ArithmeticOperators(ArithmeticOperators::Plus))),
            ':' => Some(Ok(Token::Colon)),
            ';' => Some(Ok(Token::Semicolon)),
            '=' => Some(Ok(Token::Equal)),
            '.' => {
                if pik + 1 < self.rest.len() {
                    let peek = &self.rest[pik..pik + 1];
                    if peek == "\"" {
                        self.rest = &self.rest[pik + 1..];
                        let mut apo_it = self.rest.chars();
                        let out = apo_it
                            .by_ref()
                            .take_while(|lt| *lt != '"')
                            .collect::<String>();
                        self.rest = apo_it.as_str();

                        Some(Ok(Token::ApostroColon(out)))
                    } else {
                        Some(Ok(Token::Dot))
                    }
                } else {
                    Some(Ok(Token::Dot))
                }
            }
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
            _ => {
                let word = self.parse_word(letter).unwrap();
                Some(Ok(Token::Word(word)))
            }
        }
    }
}

pub enum InterpreterStatus {
    Ok,
    Error(InterpreterError),
}
impl Display for InterpreterStatus {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match &self {
            InterpreterStatus::Ok => write!(f, "ok"),
            InterpreterStatus::Error(error) => write!(f, "{}", error),
        }
    }
}
pub enum InterpreterError {
    Overflow,
    Underflow,
}
impl Display for InterpreterError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match &self {
            InterpreterError::Overflow => write!(f, "Stack Overflow"),
            InterpreterError::Underflow => write!(f, "Stack Underflow"),
        }
    }
}

pub struct Interpreter {
    pub stack: VecDeque<Token>,
    words_dict: HashMap<String, Vec<Token>>,
}

impl Interpreter {
    pub fn new() -> Self {
        Self {
            stack: VecDeque::new(),
            words_dict: HashMap::new(),
        }
    }
    pub fn proccess_token(
        &mut self,
        tokens: Vec<Token>,
        output: &mut String,
    ) -> Result<InterpreterStatus, InterpreterStatus> {
        let mut status = InterpreterStatus::Ok;
        let mut token_iter = tokens.into_iter();
        while let Some(token) = token_iter.next() {
            match token {
                Token::Number(_) => self.stack.push_back(token.clone()),
                Token::Colon => {
                    if let Some(Token::Word(word)) = token_iter.next() {
                        let procedure = token_iter
                            .by_ref()
                            .take_while(|t| *t != Token::Semicolon)
                            .collect::<Vec<_>>();

                        self.words_dict.insert(word, procedure);
                    }
                }
                Token::ArithmeticOperators(op) => match op {
                    ArithmeticOperators::Plus => {
                        let left = self
                            .stack
                            .pop_back()
                            .ok_or(InterpreterStatus::Error(InterpreterError::Underflow))?;

                        let right = self
                            .stack
                            .pop_back()
                            .ok_or(InterpreterStatus::Error(InterpreterError::Underflow))?;

                        match (left, right) {
                            (Token::Number(l), Token::Number(r)) => {
                                let res = match (l, r) {
                                    (Number::SignedInteger(l), Number::SignedInteger(r)) => {
                                        let result = l + r;
                                        Token::Number(Number::SignedInteger(result))
                                    }
                                    (Number::SignedInteger(_), Number::Float(_)) => todo!(),

                                    (Number::Float(_), Number::SignedInteger(_)) => todo!(),
                                    (Number::Float(l), Number::Float(r)) => {
                                        let result = l + r;
                                        Token::Number(Number::Float(result))
                                    }
                                };

                                self.stack.push_back(res);
                            }
                            _ => todo!(),
                        }
                    }
                    ArithmeticOperators::Minus => todo!(),
                },
                Token::Word(word) => match word.as_str() {
                    "dup" => {
                        let dup = self
                            .stack
                            .pop_back()
                            .ok_or(InterpreterStatus::Error(InterpreterError::Underflow))?;
                        self.stack.push_back(dup.clone());
                        self.stack.push_back(dup);
                    }
                    "drop" => {
                        let _ = self
                            .stack
                            .pop_back()
                            .ok_or(InterpreterStatus::Error(InterpreterError::Underflow))?;
                    }
                    "swap" => {
                        let s1 = self
                            .stack
                            .pop_back()
                            .ok_or(InterpreterStatus::Error(InterpreterError::Underflow))?;
                        let s2 = self
                            .stack
                            .pop_back()
                            .ok_or(InterpreterStatus::Error(InterpreterError::Underflow))?;

                        self.stack.push_back(s1);
                        self.stack.push_back(s2);
                    }

                    "over" => {
                        let s1 = self
                            .stack
                            .pop_back()
                            .ok_or(InterpreterStatus::Error(InterpreterError::Underflow))?;
                        let s2 = self
                            .stack
                            .pop_back()
                            .ok_or(InterpreterStatus::Error(InterpreterError::Underflow))?;

                        self.stack.push_back(s2.clone());
                        self.stack.push_back(s1);
                        self.stack.push_back(s2);
                    }

                    "rot" => {
                        let r1 = self
                            .stack
                            .pop_back()
                            .ok_or(InterpreterStatus::Error(InterpreterError::Underflow))?;
                        let r2 = self
                            .stack
                            .pop_back()
                            .ok_or(InterpreterStatus::Error(InterpreterError::Underflow))?;
                        let r3 = self
                            .stack
                            .pop_back()
                            .ok_or(InterpreterStatus::Error(InterpreterError::Underflow))?;

                        self.stack.push_back(r2);
                        self.stack.push_back(r1);
                        self.stack.push_back(r3);
                    }

                    "emit" => {
                        let ascii = self
                            .stack
                            .pop_back()
                            .ok_or(InterpreterStatus::Error(InterpreterError::Underflow))?;

                        if let Token::Number(number) = ascii {
                            let letter = match number {
                                Number::SignedInteger(number) => {
                                    char::from_u32(number as u32).unwrap()
                                }
                                Number::Float(_) => todo!(),
                            };
                            output.push(letter);
                        }
                    }
                    _ => {
                        println!("{}", word);
                        let procedure = self.words_dict.get(&word).unwrap().clone();
                        let result = self.proccess_token(procedure, output);
                    }
                },
                Token::Semicolon => todo!(),
                Token::Dot => {
                    let out = self
                        .stack
                        .pop_back()
                        .ok_or(InterpreterStatus::Error(InterpreterError::Underflow))?;
                    output.push_str(format!("{out} ").as_str());
                }
                Token::ApostroColon(out) => {
                    output.push_str(out.as_str());
                }
                Token::Equal => {
                    let eq1 = self
                        .stack
                        .pop_back()
                        .ok_or(InterpreterStatus::Error(InterpreterError::Underflow))?;
                    let eq2 = self
                        .stack
                        .pop_back()
                        .ok_or(InterpreterStatus::Error(InterpreterError::Underflow))?;

                    if let (Token::Number(l), Token::Number(r)) = (eq1, eq2) {
                        match (l, r) {
                            (Number::SignedInteger(l), Number::SignedInteger(r)) => {
                                let res = if l == r { -1 } else { 0 };
                                self.stack
                                    .push_back(Token::Number(Number::SignedInteger(res)))
                            }
                            (Number::SignedInteger(l), Number::Float(r)) => {
                                let res = if l as f64 == r { -1 } else { 0 };
                                self.stack
                                    .push_back(Token::Number(Number::SignedInteger(res)));
                            }
                            (Number::Float(l), Number::SignedInteger(r)) => {
                                let res = if l == r as f64 { -1 } else { 0 };
                                self.stack
                                    .push_back(Token::Number(Number::SignedInteger(res)));
                            }
                            (Number::Float(l), Number::Float(r)) => {
                                let res = if l == r { -1 } else { 0 };
                                self.stack
                                    .push_back(Token::Number(Number::SignedInteger(res)));
                            }
                        }
                    }
                }
            }
        }
        Ok(status)
    }
}
