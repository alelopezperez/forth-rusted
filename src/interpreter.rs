use std::{
    char,
    collections::{HashMap, VecDeque},
    fmt::Display,
    usize,
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
    GreaterThan,
    If(Vec<Token>, Option<Vec<Token>>),
    LessThan,
    Then,
    Else,
    Constant(String),
    Do(Vec<Token>),
    Key,
    Variable(String),
    Exclamation,
    At,
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
            Token::GreaterThan => todo!(),
            Token::LessThan => todo!(),
            Token::If(_, _) => todo!(),
            Token::Then => todo!(),
            Token::Else => todo!(),
            Token::Constant(_) => todo!(),
            Token::Do(_) => todo!(),
            Token::Key => todo!(),
            Token::Variable(_) => todo!(),
            Token::Exclamation => todo!(),
            Token::At => todo!(),
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
            '>' => Some(Ok(Token::GreaterThan)),
            '!' => Some(Ok(Token::Exclamation)),
            '@' => Some(Ok(Token::At)),
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
            letter if letter.is_whitespace() => self.next(),
            _ => {
                let word = self.parse_word(letter).unwrap();
                if word == "if" {
                    let mut if_true = Vec::new();
                    let mut if_false = Vec::new();
                    while let Some(Ok(tok)) = self.next() {
                        if let Token::Else = tok {
                            while let Some(Ok(tk)) = self.next() {
                                if let Token::Semicolon = tk {
                                    break;
                                } else {
                                    if_false.push(tk);
                                }
                            }
                        } else if let Token::Semicolon = tok {
                            break;
                        } else {
                            if_true.push(tok);
                        }
                    }
                    Some(Ok(Token::If(if_true, Some(if_false))))
                } else if word == "then" {
                    Some(Ok(Token::Then))
                } else if word == "else" {
                    Some(Ok(Token::Else))
                } else if word == "constant" {
                    if let Some(Ok(Token::Word(word))) = self.next() {
                        Some(Ok(Token::Constant(word)))
                    } else {
                        Some(Err(()))
                    }
                } else if word == "do" {
                    let mut do_loop = Vec::new();
                    while let Some(Ok(tok)) = self.next() {
                        if let Token::Semicolon = tok {
                            break;
                        } else if let Token::Word(l) = tok {
                            if l == "loop" {
                                continue;
                            }
                            do_loop.push(Token::Word(l));
                        } else {
                            do_loop.push(tok);
                        }
                    }
                    Some(Ok(Token::Do(do_loop)))
                } else if word == "key" {
                    Some(Ok(Token::Key))
                } else if word == "variable" {
                    if let Some(Ok(Token::Word(variable))) = self.next() {
                        Some(Ok(Token::Variable(variable)))
                    } else {
                        Some(Err(()))
                    }
                } else {
                    Some(Ok(Token::Word(word)))
                }
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
    InvalidAddress,
}
impl Display for InterpreterError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match &self {
            InterpreterError::Overflow => write!(f, "Stack Overflow"),
            InterpreterError::Underflow => write!(f, "Stack Underflow"),
            InterpreterError::InvalidAddress => write!(f, "Invalid Memory Address"),
        }
    }
}

pub struct Interpreter {
    pub stack: VecDeque<Token>,
    words_dict: HashMap<String, Vec<Token>>,
    const_dict: HashMap<String, Token>,
    memory: Vec<Token>,
}

impl Interpreter {
    pub fn new() -> Self {
        Self {
            stack: VecDeque::new(),
            words_dict: HashMap::new(),
            const_dict: HashMap::new(),
            memory: Vec::new(),
        }
    }
    pub fn proccess_token(
        &mut self,
        tokens: Vec<Token>,
        output: &mut String,
        inside: bool,
    ) -> Result<InterpreterStatus, InterpreterStatus> {
        let mut status = InterpreterStatus::Ok;
        let mut token_iter = tokens.into_iter();
        while let Some(token) = token_iter.next() {
            match token {
                Token::Exclamation => {
                    let value = self
                        .stack
                        .pop_back()
                        .ok_or(InterpreterStatus::Error(InterpreterError::Underflow))?;

                    let address = self
                        .stack
                        .pop_back()
                        .ok_or(InterpreterStatus::Error(InterpreterError::Underflow))?;

                    if let Token::Number(Number::SignedInteger(addr)) = address {
                        if addr >= 0 && (addr as usize) < self.memory.len() {
                            self.memory[addr as usize] = value;
                        } else {
                            return Err(InterpreterStatus::Error(InterpreterError::InvalidAddress));
                        }
                    } else {
                        return Err(InterpreterStatus::Error(InterpreterError::InvalidAddress));
                    }
                }
                Token::At => {
                    let address = self
                        .stack
                        .pop_back()
                        .ok_or(InterpreterStatus::Error(InterpreterError::Underflow))?;
                    if let Token::Number(Number::SignedInteger(addr)) = address {
                        if addr >= 0 && (addr as usize) < self.memory.len() {
                            let data = self.memory[addr as usize].clone();
                            self.stack.push_back(data);
                        } else {
                            return Err(InterpreterStatus::Error(InterpreterError::InvalidAddress));
                        }
                    } else {
                        return Err(InterpreterStatus::Error(InterpreterError::InvalidAddress));
                    }
                }
                Token::Variable(variable) => {
                    self.words_dict.insert(
                        variable,
                        vec![Token::Number(Number::SignedInteger(
                            self.memory.len() as i128
                        ))],
                    );
                    self.memory.push(Token::Number(Number::SignedInteger(0)));
                }
                Token::Key => {
                    let mut buf = String::new();
                    std::io::stdin()
                        .read_line(&mut buf)
                        .expect("couldnote read");
                    let key = buf.as_bytes()[0] as i32;
                    self.stack
                        .push_back(Token::Number(Number::SignedInteger(key as i128)));
                }
                Token::Else => {}
                Token::Then => {}
                Token::Do(do_loop) => {
                    if inside {
                        let start = self
                            .stack
                            .pop_back()
                            .ok_or(InterpreterStatus::Error(InterpreterError::Underflow))?;

                        let end = self
                            .stack
                            .pop_back()
                            .ok_or(InterpreterStatus::Error(InterpreterError::Underflow))?;

                        if let (
                            Token::Number(Number::SignedInteger(start)),
                            Token::Number(Number::SignedInteger(end)),
                        ) = (start, end)
                        {
                            self.words_dict.insert("i".to_string(), vec![]);
                            (start..end).for_each(|i| {
                                self.words_dict.insert(
                                    "i".to_string(),
                                    vec![Token::Number(Number::SignedInteger(i))],
                                );
                                do_loop.iter().for_each(|instruction| {
                                    if let Token::Word(s) = instruction {
                                        if s == "i" {
                                            let _ = self.proccess_token(
                                                vec![Token::Number(Number::SignedInteger(i))],
                                                output,
                                                true,
                                            );
                                        } else {
                                            let _ = self.proccess_token(
                                                vec![instruction.clone()],
                                                output,
                                                inside,
                                            );
                                        }
                                    } else {
                                        let _ = self.proccess_token(
                                            vec![instruction.clone()],
                                            output,
                                            inside,
                                        );
                                    }
                                });
                            });
                        }
                    }
                }
                Token::Constant(constant) => {
                    self.words_dict.remove(&constant);
                    let value = self
                        .stack
                        .pop_back()
                        .ok_or(InterpreterStatus::Error(InterpreterError::Underflow))?;

                    self.const_dict.insert(constant, value);
                }
                Token::Number(_) => self.stack.push_back(token.clone()),
                Token::Colon => {
                    if let Some(Token::Word(word)) = token_iter.next() {
                        let procedure = token_iter
                            .by_ref()
                            .take_while(|t| *t != Token::Semicolon)
                            .collect::<Vec<_>>();

                        self.const_dict.remove(&word);
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
                Token::GreaterThan => {
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
                                let res = if l > r { -1 } else { 0 };
                                self.stack
                                    .push_back(Token::Number(Number::SignedInteger(res)))
                            }
                            (Number::SignedInteger(l), Number::Float(r)) => {
                                let res = if l as f64 > r { -1 } else { 0 };
                                self.stack
                                    .push_back(Token::Number(Number::SignedInteger(res)));
                            }
                            (Number::Float(l), Number::SignedInteger(r)) => {
                                let res = if l > r as f64 { -1 } else { 0 };
                                self.stack
                                    .push_back(Token::Number(Number::SignedInteger(res)));
                            }
                            (Number::Float(l), Number::Float(r)) => {
                                let res = if l > r { -1 } else { 0 };
                                self.stack
                                    .push_back(Token::Number(Number::SignedInteger(res)));
                            }
                        }
                    }
                }
                Token::LessThan => {
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
                                let res = if l < r { -1 } else { 0 };
                                self.stack
                                    .push_back(Token::Number(Number::SignedInteger(res)))
                            }
                            (Number::SignedInteger(l), Number::Float(r)) => {
                                let res = if (l as f64) < r { -1 } else { 0 };
                                self.stack
                                    .push_back(Token::Number(Number::SignedInteger(res)));
                            }
                            (Number::Float(l), Number::SignedInteger(r)) => {
                                let res = if l < r as f64 { -1 } else { 0 };
                                self.stack
                                    .push_back(Token::Number(Number::SignedInteger(res)));
                            }
                            (Number::Float(l), Number::Float(r)) => {
                                let res = if l < r { -1 } else { 0 };
                                self.stack
                                    .push_back(Token::Number(Number::SignedInteger(res)));
                            }
                        }
                    }
                }
                Token::If(case_true, case_false) => {
                    if inside {
                        let check = self
                            .stack
                            .pop_back()
                            .ok_or(InterpreterStatus::Error(InterpreterError::Underflow))?;
                        if let Token::Number(Number::SignedInteger(w)) = check {
                            if w != 0 {
                                let _ = self.proccess_token(case_true, output, inside);
                            } else if let Some(case_false) = case_false {
                                let _ = self.proccess_token(case_false, output, inside);
                            }
                        }
                    }
                }
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
                    "cr" => {
                        output.push('\n');
                    }
                    "loop" => {}
                    "mod" => {
                        let s1 = self
                            .stack
                            .pop_back()
                            .ok_or(InterpreterStatus::Error(InterpreterError::Underflow))?;
                        let s2 = self
                            .stack
                            .pop_back()
                            .ok_or(InterpreterStatus::Error(InterpreterError::Underflow))?;

                        if let (Token::Number(l), Token::Number(r)) = (s1, s2) {
                            match (l, r) {
                                (Number::SignedInteger(r), Number::SignedInteger(l)) => {
                                    let res = l % r;
                                    self.stack
                                        .push_back(Token::Number(Number::SignedInteger(res)))
                                }
                                (Number::SignedInteger(l), Number::Float(r)) => {
                                    let res = l % r as i128;
                                    self.stack
                                        .push_back(Token::Number(Number::SignedInteger(res)));
                                }
                                (Number::Float(l), Number::SignedInteger(r)) => {
                                    let res = l as i128 % r;
                                    self.stack
                                        .push_back(Token::Number(Number::SignedInteger(res)));
                                }
                                (Number::Float(l), Number::Float(r)) => {
                                    let res = l as i128 % r as i128;
                                    self.stack
                                        .push_back(Token::Number(Number::SignedInteger(res)));
                                }
                            }
                        }
                    }
                    "and" => {
                        let s1 = self
                            .stack
                            .pop_back()
                            .ok_or(InterpreterStatus::Error(InterpreterError::Underflow))?;
                        let s2 = self
                            .stack
                            .pop_back()
                            .ok_or(InterpreterStatus::Error(InterpreterError::Underflow))?;

                        if let (Token::Number(l), Token::Number(r)) = (s1, s2) {
                            match (l, r) {
                                (Number::SignedInteger(l), Number::SignedInteger(r)) => {
                                    let res = l & r;

                                    self.stack
                                        .push_back(Token::Number(Number::SignedInteger(res)))
                                }
                                _ => {}
                            }
                        }
                    }
                    "or" => {
                        let s1 = self
                            .stack
                            .pop_back()
                            .ok_or(InterpreterStatus::Error(InterpreterError::Underflow))?;
                        let s2 = self
                            .stack
                            .pop_back()
                            .ok_or(InterpreterStatus::Error(InterpreterError::Underflow))?;

                        if let (Token::Number(l), Token::Number(r)) = (s1, s2) {
                            match (l, r) {
                                (Number::SignedInteger(l), Number::SignedInteger(r)) => {
                                    let res = l | r;

                                    self.stack
                                        .push_back(Token::Number(Number::SignedInteger(res)))
                                }
                                _ => {}
                            }
                        }
                    }
                    "invert" => {
                        let s1 = self
                            .stack
                            .pop_back()
                            .ok_or(InterpreterStatus::Error(InterpreterError::Underflow))?;

                        if let Token::Number(l) = s1 {
                            match l {
                                Number::SignedInteger(l) => {
                                    let res = !l;

                                    self.stack
                                        .push_back(Token::Number(Number::SignedInteger(res)))
                                }
                                Number::Float(l) => {
                                    let res = l as i128;
                                }
                            }
                        }
                    }
                    _ => {
                        if let Some(constant) = self.const_dict.get(&word) {
                            self.stack.push_back(constant.clone());
                        } else {
                            let procedure = self.words_dict.get(&word).unwrap().clone();
                            let result = self.proccess_token(procedure, output, true);
                        }
                    }
                },
            }
        }
        Ok(status)
    }
}
