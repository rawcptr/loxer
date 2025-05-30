use std::{
    borrow::Cow,
    fmt::{self},
};

use miette::{Error, LabeledSpan};

#[derive(Debug, PartialEq, Eq, Hash, Clone)]
pub enum Token<'de> {
    LeftParen,
    RightParen,
    LeftBrace,
    RightBrace,
    Comma,
    Dot,
    Minus,
    Plus,
    SemiColon,
    Star,
    String(&'de str),
}

impl fmt::Display for Token<'_> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(
            f,
            "{}",
            match self {
                Token::LeftParen => "LEFT_PAREN ( null",
                Token::RightParen => "RIGHT_PAREN ) null",
                Token::LeftBrace => "LEFT_BRACE { null",
                Token::RightBrace => "RIGHT_BRACE } null",
                Token::Comma => "COMMA , null",
                Token::Dot => "DOT . null",
                Token::Minus => "MINUS - null",
                Token::Plus => "PLUS + null",
                Token::SemiColon => "SEMICOLON ; null",
                Token::Star => "STAR * null",
                Token::String(s) => return write!(f, "STRING \"{s}\" {}", Token::unescape(s)),
            }
        )
    }
}

pub struct Lexer<'de> {
    whole: &'de str,
    rest: &'de str,
    byte: usize,
}

impl Token<'_> {
    pub fn unescape<'de>(s: &'de str) -> Cow<'de, str> {
        let _ = s;
        todo!()
    }
}

impl<'de> Lexer<'de> {
    pub fn new(input: &'de str) -> Self {
        Self {
            whole: input,
            rest: input,
            byte: 0,
        }
    }
}

impl<'de> Iterator for Lexer<'de> {
    type Item = Result<Token<'de>, Error>;
    fn next(&mut self) -> Option<Self::Item> {
        use Token::*;
        let mut chars = self.rest.chars();
        let c = chars.next()?;
        self.rest = chars.as_str();
        self.byte += c.len_utf8();

        match c {
            '(' => return Some(Ok(LeftParen)),
            ')' => return Some(Ok(RightParen)),
            '{' => return Some(Ok(LeftBrace)),
            '}' => return Some(Ok(RightBrace)),
            ',' => return Some(Ok(Comma)),
            '.' => return Some(Ok(Dot)),
            '-' => return Some(Ok(Minus)),
            '+' => return Some(Ok(Plus)),
            ';' => return Some(Ok(SemiColon)),
            '*' => return Some(Ok(Star)),
            '"' => {}
            _ => {
                return Some(Err(miette::miette!(
                    labels = vec![LabeledSpan::at(self.byte - c.len_utf8()..self.byte, "here")],
                    "unexpected token '{c}' in input"
                )
                .with_source_code(self.whole.to_string())));
            }
        }
        None
    }
}
