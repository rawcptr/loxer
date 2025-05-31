#![warn(
    clippy::pedantic,
    clippy::nursery,
    clippy::correctness,
    clippy::complexity,
    clippy::suspicious,
    clippy::perf,
    clippy::style
)]

use std::{
    borrow::Cow,
    fmt::{self},
};

use miette::{Error, LabeledSpan};

#[derive(Debug, PartialEq, Clone)]
pub struct Token<'de> {
    kind: TokenKind,
    origin: &'de str,
}

#[derive(Debug, PartialEq, Clone)]
pub enum TokenKind {
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
    BangEqual,
    EqualEqual,
    LessEqual,
    GreaterEqual,
    Less,
    Greater,
    Slash,
    Bang,
    Equal,
    String,
    Ident,
    Number(f64),
    And,
    Class,
    Else,
    False,
    For,
    Fun,
    If,
    Nil,
    Or,
    Return,
    Super,
    This,
    True,
    Var,
    While,
}

impl fmt::Display for Token<'_> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        use TokenKind::*;
        let i = self.origin;
        match self.kind {
            LeftParen => write!(f, "LEFT_PAREN {i} null"),
            RightParen => write!(f, "RIGHT_PAREN {i} null"),
            LeftBrace => write!(f, "LEFT_BRACE {i} null"),
            RightBrace => write!(f, "RIGHT_BRACE {i} null"),
            Comma => write!(f, "COMMA {i} null"),
            Dot => write!(f, "DOT {i} null"),
            Minus => write!(f, "MINUS {i} null"),
            Plus => write!(f, "PLUS {i} null"),
            SemiColon => write!(f, "SEMICOLON {i} null"),
            Star => write!(f, "STAR {i} null"),
            BangEqual => write!(f, "BANG_EQUAL {i} null"),
            EqualEqual => write!(f, "EQUAL_EQUAL {i} null"),
            LessEqual => write!(f, "LESS_EQUAL {i} null"),
            GreaterEqual => write!(f, "GREATER_EQUAL {i} null"),
            Less => write!(f, "LESS {i} null"),
            Greater => write!(f, "GREATER {i} null"),
            Slash => write!(f, "SLASH {i} null"),
            Bang => write!(f, "BANG {i} null"),
            Equal => write!(f, "EQUAL {i} null"),
            String => write!(f, "STRING \"{i}\" {}", Token::unescape(i)),
            Ident => write!(f, "IDENTIFIER {i} null"),
            Number(n) => write!(f, "NUMBER {i} {n}"),
            And => write!(f, "AND {i} null"),
            Class => write!(f, "CLASS {i} null"),
            Else => write!(f, "ELSE {i} null"),
            False => write!(f, "FALSE {i} null"),
            For => write!(f, "FOR {i} null"),
            Fun => write!(f, "FUN {i} null"),
            If => write!(f, "IF {i} null"),
            Nil => write!(f, "NIL {i} null"),
            Or => write!(f, "OR {i} null"),
            Return => write!(f, "RETURN {i} null"),
            Super => write!(f, "SUPER {i} null"),
            This => write!(f, "THIS {i} null"),
            True => write!(f, "TRUE {i} null"),
            Var => write!(f, "VAR {i} null"),
            While => write!(f, "WHILE {i} null"),
        }
    }
}

pub struct Lexer<'de> {
    whole: &'de str,
    rest: &'de str,
    byte: usize,
}

impl Token<'_> {
    pub fn unescape(s: &'_ str) -> Cow<'_, str> {
        // no unescaping in lox
        Cow::Borrowed(s.trim_matches('"'))
    }
}

impl<'de> Lexer<'de> {
    pub const fn new(input: &'de str) -> Self {
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
        use TokenKind::*;
        enum Started {
            String,
            Number,
            Ident,
            IfEqualElse(TokenKind, TokenKind),
            Slash,
        }

        loop {
            let mut chars = self.rest.chars();
            let c = chars.next()?;
            let c_str = &self.rest[..c.len_utf8()];
            let c_onwards = self.rest;
            self.rest = chars.as_str();
            self.byte += c.len_utf8();

            let just = move |kind: TokenKind| {
                Some(Ok(Token {
                    kind,
                    origin: c_str,
                }))
            };

            let started = match c {
                '(' => return just(LeftParen),
                ')' => return just(RightParen),
                '{' => return just(LeftBrace),
                '}' => return just(RightBrace),
                ',' => return just(Comma),
                '.' => return just(Dot),
                '-' => return just(Minus),
                '+' => return just(Plus),
                ';' => return just(SemiColon),
                '*' => return just(Star),
                '/' => Started::Slash,
                '"' => Started::String,
                '!' => Started::IfEqualElse(BangEqual, Bang),
                '>' => Started::IfEqualElse(GreaterEqual, Greater),
                '<' => Started::IfEqualElse(LessEqual, Less),
                '=' => Started::IfEqualElse(EqualEqual, Equal),
                '0'..='9' => Started::Number,
                'a'..='z' | 'A'..='Z' | '_' => Started::Ident,
                c if c.is_whitespace() => continue,
                _ => {
                    return Some(Err(miette::miette!(
                        labels = vec![LabeledSpan::at(
                            self.byte - c.len_utf8()..self.byte,
                            "this string literal"
                        )],
                        "unexpected token '{c}' in input"
                    )
                    .with_source_code(self.whole.to_string())));
                }
            };

            break match started {
                Started::String => {
                    if let Some(end) = self.rest.find('"') {
                        let literal = &c_onwards[..end + 1 + 1];
                        self.byte += end + 1;
                        self.rest = &self.rest[end + 1..];
                        Some(Ok(Token {
                            origin: literal,
                            kind: TokenKind::String,
                        }))
                    } else {
                        self.byte += self.rest.len();
                        self.rest = &self.rest[self.rest.len()..];
                        Some(Err(miette::miette!(
                            labels = vec![LabeledSpan::at(
                                self.byte - c.len_utf8()..self.whole.len(),
                                "here"
                            )],
                            "unterminated literal"
                        )
                        .with_source_code(self.whole.to_string())))
                    }
                }
                Started::Slash => {
                    if self.rest.starts_with('/') {
                        let line_end = self.rest.find('\n').unwrap_or(self.rest.len());
                        self.byte += line_end;
                        self.rest = &self.rest[line_end..];
                        continue;
                    } else {
                        Some(Ok(Token {
                            kind: Slash,
                            origin: c_str,
                        }))
                    }
                }
                Started::Number => {
                    let fst_non_digit = c_onwards
                        .find(|c| !matches!(c, '.' | '0'..='9'))
                        .unwrap_or(c_onwards.len());
                    let mut literal = &c_onwards[..fst_non_digit];
                    let mut dotted = literal.splitn(3, '.');
                    match (dotted.next(), dotted.next(), dotted.next()) {
                        (Some(one), Some(two), Some(_)) => {
                            literal = &literal[..one.len() + 1 + two.len()];
                        }
                        (Some(one), Some(""), None) => literal = &literal[..one.len()],
                        _ => (), // skip literal
                    }
                    let extra_bytes = literal.len() - c.len_utf8();
                    self.byte += extra_bytes;
                    self.rest = &self.rest[extra_bytes..];

                    let Ok(n) = literal.parse() else {
                        return Some(Err(miette::miette!(
                            labels = vec![LabeledSpan::at(
                                self.byte - c.len_utf8()..self.byte,
                                "this numeric literal"
                            )],
                            "failed conversion to float"
                        )
                        .with_source_code(self.whole.to_string())));
                    };

                    return Some(Ok(Token {
                        kind: Number(n),
                        origin: literal,
                    }));
                }
                Started::Ident => {
                    let fst_non_ident = c_onwards
                        .find(|c| !matches!(c, 'a'..='z' | 'A'..='Z' | '0'..='9'))
                        .unwrap_or(c_onwards.len());
                    let literal = &c_onwards[..fst_non_ident];
                    let extra_bytes = literal.len() - c.len_utf8();
                    self.byte += extra_bytes;
                    self.rest = &self.rest[extra_bytes..];

                    let kind = match literal {
                        "and" => And,
                        "class" => Class,
                        "else" => Else,
                        "false" => False,
                        "for" => For,
                        "fun" => Fun,
                        "if" => If,
                        "nil" => Nil,
                        "or" => Or,
                        "return" => Return,
                        "super" => Super,
                        "this" => This,
                        "true" => True,
                        "var" => Var,
                        "while" => While,
                        _ => Ident,
                    };

                    return Some(Ok(Token {
                        kind,
                        origin: literal,
                    }));
                }
                Started::IfEqualElse(yes, no) => {
                    self.rest = self.rest.trim_start();
                    let trimmed = c_onwards.len() - self.rest.len() - 1;
                    self.byte += trimmed;

                    if self.rest.starts_with('=') {
                        let span = &c_onwards[..=c.len_utf8() + trimmed];
                        self.byte += 1;
                        self.rest = &self.rest[1..];
                        Some(Ok(Token {
                            origin: span,
                            kind: yes,
                        }))
                    } else {
                        just(no)
                    }
                }
            };
        }
    }
}
