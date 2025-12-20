use nom_locate::LocatedSpan;
use serde::Serialize;
use std::fmt::{Display, Formatter};

#[derive(Clone, Debug, Copy, Serialize)]
pub enum Sym<'a> {
    Id(&'a str),
    String(&'a str),
    Number(f64),
    Keyword(&'a str),
    Operator(Operator),
    Symbol(Symbol),
    Eof,
}

impl Display for Sym<'_> {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            Sym::Id(id) => write!(f, "{}", id),
            Sym::String(s) => write!(f, "\"{}\"", s),
            Sym::Number(n) => write!(f, "{}", n),
            Sym::Keyword(kw) => write!(f, "{}", kw.to_uppercase()),
            Sym::Operator(op) => write!(f, "{}", op),
            Sym::Symbol(sym) => write!(f, "{}", sym),
            Sym::Eof => write!(f, "<eof>"),
        }
    }
}

#[derive(Clone, Debug, Copy, Serialize)]
pub enum Operator {
    Add,
    Sub,
    Mul,
    Div,
    Eq,
    Neq,
    Lt,
    Lte,
    Gt,
    Gte,
    And,
    Or,
    Xor,
    Not,
}

impl Display for Operator {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            Operator::Add => write!(f, "+"),
            Operator::Sub => write!(f, "-"),
            Operator::Mul => write!(f, "*"),
            Operator::Div => write!(f, "/"),
            Operator::Eq => write!(f, "=="),
            Operator::Neq => write!(f, "!="),
            Operator::Lt => write!(f, "<"),
            Operator::Lte => write!(f, "<="),
            Operator::Gt => write!(f, ">"),
            Operator::Gte => write!(f, ">="),
            Operator::And => write!(f, "AND"),
            Operator::Or => write!(f, "OR"),
            Operator::Xor => write!(f, "XOR"),
            Operator::Not => write!(f, "NOT"),
        }
    }
}

#[derive(Clone, Debug, Copy, PartialEq, Eq, Serialize)]
pub enum Symbol {
    OpenParen,
    CloseParen,
    Dot,
    Comma,
    Colon,
    OpenBracket,
    CloseBracket,
    OpenBrace,
    CloseBrace,
}

impl Display for Symbol {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            Symbol::OpenParen => write!(f, "("),
            Symbol::CloseParen => write!(f, ")"),
            Symbol::Dot => write!(f, "."),
            Symbol::Comma => write!(f, ","),
            Symbol::Colon => write!(f, ":"),
            Symbol::OpenBracket => write!(f, "["),
            Symbol::CloseBracket => write!(f, "]"),
            Symbol::OpenBrace => write!(f, "{{"),
            Symbol::CloseBrace => write!(f, "}}"),
        }
    }
}

pub type Text<'a> = LocatedSpan<&'a str>;

#[derive(Debug, Clone, Copy, Serialize)]
pub struct Token<'a> {
    pub sym: Sym<'a>,
    pub line: u32,
    pub col: u32,
}
