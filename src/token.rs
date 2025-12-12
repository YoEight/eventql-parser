use std::fmt::Display;

use nom_locate::LocatedSpan;

#[derive(Clone, Debug, Copy, PartialEq, Eq)]
pub enum Sym {
    Id,
    Integer,
    Float,
    Keyword,
    Operator,
    Symbol,
    Eof,
}

impl Display for Sym {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Sym::Id => write!(f, "id"),
            Sym::Integer => write!(f, "integer"),
            Sym::Float => write!(f, "float"),
            Sym::Keyword => write!(f, "keyword"),
            Sym::Operator => write!(f, "operator"),
            Sym::Symbol => write!(f, "symbol"),
            Sym::Eof => write!(f, "<eof>"),
        }
    }
}

pub type Pos<'a> = LocatedSpan<&'a str>;

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct Token<'a> {
    pub sym: Sym,
    pub pos: Pos<'a>,
    pub value: &'a str,
}

impl<'a> Token<'a> {
    pub fn new(sym: Sym, pos: Pos<'a>, value: &'a str) -> Self {
        Self { sym, pos, value }
    }
}
