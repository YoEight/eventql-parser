use std::fmt::Display;

#[derive(Clone, Debug, Copy, PartialEq, Eq)]
pub enum Sym {
    Id,
    Integer,
    Float,
    Keyword,
    Operator,
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
            Sym::Eof => write!(f, "<eof>"),
        }
    }
}

#[derive(Clone, Copy, Debug, PartialEq, Eq, PartialOrd, Ord)]
pub struct Pos {
    pub line: u64,
    pub col: u64,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct Token<'a> {
    pub sym: Sym,
    pub pos: Pos,
    pub value: &'a str,
}

impl<'a> Token<'a> {
    pub fn new(sym: Sym, pos: Pos, value: &'a str) -> Self {
        Self { sym, pos, value }
    }
}
