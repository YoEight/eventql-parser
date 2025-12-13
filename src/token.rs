use nom_locate::LocatedSpan;

#[derive(Clone, Debug, Copy)]
pub enum Sym<'a> {
    Id(&'a str),
    Integer(i64),
    Float(f64),
    Keyword(&'a str),
    Operator(Operator),
    Symbol(Symbol),
    Eof,
}

#[derive(Clone, Debug, Copy)]
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
}

#[derive(Clone, Debug, Copy)]
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

// impl Display for Sym {
//     fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
//         match self {
//             Sym::Id => write!(f, "id"),
//             Sym::Integer => write!(f, "integer"),
//             Sym::Float => write!(f, "float"),
//             Sym::Keyword => write!(f, "keyword"),
//             Sym::Operator => write!(f, "operator"),
//             Sym::Symbol => write!(f, "symbol"),
//             Sym::Eof => write!(f, "<eof>"),
//         }
//     }
// // }

pub type Pos<'a> = LocatedSpan<&'a str>;

#[derive(Debug, Clone, Copy)]
pub struct Token<'a> {
    pub sym: Sym<'a>,
    pub line: u32,
    pub col: u32,
}
