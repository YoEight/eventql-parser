use crate::token::{Symbol, Token};
use std::fmt::{Display, Formatter};
use thiserror::Error;

#[derive(Debug, Error)]
pub enum Error<'a> {
    #[error("{0}")]
    Parser(ParserError<'a>),
}

#[derive(Debug)]
pub enum ParserError<'a> {
    ExpectedIdent(Token<'a>),
    ExpectedKeyword(&'static str, Token<'a>),
    ExpectedSymbol(Symbol, Token<'a>),
    UnexpectedToken(Token<'a>),
    UnexpectedEof,
}

impl Display for ParserError<'_> {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            ParserError::ExpectedIdent(t) => {
                write!(
                    f,
                    "{}:{}: expected identifier, but got {}",
                    t.line, t.col, t.sym
                )
            }

            ParserError::ExpectedKeyword(k, t) => {
                write!(
                    f,
                    "{}:{}: expected keyword {}, but got {}",
                    k.to_uppercase(),
                    t.line,
                    t.col,
                    t.sym
                )
            }

            ParserError::ExpectedSymbol(s, t) => {
                write!(f, "{}:{}: expected {}, but got {}", s, t.line, t.col, t.sym)
            }

            ParserError::UnexpectedToken(t) => {
                write!(f, "{}:{}: unexpected token {}", t.line, t.col, t.sym)
            }

            ParserError::UnexpectedEof => {
                write!(f, "unexpected end of file")
            }
        }
    }
}
