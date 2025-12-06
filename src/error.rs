use std::num::{ParseFloatError, ParseIntError};

use thiserror::Error;

#[derive(Error, Debug)]
pub enum LexerError {
    #[error("unexpected end of query")]
    UnexpectedEOF,

    #[error("unexpected symbol '{0}'")]
    UnexpectedSymbol(String),

    #[error("malformed flaoting number: {0:?}")]
    MalformedFloatingNumber(Option<ParseFloatError>),

    #[error("malformed integral number: {0}")]
    MalformedIntegralNumber(ParseIntError),

    #[error("string literal is not closed properly")]
    StringLiteralNotClosed,
}
