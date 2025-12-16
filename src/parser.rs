use nom::{
    AsChar, IResult, Parser,
    bytes::complete::take_while,
    character::complete::anychar,
    combinator::{eof, map, opt, peek},
};

use crate::token::{Text, Sym, Token};

