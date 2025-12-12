use crate::token::{Pos, Sym, Token};
use nom::bytes::complete::take_while;
use nom::bytes::take;
use nom::character::char;
use nom::character::complete::anychar;
use nom::combinator::{eof, fail, map, opt, peek};
use nom::error::context;
use nom::{AsChar, IResult, Parser};

pub fn next_token(pos: Pos) -> IResult<Pos, Token> {
    let (pos, _) = take_while(|c: char| c.is_ascii_whitespace())(pos)?;
    let (_, is_eof) = map(opt(eof), |res| res.is_some()).parse(pos)?;

    if is_eof {
        return Ok((pos, Token::new(Sym::Eof, pos, "")));
    }

    let (pos, c) = peek(anychar).parse(pos)?;

    if matches!(c, '(' | ')' | '.' | ',' | ':' | '[' | ']' | '{' | '}') {
        let (pos, value) = take(1usize).parse(pos)?;
        return Ok((pos, Token::new(Sym::Symbol, pos, value.fragment())));
    }

    if matches!(c, '+' | '-' | '*' | '/' | '=' | '^') {
        let (pos, value) = take(1usize).parse(pos)?;
        return Ok((pos, Token::new(Sym::Operator, pos, value.fragment())));
    }

    if matches!(c, '<' | '>') {
        let (tmp, _) = anychar(pos)?;
        let (_, ok) = opt(char('=')).parse(tmp)?;

        let (pos, value) = if ok.is_some() {
            take(2usize).parse(pos)?
        } else {
            take(1usize).parse(pos)?
        };

        return Ok((tmp, Token::new(Sym::Operator, pos, value.fragment())));
    }

    if matches!(c, '!') {
        let (pos, _) = anychar(pos)?;
        let (_, ok) = opt(char('=')).parse(pos)?;

        if ok.is_some() {
            failure("invalid symbol '='", pos)?;
        }

        return Ok((pos, Token::new(Sym::Operator, pos, "!")));
    }

    if c.is_ascii_alphabetic() {
        let (pos, value) = take_while(AsChar::is_alpha).parse(pos)?;

        return Ok((pos, Token::new(Sym::Id, pos, value.fragment())));
    }

    if c.is_ascii_digit() {
        let (pos, value) = take_while(AsChar::is_dec_digit).parse(pos)?;
        return Ok((pos, Token::new(Sym::Integer, pos, value.fragment())));
    }

    failure("unexpected character", pos)
}

fn failure<'a>(reason: &'static str, pos: Pos<'a>) -> IResult<Pos<'a>, Token<'a>> {
    context(reason, fail()).parse(pos)
}
