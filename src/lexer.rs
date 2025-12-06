use crate::token::Pos;

#[derive(Copy, Clone)]
pub struct Text<'a> {
    pub text: &'a str,
    pub pos: Pos,
}

impl<'a> Text<'a> {
    pub fn new(text: &'a str) -> Self {
        Self { text, pos: 0 }
    }

    pub fn peek(self) -> (Option<&'a str>, Self) {
        if self.pos >= self.text.len() {
            return (None, self);
        }

        (Some(&self.text[self.pos..=self.pos]), self)
    }

    pub fn pos(&self) -> usize {
        self.pos
    }

    pub fn shift(mut self) -> (Option<&'a str>, Self) {
        if self.pos >= self.text.len() {
            return (None, self);
        }

        let value = &self.text[self.pos..=self.pos];
        self.pos += 1;

        (Some(value), self)
    }

    pub fn skip_while(self, fun: impl Fn(char) -> bool) -> Self {
        let mut current = self;

        while let (Some(c), _) = current.peek()
            && fun(c.as_bytes()[0] as char)
        {
            let (_, next) = current.shift();
            current = next;
        }

        current
    }

    pub fn take_while(self, fun: impl Fn(char) -> bool) -> (&'a str, Self) {
        let pos = self.pos;
        let next = self.skip_while(fun);

        (next.str(pos), next)
    }

    pub fn skip_whitespaces(self) -> Self {
        self.skip_while(|c| c.is_ascii_whitespace())
    }

    pub fn str(self, from: usize) -> &'a str {
        &self.text[from..self.pos]
    }
}
