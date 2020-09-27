use crate::error::ParseError;
use crate::token::{Kind, Token, NumberLiteral};

#[derive(Debug, Clone)]
pub struct Lexer<'a> {
    cursor: usize,
    source: &'a str,
}

impl<'a> Lexer<'a> {
    pub fn new(source: &'a str) -> Self {
        Self { cursor:0, source }
    }

    fn next_number_literal<I>(
        &mut self,
        it: &mut I,
        start: usize,
    ) -> Result<Option<Token>, ParseError> 
    where
    I: Clone + Iterator<Item = (usize, char)>,
    {
        let number = match it.clone().next() {
            _ => NumberLiteral::Decimal,
        };
        self.cursor = loop {
            break match it.next() {
                Some((n, c)) => {
                    if char::is_alphanumeric(c) {
                        continue;
                    } else {
                        self.cursor + n
                    }
                }
                None => self.source.len(),
            };
        };
        return Ok(Some(Token {
            kind: Kind::NumberLiteral { number },
        }));

    }

    pub fn next(&mut self) -> Result<Option<Token>, ParseError> {
        let mut it = self.source.char_indices();

        while let Some((start, c)) = it.next() {
            if char::is_whitespace(c) {
                continue;
            }

            let kind = loop {
                break match c {
                    '+' => Kind::Plus,
                    '-' => Kind::Minus,
                    '/' => Kind::Slash,
                    '*' => Kind::Star,
                    '0'..='9' => {
                        return self.next_number_literal(&mut it, start);
                    }
                    _ => {
                        return Err(ParseError::UnexpectedChar { c });
                    }
                };
            };

            return Ok(Some(Token { kind }));
        }

        Ok(None)
    }
}
