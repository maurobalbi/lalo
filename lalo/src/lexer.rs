use crate::error::ParseError;
use crate::token::{Kind, Span, NumberLiteral, Token};

#[derive(Debug, Clone)]
pub struct Lexer<'a> {
    cursor: usize,
    source: &'a str,
}

impl<'a> Lexer<'a> {
    pub fn new(source: &'a str) -> Self {
        Self { cursor: 0, source }
    }

    /// Calculate the end span by peeking the next token.
    fn end_span<I, T>(&self, it: &I) -> usize
    where
        I: Iterator<Item = (usize, T)> + Clone,
    {
        it.clone()
            .next()
            .map(|(n, _)| self.cursor + n)
            .unwrap_or(self.source.len())
    }

    /// Access the end span of the input.
    pub fn end(&self) -> Span {
        Span::point(self.source.len())
    }

    fn next_ident<I>(
        &mut self, 
        it: &mut I,
        start: usize
    ) -> Result<Option<Token>, ParseError>
    where
        I: Clone + Iterator<Item = (usize, char)>,
    {
        self.cursor = loop {
            break match it.clone().next() {
                Some((n, c)) => match c {
                    'a'..='z' | 'A'..='Z' | '_' | '0'..='9' => {
                        it.next();
                        continue;
                    }
                    _ => self.cursor + n,
                },
                None => self.source.len(),
            };
        };

        let ident = &self.source[start..self.cursor];

        let kind = match ident {
            "let" => Kind::Let,
            "if" => Kind::If,
            "then" => Kind::Then,
            "else" => Kind::Else,
            "true" => Kind::True,
            "false" => Kind::False,
            _ => Kind::Ident,
        };

        Ok(Some(Token {
            kind,
            span: Span {
                start,
                end: self.cursor,
            },
        }))
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
            span: Span {
                start,
                end: self.cursor,
            }
        }));
    }

    pub fn next(&mut self) -> Result<Option<Token>, ParseError> {
        let mut it = self.source[self.cursor..].char_indices();

        while let Some((start, c)) = it.next() {
            let start = self.cursor + start;

            if char::is_whitespace(c) {
                continue;
            }

            let kind = loop {
                if let Some(c2) = it.clone().next().map(|(_, c)| c) {
                    match (c, c2) {
                        ('=', '>') => {
                            it.next();
                            break Kind::Arrow;
                        },
                        _ => (),
                    }
                }

                break match c {
                    '+' => Kind::Plus,
                    '-' => Kind::Minus,
                    '/' => Kind::Slash,
                    '*' => Kind::Star,
                    '\\' => Kind::BackSlash,
                    '0'..='9' => {
                        return self.next_number_literal(&mut it, start);
                    },
                    'a'..='y' | 'A'..='Z' => {
                        return self.next_ident(&mut it, start);
                    },
                    '=' => Kind::Eq,
                    _ => {
                        let span = Span {
                            start,
                            end: self.end_span(&mut it)
                        };

                        return Err(ParseError::UnexpectedChar { span, c });
                    }
                };
            };

            self.cursor = self.end_span(&it);

            return Ok(Some(Token { 
                kind,
                span: Span {
                    start,
                    end: self.cursor
                }            
             }));
        }

        self.cursor = self.source.len();
        print!("{:?}", self.cursor);
        Ok(None)
    }
}
