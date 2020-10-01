use crate::error::{ParseError, Result};
use crate::lexer::Lexer;
use crate::token::Token;
use crate::traits::{Parse, Peek};

#[derive(Debug, Clone)]
pub struct Parser<'a> {
    pub(crate) lexer: Lexer<'a>,
    p1: Result<Option<Token>, ParseError>,
    p2: Result<Option<Token>, ParseError>,
}

impl<'a> Parser<'a> {
    pub fn new(source: &'a str) -> Self {
        let mut lexer = Lexer::new(source);

        let p1 = lexer.next();
        let p2 = lexer.next();

        Self { lexer, p1, p2 }
    }

    pub(crate) fn is_eof(&self) -> Result<bool, ParseError> {
        Ok(self.p1?.is_none())
    }

    pub fn parse<T>(&mut self) -> Result<T, ParseError>
    where
        T: Parse,
    {
        T::parse(self)
    }

    pub fn peek<T>(&self) -> Result<bool, ParseError>
    where
        T: Peek,
    {
        Ok(T::peek(self.p1?))
    }

    pub fn peek2<T>(&self) -> Result<bool, ParseError>
    where
        T: Peek,
    {
        Ok(T::peek(self.p2?))
    }

    pub(crate) fn token_peek(&mut self) -> Result<Option<Token>, ParseError> {
        self.p1
    }

    pub(crate) fn token_peek2(&mut self) -> Result<Option<Token>, ParseError> {
        self.p2
    }

    pub(crate) fn token_next(&mut self) -> Result <Token, ParseError> {
        let old = std::mem::replace(&mut self.p2, self.lexer.next());
        let token = std::mem::replace(&mut self.p1, old);
        
        match token? {
            Some(token) => Ok(token),
            None => Err(ParseError::UnexpectedEof {
                span: self.lexer.end(),
            }),
        }
    }

    pub(crate) fn token_peek_eof(&mut self) -> Result<Token, ParseError> {
        match self.p1? {
            Some(token) => Ok(token),
            None => Err(ParseError::UnexpectedEof {
                span: self.lexer.end(),
            }),
        }
    }
}
