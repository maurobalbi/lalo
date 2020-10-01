use crate::error::{ParseError, Result};
use crate::parser::Parser;
use crate::token::Token;

/// The parse trait, implemented by items that can be parsed.
pub trait Parse
where
    Self: Sized,
{
    /// Parse the current item from the parser.
    fn parse(parser: &mut Parser) -> Result<Self, ParseError>;
}

/// Implemented by tokens that can be peeked for.
pub trait Peek {
    /// Peek the parser for the given token.
    fn peek(token: Option<Token>) -> bool;
}