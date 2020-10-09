pub mod ast;
mod error;
mod lexer;
mod parser;
mod source;
mod token;
mod traits;

pub use crate::error::ParseError;
pub use crate::lexer::Lexer;
pub use crate::parser::Parser;
pub use crate::source::Source;

#[derive(Debug)]
pub struct ParseAll<'a, T> {
    /// The source parsed.
    ///
    /// Is needed to resolve things on the item through [Resolve::resolve]
    /// later.
    pub source: Source<'a>,
    /// The item parsed.
    pub item: T,
}

/// Parse the given input as the given type that implements
/// [Parse][crate::traits::Parse].
///
/// This required the whole input to be parsed.
///
/// Returns the wrapped source and the parsed type.
pub fn parse_all<'a, T>(source: &'a str) -> Result<ParseAll<T>, ParseError>
where
    T: crate::traits::Parse,
{
    let mut parser = Parser::new(source);
    let ast = parser.parse::<T>()?;

    parser.eof()?;

    Ok(ParseAll {
        source: Source { source },
        item: ast,
    })
}
