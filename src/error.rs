use crate::token::Kind;
use thiserror::Error;

pub type Result<T, E = Error> = std::result::Result<T, E>;

#[derive(Debug, Error)]
pub enum Error {
    /// Source parse error.
    #[error("parse error")]
    ParseError(#[from] ParseError),
}

#[derive(Debug, Clone, Copy, Error)]
pub enum ParseError {
    #[error("unexpected end-of-file")]
    UnexpectedEof,
    #[error("unexpected character `{c}`")]
    UnexpectedChar {
        c: char,
    },
    #[error("token mismatch, expected `{expected}` but was `{actual}`")]
    TokenMismatch {
        expected: Kind,
        actual: Kind,
    }
}

