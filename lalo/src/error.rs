use crate::token::{Kind, Span};
use thiserror::Error;

pub type Result<T, E = Error> = std::result::Result<T, E>;

#[derive(Debug, Error)]
pub enum Error {
    #[error("resolve error")]
    ResolveError(#[from] ResolveError),
    /// Source parse error.
    #[error("parse error")]
    ParseError(#[from] ParseError),
}

/// Error raised when resolving a value.
#[derive(Debug, Clone, Copy, Error)]
pub enum ResolveError {
    /// Encountered a bad string escape sequence.
    #[error("bad string escape sequence character `{c}`")]
    BadStringEscapeSequence {
        /// Span of the illegal escape sequence.
        span: Span,
        /// The illegal character.
        c: char,
    },
    /// Tried to resolve an illegal number literal.
    #[error("illegal number literal")]
    IllegalNumberLiteral {
        /// Span of the illegal number literal.
        span: Span,
    },
}

#[derive(Debug, Clone, Copy, Error)]
pub enum ParseError {
    #[error("unexpected end-of-file")]
    UnexpectedEof {
        /// Span that caused the error.
        span: Span,
    },
    #[error("unexpected character `{c}`")]
    UnexpectedChar { 
        span: Span,
        c: char
     },
    #[error("expected expression but got `{actual}`")]
    ExpectedExprError {
        /// Span that caused the error.
        span: Span,
        /// The kind of the actual token we saw.
        actual: Kind,
    },
    /// Expected a number, but got something else.
    #[error("expected number but got `{actual}`")]
    ExpectedNumberError {
        /// Span that caused the error.
        span: Span,
        /// The kind of the actual token we saw.
        actual: Kind,
    },
    #[error("expected an operator but got `{actual}`")]
    ExpectedOperator {
        /// The location of the unexpected operator.
        span: Span,
        /// The actual token that was encountered instead of an operator.
        actual: Kind,
    },
    #[error("expected end-of-file")]
    ExpectedEof {
        /// Span that caused the error.
        span: Span,
        /// Kind of the token encountered instead of end-of-file.
        actual: Kind,
    },
    #[error("token mismatch, expected `{expected}` but was `{actual}`")]
    TokenMismatch {
        /// Span that caused the error.
        span: Span,
        /// The kind of the expected token we saw.
        expected: Kind,
        /// The kind of the actual token we saw.
        actual: Kind,
    },
}
