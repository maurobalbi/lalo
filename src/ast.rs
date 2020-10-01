use crate::error::{ParseError, Result};
use crate::parser::Parser;
use crate::token::{self, Kind, Token};
use crate::traits::Parse;

pub enum Number {
    Float(f64),
    Integer(i64),
}

/// A number literal.
#[derive(Debug)]
pub struct NumberLiteral {
    /// The kind of the number literal.
    number: token::NumberLiteral,
    /// The token corresponding to the literal.
    token: Token,
}

impl Parse for NumberLiteral {
    fn parse(parser: &mut Parser<'_>) -> Result<Self, ParseError> {
        let token = parser.token_next()?;

        Ok(match token.kind {
            Kind::NumberLiteral { number } => NumberLiteral { number, token },
            _ => return Err(ParseError::ExpectedNumberError { 
                actual: token.kind,
                span: token.span
             }),
        })
    }
}

#[derive(Debug, Clone, Copy)]
pub enum BinOp {
    /// Addition.
    Add {
        /// Token associated with operator.
        token: Token,
    },
    /// Subtraction.
    Sub {
        /// Token associated with operator.
        token: Token,
    },
    /// Division.
    Div {
        /// Token associated with operator.
        token: Token,
    },
    /// Multiplication.
    Mul {
        /// Token associated with operator.
        token: Token,
    },
}

impl BinOp {
    fn precedence(self) -> usize {
        match self {
            Self::Add { .. } | Self::Sub { .. } => 1,
            Self::Div { .. } | Self::Mul { .. } => 10,
        }
    }

    fn from_token(token: Token) -> Option<BinOp> {
        Some(match token.kind {
            Kind::Plus => Self::Add { token },
            Kind::Minus => Self::Sub { token },
            Kind::Slash => Self::Div { token },
            Kind::Star => Self::Mul { token },
            _ => return None,
        })
    }
}

impl Parse for BinOp {
    fn parse(parser: &mut Parser) -> Result<Self, ParseError> {
        let token = parser.token_next()?;

        Ok(match Self::from_token(token) {
            Some(bin_op) => bin_op,
            None => return Err(ParseError::ExpectedOperator { 
                actual: token.kind,
                span: token.span
             }),
        })
    }
}

#[derive(Debug)]
pub struct ExprBinary {
    pub lhs: Box<Expr>,
    pub op: BinOp,
    pub rhs: Box<Expr>,
}

#[derive(Debug)]
pub enum Expr {
    ExprBinary(ExprBinary),
    NumberLiteral(NumberLiteral),
}

impl Expr {
    fn parse_default(parser: &mut Parser<'_>) -> Result<Self, ParseError> {
        Self::parse_primary(parser)
    }

    fn parse_primary(
        parser: &mut Parser<'_>,

    ) -> Result<Self, ParseError> {
        let token = parser.token_peek_eof()?;

        Ok(match token.kind {
            Kind::NumberLiteral { .. } => Self::NumberLiteral(parser.parse()?),
            _ => {
                return Err(ParseError::ExpectedExprError {
                    actual: token.kind,
                    span: token.span
                })
            }
        })
    }

    fn parse_expr_binary(
        parser: &mut Parser<'_>,
        mut lhs: Self,
        min_precedence: usize,
    ) -> Result<Self, ParseError> {
        let mut lookahead = parser.token_peek()?.and_then(BinOp::from_token);

        loop {
            let op = match lookahead {
                Some(op) if op.precedence() >= min_precedence => op,
                _ => break,
            };

            parser.token_next()?;
            let mut rhs = Self::parse_default(parser)?;

            lookahead = parser.token_peek()?.and_then(BinOp::from_token);

            loop {
                let lh = match lookahead {
                    Some(lh) if lh.precedence() > op.precedence() => lh,
                    _ => break,
                };

                rhs = Self::parse_expr_binary(parser, rhs, lh.precedence())?;
                lookahead = parser.token_peek()?.and_then(BinOp::from_token);
            }

            lhs = Expr::ExprBinary(ExprBinary {
                lhs: Box::new(lhs),
                op,
                rhs: Box::new(rhs),
            });
        }

        Ok(lhs)
    }
}

/// Parsing a block expression.
///
/// # Examples
///
/// ```rust
/// use lalo-rust::{parse_all, ast};
///
/// # fn main() {
/// parse_all::<ast::Expr>("42").unwrap();
/// parse_all::<ast::Expr>("1 + 2 / 3 - 4 * 1").unwrap();
/// # }
/// ```
impl Parse for Expr {
    fn parse(parser: &mut Parser<'_>) -> Result<Self, ParseError> {
        let lhs = Self::parse_default(parser)?;
        Self::parse_expr_binary(parser, lhs, 0)
    }
}
