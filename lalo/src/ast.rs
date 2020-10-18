use crate::error::{ParseError, Result};
use crate::parser::Parser;
use crate::token::{self, Kind, Token, Span};
use crate::traits::{Parse, Peek};

pub enum Number {
    Float(f64),
    Integer(i64),
}

/// A number literal.
#[derive(Debug, Clone)]
pub struct NumberLiteral {
    /// The kind of the number literal.
    number: token::NumberLiteral,
    /// The token corresponding to the literal.
    token: Token,
}

impl NumberLiteral {
    /// Access the span of the expression.
    pub fn span(&self) -> Span {
        self.token.span
    }
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

#[derive(Debug, Clone)]
pub struct Let {
    pub let_: LetToken,
    pub name: Ident,
    pub eq: Eq,
    pub expr: Box<Expr>,
}

impl Let {
    /// Access the span of the expression.
    pub fn span(&self) -> Span {
        self.let_.token.span.join(self.expr.span())
    }
}

impl Parse for Let {
    fn parse(parser: &mut Parser) -> Result<Self, ParseError> {
        Ok(Self {
            let_: parser.parse()?,
            name: parser.parse()?,
            eq: parser.parse()?,
            expr: Box::new(parser.parse()?),
        })
    }
}

#[derive(Debug, Clone)]
pub struct ExprIf {
    pub if_: IfToken,
    pub condition: Ident,
    pub then_: ThenToken,
    pub expr_then: Box<Expr>,
    pub else_: ElseToken,
    pub expr_else: Box<Expr>,
}

impl ExprIf {
    /// Access the span of the expression.
    pub fn span(&self) -> Span {
        self.if_.token.span.join(self.expr_else.span())
    }
}

impl Parse for ExprIf {
    fn parse(parser: &mut Parser) -> Result<Self, ParseError> {
        Ok(Self {
            if_: parser.parse()?,
            condition: parser.parse()?,
            then_: parser.parse()?,
            expr_then: Box::new(parser.parse()?),
            else_: parser.parse()?,
            expr_else: Box::new(parser.parse()?),
        })
    }
}


#[derive(Debug, Clone)]
pub struct Lambda {
    pub backslash_: BackSlashToken,
    pub arg: Ident,
    pub body: Box<Expr>,
}

impl Lambda {
    pub fn span(&self) -> Span {
        self.backslash_.span().join(self.body.span())
    }
}

impl Parse for Lambda {
    fn parse(parser: &mut Parser<'_>) -> Result<Self, ParseError> {
        let backslash_ = parser.parse()?;

        let arg = parser.parse()?;

        parser.parse::<Arrow>()?; 

        let body = Box::new(parser.parse()?);

        Ok(Self { backslash_, arg, body})
    }
}

/// Something parenthesized and comma separated `(<T,>*)`.
#[derive(Debug, Clone)]
pub struct FunctionArgs {
    pub first: Ident,
    /// The parenthesized type.
    pub rest: Vec<Ident>,
}

impl FunctionArgs {
    /// Access the span of expression.
    pub fn span(&self) -> Span {
        match self.rest.last() {
            Some(ident) => self.first.span().join(ident.span()),
            _ => self.first.span()
        }
    }
}

impl Parse for FunctionArgs {
    fn parse(parser: &mut Parser<'_>) -> Result<Self, ParseError> {
        let first = parser.parse()?;

        let mut rest = Vec::new();

        while parser.peek::<Ident>()? {
            rest.push(parser.parse()?);
        }

        Ok(Self { first, rest })
    }
}

#[derive(Debug, Clone)]
pub struct ExprBinary {
    pub lhs: Box<Expr>,
    pub op: BinOp,
    pub rhs: Box<Expr>,
}

impl ExprBinary {
    /// Access the span of the expression.
    pub fn span(&self) -> Span {
        self.lhs.span().join(self.rhs.span())
    }
}

#[derive(Debug, Clone)]
pub enum Expr {
    ExprBinary(ExprBinary),
    Ident(Ident),
    Lambda(Lambda),
    Let(Let),
    ExprIf(ExprIf),
    NumberLiteral(NumberLiteral),
}

impl Expr {
    /// Get the span of the expression.
    pub fn span(&self) -> Span {
        match self {
            Self::Let(expr) => expr.span(),
            Self::ExprIf(expr) => expr.span(),
            Self::Ident(expr) => expr.span(),
            Self::Lambda(expr) => expr.span(),
            Self::NumberLiteral(expr) => expr.span(),
            Self::ExprBinary(expr) => expr.span(),
        }
    }

    fn parse_default(parser: &mut Parser<'_>) -> Result<Self, ParseError> {
        Self::parse_primary(parser)
    }

    fn parse_ident_start(parser: &mut Parser<'_>) -> Result<Self, ParseError> {
        Ok( Self::Ident(parser.parse()?))
    }

    fn parse_primary(
        parser: &mut Parser<'_>,

    ) -> Result<Self, ParseError> {
        let token = parser.token_peek_eof()?;

        Ok(match token.kind {
            Kind::NumberLiteral { .. } => Self::NumberLiteral(parser.parse()?),
            Kind::Let { .. } => Self::Let(parser.parse()?),
            Kind::BackSlash { .. } => Self::Lambda(parser.parse()?),
            Kind::Ident { .. } => Self::parse_ident_start(parser)?,
            Kind::If { .. } => Self::ExprIf(parser.parse()?),
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

macro_rules! decl_tokens {
    ($(($parser:ident, $($kind:tt)*),)*) => {
        $(
            /// Helper parser for a specifik token kind
            #[derive(Debug, Clone, Copy)]
            pub struct $parser {
                /// Associated token.
                pub token: Token,
            }

            impl $parser {
                /// Access the span of the token.
                pub fn span(&self) -> Span {
                    self.token.span
                }
            }

            impl Parse for $parser {
                fn parse(parser: &mut Parser<'_>) -> Result<Self, ParseError> {
                    let token = parser.token_next()?;

                    match token.kind {
                        $($kind)* => Ok(Self {
                            token,
                        }),
                        _ => Err(ParseError::TokenMismatch {
                            expected: $($kind)*,
                            actual: token.kind,
                            span: token.span,
                        }),
                    }
                }
            }

            impl Peek for $parser {
                fn peek(p1: Option<Token>) -> bool {
                    match p1 {
                        Some(p1) => match p1.kind {
                            $($kind)* => true,
                            _ => false,
                        }
                        _ => false,
                    }
                }
            }
        )*
    }
}

decl_tokens! {
    (Arrow, Kind::Arrow),
    (IfToken, Kind::If),
    (ThenToken, Kind::Then),
    (ElseToken, Kind::Else),
    (LetToken, Kind::Let),
    (BackSlashToken, Kind::BackSlash),
    (Ident, Kind::Ident),
    (Eq, Kind::Eq),
}