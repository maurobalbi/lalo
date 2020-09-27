use std::fmt;

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum NumberLiteral {
    Decimal,
}

impl fmt::Display for NumberLiteral {
    fn fmt(&self, fmt: &mut fmt::Formatter<'_>) -> fmt::Result {
        match *self {
            Self::Decimal => write!(fmt, "decimal"),
        }
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum Kind {
    NumberLiteral { number: NumberLiteral },
    Plus,
    Minus,
    Slash,
    Star,
}

impl fmt::Display for Kind {
    fn fmt(&self, fmt: &mut fmt::Formatter<'_>) -> fmt::Result {
        match *self {
            Self::NumberLiteral { number } => write!(fmt, "{}", number),
            Self::Plus => write!(fmt, "+"),
            Self::Minus => write!(fmt, "-"),
            Self::Slash => write!(fmt, "/"),
            Self::Star => write!(fmt, "*"),
        }
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct Token {
    pub kind: Kind,
}
