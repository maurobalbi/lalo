use std::fmt;

/// A span corresponding to a range in the source file being parsed.
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash, Default)]
pub struct Span {
    /// The start of the span in bytes.
    pub start: usize,
    /// The end of the span in bytes.
    pub end: usize,
}

impl Span {
    /// Join this span with another span.
    pub fn join(self, other: Self) -> Self {
        Self {
            start: usize::min(self.start, other.start),
            end: usize::min(self.end, other.end),
        }
    }

    /// Get the point span.
    pub fn point(pos: usize) -> Self {
        Self {
            start: pos,
            end: pos,
        }
    }

    /// Narrow the span with the given amount.
    pub fn narrow(self, amount: usize) -> Self {
        Self {
            start: self.start.saturating_add(amount),
            end: self.end.saturating_sub(amount),
        }
    }

    /// Return the zero-based line and column.
    pub fn line_col(self, source: &str) -> (usize, usize) {
        let mut line = 0;
        let mut col = 0;

        let mut it = source.chars();
        let mut count = 0;

        while let Some(c) = it.next() {
            if count >= self.start {
                break;
            }

            count += c.encode_utf8(&mut [0u8; 4]).len();

            if let '\n' | '\r' = c {
                if c == '\n' {
                    line += 1;
                }

                if col > 0 {
                    col = 0;
                }

                continue;
            }

            col += 1;
        }

        (line, col)
    }
}


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
    BackSlash,
    Star,
    Eq,
    If,
    Else,
    Then,
    Let,
    True,
    False,
    Ident,
}

impl fmt::Display for Kind {
    fn fmt(&self, fmt: &mut fmt::Formatter<'_>) -> fmt::Result {
        match *self {
            Self::NumberLiteral { number } => write!(fmt, "{}", number),
            Self::Plus => write!(fmt, "+"),
            Self::Minus => write!(fmt, "-"),
            Self::Slash => write!(fmt, "/"),
            Self::BackSlash => write!(fmt, "\\"),
            Self::Star => write!(fmt, "*"),
            Self::Eq => write!(fmt, "="),
            Self::If => write!(fmt, "if"),
            Self::Else => write!(fmt, "else"),
            Self::Then => write!(fmt, "then"),
            Self::Let => write!(fmt, "let"),
            Self::True => write!(fmt, "true"),
            Self::False => write!(fmt, "false"),
            Self::Ident => write!(fmt, "ident"),
        }
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct Token {
    pub kind: Kind,
    pub span: Span,
}
