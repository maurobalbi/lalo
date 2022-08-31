{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Lalo.Parser where

import Control.Monad.Combinators.Expr
import Data.Bifoldable (Bifoldable)
import Data.Char (digitToInt)
import Data.Foldable
import Data.List.NonEmpty
import Data.String (IsString)
import Data.Text qualified as T
import Data.Text.Internal.Encoding.Fusion (restreamUtf16BE)
import Data.Void
import GHC.Exts (IsString (fromString))
import Lalo.Input (Input)
import Lalo.Location (Offset)
import Lalo.Syntax (Binding, Expr, Literal)
import Lalo.Syntax qualified as Syntax
import Text.Megaparsec hiding (token)
import Text.Megaparsec.Char
  ( alphaNumChar,
    digitChar,
    letterChar,
    space1,
    string,
  )
import Text.Megaparsec.Char.Lexer qualified as L

type Parser = Parsec Void T.Text

reservedWords :: [T.Text]
reservedWords =
  [ "let",
    "in",
    "if",
    "then",
    "else",
    "True",
    "False"
  ]

-- Lexing
whiteSpace :: Parser ()
whiteSpace = L.space space1 (L.skipLineComment "//") (L.skipBlockComment "/*" "*/")

lexeme :: Parser a -> Parser a
lexeme = L.lexeme whiteSpace

symbol :: T.Text -> Parser T.Text
symbol = L.symbol whiteSpace

token :: Parser a -> Parser a
token = lexeme . try

fully :: Parser a -> Parser a
fully p = whiteSpace *> p <* eof

integer :: Parser Int
integer = lexeme L.decimal

parens :: Parser a -> Parser a
parens = between (symbol "(") (symbol ")")

keyword :: T.Text -> Parser T.Text
keyword keyword = token (string keyword <* notFollowedBy alphaNumChar)

identifier :: Parser T.Text
identifier = token $ (p >>= notReservedWord)
  where
    p = T.pack <$> some letterChar
    notReservedWord w =
      if w `elem` reservedWords
        then fail $ "Keyword " ++ show w ++ " is not a valid identifier"
        else pure w

-- Location
locatedIdentifier :: Parser (Offset, T.Text)
locatedIdentifier = (,) <$> getOffset <*> identifier <?> "identifier"

locatedSymbol :: T.Text -> Parser Offset
locatedSymbol s = getOffset <* (symbol s)

locatedKeyword :: T.Text -> Parser Offset
locatedKeyword text = getOffset <* (keyword text)

integerLiteral :: Parser Literal
integerLiteral = Syntax.Int <$> integer

boolLiteral :: Parser Literal
boolLiteral =
  Syntax.Bool True <$ keyword "True"
    <|> Syntax.Bool False <$ keyword "False"

unitLiteral :: Parser Literal
unitLiteral = Syntax.Unit <$ symbol "()"

-----
expr :: Parser (Expr)
expr = do
  es <- some aexpr
  pure $ foldl1 application es
  where
    application function argument = Syntax.Application {function, ..}

aexpr :: Parser (Expr)
aexpr =
  lambda
    <|> letExpr
    <|> binaryExpr

term :: Parser (Expr)
term =
  literal
    <|> parens expr
    <|> ifExpr
    <|> variable

letExpr :: Parser (Expr)
letExpr = do
  location <- locatedKeyword "let"
  bindings <- binding
  keyword "in"
  body <- expr
  pure Syntax.Let {bindings = bindings :| [], ..}

binding :: Parser (Binding)
binding = do
  (nameLocation, name) <- locatedIdentifier
  symbol "="
  assignment <- expr
  pure Syntax.Binding {..}

lambda :: Parser (Expr)
lambda = do
  location <- locatedSymbol "\\"
  (nameLocation, name) <- locatedIdentifier
  symbol "->"
  body <- expr
  pure Syntax.Lambda {..}

variable :: Parser (Expr)
variable = do
  (location, name) <- locatedIdentifier
  pure Syntax.Variable {..}

ifExpr :: Parser (Expr)
ifExpr = do
  location <- locatedKeyword "if"
  predicate <- expr
  keyword "then"
  ifTrue <- expr
  keyword "else"
  ifFalse <- expr
  pure Syntax.If {..}

literal :: Parser (Expr)
literal = do
  location <- getOffset
  literal <- boolLiteral <|> integerLiteral <|> unitLiteral
  pure Syntax.Literal {..}

binaryExpr :: Parser (Expr)
binaryExpr = makeExprParser term operatorTable

parseOperation :: T.Text -> Syntax.Operator -> Parser (Expr -> Expr -> Expr)
parseOperation _symbol operator = do
  location <- getOffset
  symbol _symbol
  pure $ \expr1 expr2 -> Syntax.Operator {operator = operator, left = expr1, right = expr2}

operatorTable :: [[Operator Parser (Expr)]]
operatorTable =
  [ [InfixL $ parseOperation "&&" Syntax.And],
    [InfixL $ parseOperation "||" Syntax.Or],
    [InfixL $ parseOperation "*" Syntax.Times],
    [ InfixL $ parseOperation "+" Syntax.Plus,
      InfixL $ parseOperation "-" Syntax.Minus
    ],
    [InfixL $ parseOperation "==" Syntax.Eq]
  ]

-- >>> runParser (fully expr) "<interactive>" "\\x-> if x then 1 else 2"
-- Right (Lambda {name = "x", body = If {predicate = Variable {name = "x"}, ifTrue = Literal {literal = Int 1}, ifFalse = Literal {literal = Int 2}}})
parseExpr input = runParser expr "<interactive>" input
