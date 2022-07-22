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

-- High level combinators
(<**>) = flip (<*>)

infixl1 :: (a -> b) -> Parser a -> Parser (b -> a -> b) -> Parser b
infixl1 wrap p op = rest <*> (wrap <$> p)
  where
    rest = flip (.) <$> (flip <$> op <*> p) <*> rest <|> pure id

infixr1 :: (a -> b) -> Parser a -> Parser (a -> b -> b) -> Parser b
infixr1 wrap p op = (flip <$> op <*> infixr1 wrap p op <|> pure wrap) <*> p

postfix :: (a -> b) -> Parser a -> Parser (b -> b) -> Parser b
postfix wrap p op = (wrap <$> p) <**> rest
  where
    rest = flip (.) <$> op <*> rest <|> pure id

prefix :: (a -> b) -> Parser (b -> b) -> Parser a -> Parser b
prefix wrap op p = op <*> prefix wrap op p <|> wrap <$> p

chainl1 :: Parser b -> Parser (b -> b -> b) -> Parser b
chainl1 = infixl1 id

chainr1 :: Parser b -> Parser (b -> b -> b) -> Parser b
chainr1 = infixr1 id

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

locatedInt :: Parser (Offset, Literal)
locatedInt = (,) <$> getOffset <*> (Syntax.LInt <$> integer)

locatedBool :: Parser (Offset, Literal)
locatedBool =
  (,) <$> getOffset
    <*> ( pure (Syntax.LBool True) <* locatedKeyword "True"
            <|> pure (Syntax.LBool False) <* locatedKeyword "False"
        )

-----
expr :: Parser (Expr Offset Input)
expr = do
  es <- some aexpr
  pure $ foldl1 application es
  where
    application function argument = Syntax.Application {location = Syntax.location function, ..}

aexpr :: Parser (Expr Offset Input)
aexpr =
  lambda
    <|> letExpr
    <|> binaryExpr

term :: Parser (Expr Offset Input)
term =
  parens expr
    <|> ifExpr
    <|> literal
    <|> variable

letExpr :: Parser (Expr Offset Input)
letExpr = do
  location <- locatedKeyword "let"
  bindings <- binding
  keyword "in"
  body <- expr
  pure Syntax.Let {bindings = bindings :| [], ..}

binding :: Parser (Binding Offset Input)
binding = do
  (nameLocation, name) <- locatedIdentifier
  symbol "="
  assignment <- expr
  pure Syntax.Binding {..}

lambda :: Parser (Expr Offset Input)
lambda = do
  location <- locatedSymbol "\\"
  (nameLocation, name) <- locatedIdentifier
  symbol "->"
  body <- expr
  pure Syntax.Lambda {..}

variable :: Parser (Expr Offset Input)
variable = do
  (location, name) <- locatedIdentifier
  pure Syntax.Variable {..}

ifExpr :: Parser (Expr Offset Input)
ifExpr = do
  location <- locatedKeyword "if"
  predicate <- expr
  keyword "then"
  ifTrue <- expr
  keyword "else"
  ifFalse <- expr
  pure Syntax.If {..}

literal :: Parser (Expr Offset Input)
literal = do
  (location, literal) <- locatedBool <|> locatedInt
  pure Syntax.Literal {..}

binaryExpr :: Parser (Expr Offset Input)
binaryExpr = makeExprParser term operatorTable

parseOperation :: T.Text -> Syntax.Operator -> Parser (Expr Int a -> Expr Int a -> Expr Int a)
parseOperation _symbol operator = do
  location <- getOffset
  symbol _symbol
  pure $ \expr1 expr2 -> Syntax.Operator {location = Syntax.location expr1, operator = operator, operatorLocation = location, left = expr1, right = expr2}

operatorTable :: [[Operator Parser (Expr Offset Input)]]
operatorTable =
  [ [ InfixL $ parseOperation "*" Syntax.Times
    ],
    [ InfixL $ parseOperation "+" Syntax.Plus,
      InfixL $ parseOperation "-" Syntax.Minus
    ]
  ]

-- [ [binary "+" (\x y -> Syntax.Operator {location = 0, operator = Syntax.Plus, operatorLocation = 0, left = x, right = y})],
--   []
-- ]

-- >>> runParser (fully expr) "<interactive>" "\\x->x + 1"
-- Left (ParseErrorBundle {bundleErrors = TrivialError 6 (Just (Tokens ('+' :| ""))) (fromList [Tokens ('(' :| ""),Tokens ('F' :| "alse"),Tokens ('T' :| "rue"),Tokens ('\\' :| ""),Tokens ('i' :| "f"),Label ('i' :| "dentifier"),Label ('i' :| "nteger"),EndOfInput]) :| [], bundlePosState = PosState {pstateInput = "\\x->x + 1", pstateOffset = 0, pstateSourcePos = SourcePos {sourceName = "<interactive>", sourceLine = Pos 1, sourceColumn = Pos 1}, pstateTabWidth = Pos 8, pstateLinePrefix = ""}})
parseExpr input = runParser expr "<interactive>" input
