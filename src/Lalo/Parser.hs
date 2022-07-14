{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE GADTs #-}
module Lalo.Parser where

import Text.Megaparsec hiding (token)
import qualified Text.Megaparsec.Char.Lexer as L
import qualified Data.Text as T
import Data.Void
import Data.Foldable
import Text.Megaparsec.Char (space1, alphaNumChar, digitChar, string, letterChar)
import Data.Text.Internal.Encoding.Fusion (restreamUtf16BE)
import Data.Char (digitToInt)
import Data.Bifoldable (Bifoldable)
import Lalo.Syntax ( Binop(..), Lit(..), Expr(..) )

type Parser = Parsec Void T.Text

reservedWords :: [T.Text]
reservedWords = 
  [ "if"
  , "then"
  , "else"
  , "True"
  , "False"
  ]

-- High level combinators
(<**>) = flip (<*>)

infixl1 :: (a -> b) -> Parser a -> Parser (b -> a -> b) ->  Parser b
infixl1 wrap p op = rest <*> (wrap <$> p)
  where rest = flip (.) <$> (flip <$> op <*> p) <*> rest <|> pure id

infixr1 :: (a -> b) -> Parser a -> Parser (a -> b -> b) ->  Parser b
infixr1 wrap p op = (flip <$> op <*> infixr1 wrap p op <|> pure wrap) <*> p

postfix :: (a -> b) -> Parser a -> Parser (b -> b) -> Parser b
postfix wrap p op = (wrap <$> p) <**> rest
  where rest = flip (.) <$> op <*> rest <|> pure id

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
keyword keyword = lexeme (string keyword <* notFollowedBy alphaNumChar)

identifier :: Parser T.Text
identifier = token $ (p >>= notReservedWord)
  where
    p= T.pack <$>  some letterChar
    notReservedWord w = if w `elem` reservedWords
                        then fail $ "Keyword " ++ show w ++ " is not a valid identifier"
                        else pure w

----- 
literal :: Parser Lit
literal = LInt <$> integer

aexpr = parens expr
  <|> lambda
  <|> lit
  <|> binop
  <|> ifexpr
  <|> variable
  <|> boolean

boolean = Lit <$> LBool <$> (pure True <* keyword "True" <|> pure False <* keyword "False")

expr :: Parser Expr
expr = do
  es <- some aexpr
  pure $ foldl1 App es

lambda:: Parser Expr
lambda = Lam <$> (symbol "\\" *> identifier) <*> expr
 
variable :: Parser Expr
variable = Var <$> identifier

binop :: Parser Expr
binop = do
  keyword "+"
  r <- expr
  pure $ Op Add r r

ifexpr :: Parser Expr
ifexpr = do
  keyword "if"
  p <- expr
  keyword "then"
  t <- expr
  keyword "else"
  f <- expr
  pure $ If p t f

lit :: Parser Expr
lit = Lit <$> literal

parseExpr input = runParser expr "<interactive>" input


