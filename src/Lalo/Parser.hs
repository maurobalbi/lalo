{-# LANGUAGE GADTs             #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

module Lalo.Parser where

import           Data.Bifoldable                    (Bifoldable)
import           Data.Char                          (digitToInt)
import           Data.Foldable
import qualified Data.Text                          as T
import           Data.Text.Internal.Encoding.Fusion (restreamUtf16BE)
import           Data.Void

import           Lalo.Input                         (Input)
import           Lalo.Location                      (Offset)
import           Lalo.Syntax                        (Expr, Literal)
import qualified Lalo.Syntax                        as Syntax

import           Data.String                        (IsString)
import           GHC.Exts                           (IsString (fromString))
import           Text.Megaparsec                    hiding (token)
import           Text.Megaparsec.Char               (alphaNumChar, digitChar,
                                                     letterChar, space1, string)
import qualified Text.Megaparsec.Char.Lexer         as L
import Control.Monad.Combinators.Expr

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
keyword keyword = token (string keyword <* notFollowedBy alphaNumChar)

identifier :: Parser T.Text
identifier = token $ (p >>= notReservedWord)
  where
    p= T.pack <$>  some letterChar
    notReservedWord w = if w `elem` reservedWords
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
locatedBool = (,) <$> getOffset <*> (pure (Syntax.LBool True) <* locatedKeyword "True" <|> pure (Syntax.LBool False) <* locatedKeyword "False")
-----

aexpr = parens expr
  <|> lambda
  <|> literal
  <|> ifexpr
  <|> variable

expr :: Parser (Expr Offset Input)
expr = do
  es <- some aexpr
  pure $ foldl1 application es
    where
      application function argument = Syntax.Application{location = Syntax.location function, ..}

lambda:: Parser (Expr Offset Input)
lambda = do
  location <- locatedSymbol "\\"
  (nameLocation, name) <- locatedIdentifier
  symbol "->"
  body <- expr
  pure Syntax.Lambda{..}

variable :: Parser (Expr Offset Input)
variable = do
  (location, name) <- locatedIdentifier
  pure Syntax.Variable{..}

ifexpr :: Parser (Expr Offset Input)
ifexpr = do
  location <- locatedKeyword "if"
  predicate <- expr
  keyword "then"
  ifTrue <- expr
  keyword "else"
  ifFalse <- expr
  pure Syntax.If{..}

literal :: Parser (Expr Offset Input)
literal = do 
  (location, literal) <- locatedBool <|> locatedInt
  pure Syntax.Literal{..}

binaryExpr = makeExprParser expr operatorTable

binary :: T.Text -> (Expr a s -> Expr a s -> Expr a s) -> Operator Parser (Expr a s)
binary name f = InfixL  (f <$ symbol name)

operatorTable :: [[Operator Parser (Expr Offset Input)]]
operatorTable = [[
    binary "+" (\x y -> Syntax.Operator{location=0, operator=Syntax.Plus, operatorLocation=0, left=x, right=y})
  ]]

-- >>> runParser (fully expr) "<interactive>" "\\x->x"
-- Left (ParseErrorBundle {bundleErrors = TrivialError 1 (Just (Tokens ('x' :| ""))) (fromList []) :| [], bundlePosState = PosState {pstateInput = "\\x->x", pstateOffset = 0, pstateSourcePos = SourcePos {sourceName = "<interactive>", sourceLine = Pos 1, sourceColumn = Pos 1}, pstateTabWidth = Pos 8, pstateLinePrefix = ""}})
parseExpr input = runParser expr "<interactive>" input


