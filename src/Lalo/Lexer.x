{
module Lalo.Lexer (
  Token(..),
  scanToken,
  lexer
) where

import qualified Data.Text as T
import Lalo.Parser.Monad
import Lalo.Parser.Span
import Lalo.Parser.Token

import Control.Monad.Except

}

$digit = 0-9
$alpha = [a-zA-Z]
$eol   = [\n]

tokens :-

  -- Whitespace insensitive
  $eol                          ;
  $white+                       ;

  -- Comments
  "#".*                         ;

  -- Syntax
  let                           { token TokenLet }
  if                            { token TokenIf }
  then                          { token TokenThen }
  else                          { token TokenElse }
  True                          { token TokenTrue }
  False                         { token TokenFalse }
  in                            { token TokenIn }
  $digit+                       { token (\loc -> TokenNum (read. T.unpack <$> loc)) }
  "->"                          { token TokenArrow }
  \=                            { token TokenEq }
  \\                            { token TokenLambda }
  [\+]                          { token TokenAdd }
  [\-]                          { token TokenSub }
  [\*]                          { token TokenMul }
  \(                            { token TokenLParen }
  \)                            { token TokenRParen }
  $alpha [$alpha $digit \_ \']* { token TokenSym  }

{


scanToken :: Parser Token
scanToken = do
  input <- getInput
  case alexScan input 0 of
    AlexEOF -> pure TokenEOF
    AlexError (AlexInput pos _ _ _) ->
      parseError $ InvalidLexeme pos
    AlexSkip rest len -> do
      advance rest
      scanToken
    AlexToken rest nbytes action -> do
      advance rest
      action (slice nbytes input)


lexer :: Parser [Token]
lexer = do
  token <- scanToken
  case token of
    TokenEOF -> pure []
    x -> (x :) <$> lexer

}