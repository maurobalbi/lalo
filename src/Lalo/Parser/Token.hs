module Lalo.Parser.Token where 

import qualified Data.Text as T

import Lalo.Parser.Span

data Token 
  = TokenLet (Loc T.Text)
  | TokenTrue (Loc T.Text)
  | TokenFalse (Loc T.Text)
  | TokenIn (Loc T.Text)
  | TokenLambda (Loc T.Text)
  | TokenNum (Loc Int)
  | TokenSym (Loc T.Text)
  | TokenArrow (Loc T.Text)
  | TokenEq (Loc T.Text)
  | TokenIf (Loc T.Text)
  | TokenThen (Loc T.Text)
  | TokenElse (Loc T.Text)
  | TokenAdd (Loc T.Text)
  | TokenSub (Loc T.Text)
  | TokenMul (Loc T.Text)
  | TokenLParen (Loc T.Text)
  | TokenRParen (Loc T.Text)
  | TokenEOF
  deriving (Show)