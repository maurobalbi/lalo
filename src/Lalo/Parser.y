{
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Lalo.Parser (
  parse,
) where

import Lalo.Lexer as L
import Lalo.Parser.Monad as M
import Lalo.Parser.Token
import Lalo.Syntax
import Lalo.Parser.Span

import Debug.Trace

import Data.ByteString as B

import Error.Diagnose

import Control.Monad.Except

}

-- Entry point
%name expr

-- Lexer structure 
%tokentype { Token }

-- Parser monad
%monad { Parser }
%error { failure }

-- Token Names
%token
    let   { TokenLet $$}
    if    { TokenIf $$}
    then  { TokenThen $$}
    else  { TokenElse $$}
    true  { TokenTrue $$}
    false { TokenFalse $$}
    in    { TokenIn $$}
    NUM   { TokenNum $$ }
    VAR   { TokenSym $$ }
    '\\'  { TokenLambda $$}
    '->'  { TokenArrow $$}
    '='   { TokenEq $$}
    '+'   { TokenAdd $$}
    '-'   { TokenSub $$}
    '*'   { TokenMul $$}
    '('   { TokenLParen $$}
    ')'   { TokenRParen $$}

-- Operators
%left '+' '-'
%left '*'
%%

Expr : let VAR '=' Expr in Expr    { App (Lam $2 $6) $4 }
     | '\\' VAR '->' Expr          { Lam $2 $4 }
     | if Expr then Expr else Expr { If $2 $4 $6 }
     | Form                        { $1 }

Form : Form '+' Form               { Op Add $1 $3 }
     | Form '-' Form               { Op Sub $1 $3 }
     | Form '*' Form               { Op Mul $1 $3 }
     | Fact                        { $1 }

Fact : Fact Atom                   { App $1 $2 }
     | Atom                        { $1 }

Atom : '(' Expr ')'                { $2 }
     | NUM                         { Lit (LInt $1) }
     | VAR                         { Var $1 }
     | true                        { Lit (LBool True) }
     | false                       { Lit (LBool False) }

{
failure :: [Token] -> M.Parser a
failure [] = do
  sp <- M.location
  M.parseError M.EmptyTokenStream
failure (tok:_) = do
  sp <- M.location
  M.parseError $ M.UnexpectedToken (Loc sp tok)
 
lex :: B.ByteString -> Either ParseError [Token]
lex bs = M.runParser bs L.lexer

parse :: B.ByteString -> Either ParseError Expr
parse bs = M.runParser bs $ do
  toks <- L.lexer
  expr toks
}