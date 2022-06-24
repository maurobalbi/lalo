module Lalo.Syntax where

import Data.Text as T
import Lalo.Parser.Span

type Name = Loc T.Text

data Expr
  = Lam Name Expr
  | App Expr Expr
  | Var Name
  | Lit Lit
  | Op Binop Expr Expr
  | If Expr Expr Expr
  deriving (Show)

data Lit
  = LInt (Loc Int)
  | LBool Bool
  deriving (Show)

data Binop = Add | Sub | Mul | Eql
  deriving (Eq, Ord, Show)