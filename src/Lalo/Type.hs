module Lalo.Type where

import Data.Text qualified as T

data Literal
  = TBool
  | TInt
  | TUnit
  deriving (Eq, Show)

newtype TVar = MkTVar {unTVar :: T.Text} deriving (Eq, Ord, Show)

data Type
  = TLiteral Literal
  | TFunction Type Type
  | TVariable TVar
  | TExists TVar
  | TForall TVar Type
  deriving (Eq, Show)

data Scheme = Scheme [T.Text] Type