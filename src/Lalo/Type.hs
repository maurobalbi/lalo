module Lalo.Type where

import Data.Text qualified as T

data Literal
  = Bool
  | Int
  | Unit
  deriving (Eq, Show)

data Type
  = Literal Literal
  | Function Type Type
  | Variable T.Text
  deriving (Eq, Show)

data Scheme = Scheme [T.Text] Type