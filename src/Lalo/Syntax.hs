{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DuplicateRecordFields #-}

module Lalo.Syntax where

import Data.List.NonEmpty (NonEmpty (..))
import Data.Text as T
import GHC.Generics (Generic)
import Language.Haskell.TH.Syntax (Lift)
import qualified Lalo.Type as Type

data Expr
    = Variable {
        name :: Text
      }
  | Lambda {
        name :: Text,
        body :: Expr
      }
  | Annotation {
        annotated :: Expr,
        annotation :: Type.Type
    }
  | Application
      { 
        function :: Expr,
        argument :: Expr
      }
  | Let
      { 
        bindings :: NonEmpty (Binding),
        body :: Expr 
      }
  | If
      { 
        predicate :: Expr,
        ifTrue :: Expr,
        ifFalse :: Expr
      }
  | Literal
      { 
        literal :: Literal
      }
  | Operator
      { 
        left :: Expr,
        operator :: Operator,
        right :: Expr 
      }
  deriving stock (Eq,  Show)

data Literal
  = Bool Bool
  | Int Int
  | Unit
  deriving (Eq, Generic, Lift, Show)

data Operator
  = And
  | Or
  | Eq
  | Plus
  | Minus
  | Times
  deriving (Eq, Generic, Lift, Show)

data Binding = Binding
  { 
    name :: Text,
    annotation :: Maybe Type.Type,
    assignment :: Expr
  }
  deriving stock (Eq, Show)
