{-# LANGUAGE DerivingStrategies    #-}
{-# LANGUAGE DuplicateRecordFields #-}

module Lalo.Syntax where

import Data.List.NonEmpty         (NonEmpty (..))
import Data.Text                  as T
import GHC.Generics               (Generic)
import Language.Haskell.TH.Syntax (Lift)

data Expr s a
  = Variable
      { location :: s
      , name     :: Text
      }
  | Lambda
      { location     :: s
      , nameLocation :: s
      , name         :: Text
      , body         :: Expr s a
      }
  | Application
      { location :: s
      , function :: Expr s a
      , argument :: Expr s a
      }
  | Let
      { location :: s
      , bindings :: NonEmpty (Binding s a)
      , body     :: Expr s a
      }
  | If
      { location  :: s
      , predicate :: Expr s a
      , ifTrue    :: Expr s a
      , ifFalse   :: Expr s a
      }
  | Literal
      { location :: s
      , literal  :: Literal
      }
  | Operator
      { location         :: s
      , left             :: Expr s a
      , operatorLocation :: s
      , operator         :: Operator
      , right            :: Expr s a
      }
  deriving stock (Eq, Foldable, Functor, Generic, Lift, Show, Traversable)

data Literal
  = LBool Bool
  | LInt Int
  | LUnit
  deriving (Eq, Generic, Lift, Show)

data Operator
  = And
  | Or
  | Plus
  | Times
  deriving (Eq, Generic, Lift, Show)

data Binding s a
  = Binding
      { nameLocation :: s
      , name         :: Text
      , assignment   :: Expr s a
      }
  deriving stock (Eq, Foldable, Functor, Generic, Lift, Show, Traversable)
