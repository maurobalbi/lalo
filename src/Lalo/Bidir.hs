{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Lalo.Bidir where

import Control.Monad.Except (ExceptT, MonadError (throwError))
import Control.Monad.Identity
import Data.List.NonEmpty qualified as Data
import Data.Text
import Data.Void
import Lalo.Syntax qualified as Syntax
import Lalo.Type qualified as Type

type Context = [(Text, Type.Type)]

type Result a = ExceptT Text Identity a

infer :: Context -> Syntax.Expr -> Result Type.Type
infer context (Syntax.Annotation {..}) =
  do
    check context annotated annotation
    pure annotation
infer context (Syntax.Variable {..}) = case lookup name context of
  Just a -> pure a
  Nothing -> throwError $ "Variable not found"
infer context (Syntax.Application {..}) =
  do
    sigma <- infer context function
    case sigma of
      (Type.TFunction t1 t2) ->
        do
          check context argument t2
          pure t1
infer _ (Syntax.Literal (Syntax.Int 1)) = pure $ Type.TLiteral Type.TInt
infer _ t = throwError $ pack $ "Unhandled case " ++ show t

check :: Context -> Syntax.Expr -> Type.Type -> Result ()
check context Syntax.Lambda {..} (Type.TFunction t1 t2) =
  do
    check ((name, t1) : context) body t2
check context expr ty =
  do
    t' <- infer context expr
    unless (ty == t') (throwError "Type mismatch")

-- >>> import Lalo.Syntax
-- >>> import Lalo.Type
-- >>> import Data.List.NonEmpty
-- >>> infer [] (Application {function = Annotation {annotated = Lambda {name = "x", body = Variable {name = "x"}}, annotation = TFunction (TLiteral TInt) (TLiteral TInt)}, argument = Literal {literal = Int 1}})
-- ExceptT (Identity (Right (TLiteral TInt)))