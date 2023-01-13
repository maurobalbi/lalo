{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Lalo.Bidir where

import Control.Monad.Except (ExceptT, MonadError (throwError))
import Control.Monad.State (State, MonadState, gets, modify')
import Control.Monad.Identity
import Data.List.NonEmpty qualified as Data
import qualified Data.Text as T
import Data.Void
import Lalo.Syntax qualified as Syntax
import Lalo.Type

data ContextInfo = CExists TVar
  | CMarker TVar
  | CForall TVar

type Context = [ContextInfo]

data CheckState = CheckState {
  checkNextEVar :: Integer
} deriving (Eq, Show)

newtype NameGen a = CheckM (ExceptT T.Text (State CheckState) a) 
  deriving (MonadState CheckState, Functor, Applicative, Monad, MonadError T.Text)

freshTVar = do
  name <- gets checkNextEVar
  modify' (\s -> CheckState {checkNextEVar = checkNextEVar s + 1})
  pure $ MkTVar (T.pack ("a" ++ show name))

-- Γ ⊢ A <: B ⊣ Δ
subtype :: Context -> Type -> Type -> NameGen Context
subtype gamma (TLiteral a) (TLiteral b) | a == b = return gamma 
subtype gamma (TVariable a) (TVariable b) | a == b = return gamma
subtype gamma (TExists a) (TExists b) | a == b = return gamma
subtype gamma (TFunction a1 a2) (TFunction b1 b2) = 
  do
    theta <- subtype gamma b1 a1
    subtype 
      theta 
      (apply theta a2) 
      (apply theta a2)
subtype gamma (TForall alpha a) b = 
  do
    ā <- freshTVar
    theta <- 
      subtype 
        (CMarker â : CExists â : gamma) 
        (typeSubst (TExists â) alpha a) 
        b
    pure $ dropMarker (CExists â) theta
subtype gamma a (TForall alpha b) =
  do
    theta <- 
      subtype 
        (CForall alpha : gamma)
        a
        b
    pure $ dropMarker (CForall alpha) theta
subtype gamma (TExists â) a | â `notElem` freeVars gamma = instanstiateL gamma â a
subtype gamma a (TExists â) | â `notElem` freeVars gamma = instanstiateR gamma a â

-- Γ ⊢ ā :≦ A ⊣ Δ
instanstiateL :: Context -> TVar -> Type -> NameGen Context
instanstiateL gamma (TExists â) t = do
  
  (gammaR, gammaL) <- ctxHole gamma (CExists â)

  

dropMarker :: ContextInfo -> Context -> Context
dropMarker marker = tail . dropWhile (/= marker)


-- solve :: Context -> TVar -> Monotype -> Maybe Context
-- solve gamma alpha tau | typewf gammaL tau = Just gamma'
--                              | otherwise         = Nothing
--   where (gammaL, gammaR) = breakMarker (CExists alpha) gamma
--         gamma' = gammaL >++ [CExistsSolved alpha tau] <> gammaR

infer :: Context -> Syntax.Expr -> NameGen Type
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
      (TFunction t1 t2) ->
        do
          check context argument t2
          pure t1
infer _ (Syntax.Literal (Syntax.Int 1)) = pure $ TLiteral TInt
infer _ t = throwError $ pack $ "Unhandled case " ++ show t

check :: Context -> Syntax.Expr -> Type -> NameGen ()
check context Syntax.Lambda {..} (TFunction t1 t2) =
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