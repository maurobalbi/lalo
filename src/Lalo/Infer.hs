{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Lalo.Infer where

import Control.Arrow (ArrowChoice (right))
import Control.Monad.Except (ExceptT, MonadError (catchError, throwError), runExceptT)
import Control.Monad.Identity (Identity (runIdentity))
import Control.Monad.Reader
import Control.Monad.State
import Data.List.NonEmpty (NonEmpty ((:|)))
import Data.Map qualified as M
import Data.Set qualified as S
import Data.Text qualified as T
import Lalo.Syntax qualified as Syntax
import Lalo.Type qualified as Type

type Context = M.Map T.Text Type.Type

data TypeInferenceError = UnifyError Type.Type Type.Type
  deriving (Show)

class Types a where
  ftv :: a -> S.Set T.Text
  apply :: Subst -> a -> a

instance Types Type.Type where
  ftv (Type.TVariable n) = S.singleton n
  ftv (Type.TLiteral _) = S.empty
  ftv (Type.TFunction t1 t2) = ftv t1 `S.union` ftv t2

  apply s (Type.TVariable n) = case M.lookup n s of
    Nothing -> Type.TVariable n
    Just t -> t
  apply s (Type.TFunction t1 t2) = Type.TFunction (apply s t1) (apply s t2)
  apply s t = t

instance Types Type.Scheme where
  ftv (Type.Scheme vars t) = (ftv t) `S.difference` (S.fromList vars)
  apply s (Type.Scheme vars t) = Type.Scheme vars (apply (foldr M.delete s vars) t)

instance Types a => Types [a] where
  ftv l = foldr S.union S.empty (map ftv l)
  apply s = map (apply s)

type Subst = M.Map T.Text Type.Type

nullSubst :: Subst
nullSubst = M.empty

composeSubst :: Subst -> Subst -> Subst
composeSubst s1 s2 = (M.map (apply s1) s2) `M.union` s1

newtype TypeEnv = TypeEnv (M.Map T.Text Type.Scheme)

remove :: TypeEnv -> T.Text -> TypeEnv
remove (TypeEnv env) var = TypeEnv (M.delete var env)

instance Types TypeEnv where
  ftv (TypeEnv env) = ftv (M.elems env)
  apply s (TypeEnv env) = TypeEnv (M.map (apply s) env)

generalize :: TypeEnv -> Type.Type -> Type.Scheme
generalize env t = Type.Scheme vars t
  where
    vars = S.toList ((ftv t) `S.difference` (ftv env))

data TIEnv = TIEnv {}

data TIState = TIState {tiSupply :: Int}
  deriving (Show)

type TI a = ExceptT T.Text (ReaderT TIEnv (StateT TIState Identity)) a

runTI :: TI a -> (Either T.Text a, TIState)
runTI t =
  runIdentity $ do
    (res, st) <-
      runStateT
        (runReaderT (runExceptT t) initTIEnv)
        initTIState
    pure (res, st)
  where
    initTIEnv = TIEnv
    initTIState = TIState {tiSupply = 0}

newTyVar :: T.Text -> TI Type.Type
newTyVar prefix =
  do
    s <- get
    put s {tiSupply = tiSupply s + 1}
    return (Type.TVariable (prefix `T.append` (T.pack $ show (tiSupply s))))

instantiate :: Type.Scheme -> TI Type.Type
instantiate (Type.Scheme vars t) =
  do
    nvars <- mapM (\_ -> newTyVar "a") vars
    let s = M.fromList (zip vars nvars)
    pure $ apply s t

mgu :: Type.Type -> Type.Type -> TI Subst
mgu (Type.TFunction l r) (Type.TFunction l' r') =
  do
    s1 <- mgu l l'
    s2 <- mgu (apply s1 r) (apply s1 r')
    pure (s1 `composeSubst` s2)
mgu (Type.TVariable u) t = varBind u t
mgu t (Type.TVariable u) = varBind u t
mgu (Type.TLiteral _) (Type.TLiteral _) = pure nullSubst
mgu t1 t2 = throwError $ "Types do not unify" `T.append` (T.pack $ show t1) `T.append` "" `T.append` (T.pack $ show t2)

varBind :: T.Text -> Type.Type -> TI Subst
varBind u t
  | t == Type.TVariable u = pure nullSubst
  | u `S.member` ftv t = throwError $ "Occurs check fails: " `T.append` u `T.append` " vs. " `T.append` (T.pack $ show t)
  | otherwise = pure (M.singleton u t)

tiLit :: Syntax.Literal -> TI (Subst, Type.Type)
tiLit (Syntax.Int _) = pure (nullSubst, Type.TLiteral Type.TInt)
tiLit (Syntax.Bool _) = pure (nullSubst, Type.TLiteral Type.TBool)

ti :: TypeEnv -> Syntax.Expr -> TI (Subst, Type.Type)
ti (TypeEnv env) (Syntax.Variable n) =
  case M.lookup n env of
    Nothing -> throwError $ "Unbound variable: " `T.append` n
    Just sigma -> do
      t <- instantiate sigma
      pure (nullSubst, t)
ti _ (Syntax.Literal l) = tiLit l
ti env (Syntax.Lambda n e) =
  do
    tv <- newTyVar "a"
    let TypeEnv env' = remove env n
        env'' = TypeEnv (env' `M.union` (M.singleton n (Type.Scheme [] tv)))
    (s1, t1) <- ti env'' e
    pure (s1, Type.TFunction (apply s1 tv) t1)
ti env exp@(Syntax.Application e1 e2) =
  do
    tv <- newTyVar "a"
    (s1, t1) <- ti env e1
    (s2, t2) <- ti (apply s1 env) e2
    s3 <- mgu (apply s2 t1) (Type.TFunction t2 tv)
    pure (s3 `composeSubst` s2 `composeSubst` s1, apply s3 tv)
    `catchError` \e -> throwError $ e `T.append` "\n in" `T.append` (T.pack $ show exp)
ti env (Syntax.Let ((Syntax.Binding x _ e1) :| _) e2) =
  do
    (s1, t1) <- ti env e1
    let TypeEnv env' = remove env x
        t' = generalize (apply s1 env) t1
        env'' = TypeEnv (M.insert x t' env')
    (s2, t2) <- ti (apply s1 env'') e2
    pure (s1 `composeSubst` s2, t2)


typeInference :: M.Map T.Text Type.Scheme -> Syntax.Expr -> TI Type.Type
typeInference env e =
  do
    (s, t) <- ti (TypeEnv env) e
    pure (apply s t)

-- >>> import Lalo.Syntax
-- >>> let ast = Lambda {name = "x", body = Literal (Int 1)}
-- >>> runTI (typeInference M.empty ast)
-- (Right (Function (Variable "a0") (Literal Int)),TIState {tiSupply = 1})
