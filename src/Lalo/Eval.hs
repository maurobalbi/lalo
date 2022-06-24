{-# LANGUAGE OverloadedStrings  #-}

module Lalo.Eval (
  runEval,
) where

import Lalo.Syntax

import Control.Monad.Except
import qualified Data.Map as Map
import Error.Diagnose
import Lalo.Parser.Span (Loc(..))
import Data.Text

data Value
  = VInt Integer
  | VBool Bool
  | VClosure Text Expr (Scope)

instance Show Value where
  show (VInt x) = show x
  show (VBool x) = show x
  show VClosure{} = "<<closure>>"

type Eval t = Except Text t

type Scope = Map.Map Text Value

eval :: Scope -> Expr -> Eval Value
eval env expr = case expr of
  Lit (LInt x) -> return $ VInt (fromIntegral $ unloc x)
  Lit (LBool x) -> return $ VBool x
  If p t f -> do
    pred <- eval env p
    case pred of
      VBool True -> eval env t
      VBool False -> eval env f
      _ -> error "Woop"
  Var (Loc _ x) -> return $ env Map.! x
  Lam (Loc _ x) body -> return (VClosure x body env)
  App a b -> do
    x <- eval env a
    y <- eval env b
    apply x y
  Op op a b -> do
    x <- eval env a
    y <- eval env b
    binop op x y

binop :: Binop -> Value -> Value -> Eval Value
binop Add (VInt a) (VInt b) = return $ VInt (a+b)
binop Sub (VInt a) (VInt b) = return $ VInt (a-b)
binop Mul (VInt a) (VInt b) = return $ VInt (a*b)
binop Eql (VInt a) (VInt b) = return $ VBool (a==b)
binop _ _ _ = throwError "Weird"

extend :: Scope -> Text -> Value -> Scope
extend env v t = Map.insert v t env

apply :: Value -> Value -> Eval Value
apply (VClosure v t0 e) t1 = eval (extend e v t1) t0
apply _ _  = throwError "Could not apply"

emptyScope :: Scope
emptyScope = Map.empty

runEval :: Expr -> Either Text Value
runEval x = runExcept (eval emptyScope x)