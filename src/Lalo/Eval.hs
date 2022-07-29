{-# LANGUAGE OverloadedStrings #-}

module Lalo.Eval

where

-- import Control.Monad.Except
-- import Data.Map qualified as Map
-- import Data.Text qualified as T
-- import Lalo.Syntax

-- data Value
--   = VInt Integer
--   | VBool Bool
--   | VClosure T.Text Expr (Scope)

-- instance Show Value where
--   show (VInt x) = show x
--   show (VBool x) = show x
--   show VClosure {} = "<<closure>>"

-- type Eval t = Except T.Text t

-- type Scope = Map.Map T.Text Value

-- eval :: Scope -> Expr -> Eval Value
-- eval env expr = case expr of
--   Lit (LInt x) -> return $ VInt (fromIntegral x)
--   Lit (LBool x) -> return $ VBool x
--   If p t f -> do
--     pred <- eval env p
--     case pred of
--       VBool True -> eval env t
--       VBool False -> eval env f
--       _ -> error "Woop"
--   Var x -> return $ env Map.! x
--   Lam x body -> return (VClosure x body env)
--   App a b -> do
--     x <- eval env a
--     y <- eval env b
--     apply x y
--   Op op a b -> do
--     x <- eval env a
--     y <- eval env b
--     binop op x y

-- binop :: Binop -> Value -> Value -> Eval Value
-- binop Add (VInt a) (VInt b) = return $ VInt (a + b)
-- binop Sub (VInt a) (VInt b) = return $ VInt (a - b)
-- binop Mul (VInt a) (VInt b) = return $ VInt (a * b)
-- binop Eql (VInt a) (VInt b) = return $ VBool (a == b)
-- binop _ _ _ = throwError "Tried to do arithmetic operation over non-number"

-- extend :: Scope -> T.Text -> Value -> Scope
-- extend env v t = Map.insert v t env

-- apply :: Value -> Value -> Eval Value
-- apply (VClosure v t0 e) t1 = eval (extend e v t1) t0
-- apply _ _ = throwError "Tried to apply closure"

-- emptyScope :: Scope
-- emptyScope = Map.empty

-- runEval :: Expr -> Either T.Text Value
-- runEval x = runExcept (eval emptyScope x)
