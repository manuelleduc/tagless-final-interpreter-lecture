{-# LANGUAGE GADTs #-}

module Exp2 () where

data Exp env t where
  B :: Bool -> Exp env Bool
  V :: Var env t -> Exp env t
  L :: Exp (a, env) b -> Exp env (a -> b)
  A :: Exp env (a -> b) -> Exp env a -> Exp env b

data Var env t where
  VZ :: Var (t, env) t
  VS :: Var env t -> Var (a, env) t


ti1 :: Exp env Bool
ti1 = A (L (V VZ)) (B True)

eval :: env -> Exp env t -> t
eval env (V v) = lookp v env
eval env (B b) = b
eval env (L e) = \x -> eval (x, env) e
eval env (A e1 e2) = (eval env e1) (eval env e2)

lookp :: Var env t -> env -> t
lookp VZ (x, _) = x
lookp (VS v) (_, env) = lookp v env
