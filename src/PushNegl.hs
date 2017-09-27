module PushNegl (ti1_norm_view, ti1_norm_eval) where

import           Lib (Exp (Add, Lit, Neg), ti1, view, eval)

push_neg ::Exp -> Exp
push_neg e@Lit{}           = e
push_neg e@(Neg (Lit _))   = e
push_neg (Neg (Neg e))     = push_neg e
push_neg (Neg (Add e1 e2)) = Add (push_neg (Neg e1)) (push_neg (Neg e2))
push_neg (Add e1 e2) = Add (push_neg e1) (push_neg e2)

ti1_norm :: Exp
ti1_norm = push_neg ti1

ti1_norm_view :: String
ti1_norm_view = view ti1_norm

ti1_norm_eval :: Int
ti1_norm_eval = eval ti1_norm
