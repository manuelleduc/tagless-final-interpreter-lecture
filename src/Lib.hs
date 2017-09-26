module Lib where

data Exp = Lit Int
         | Neg Exp
         | Add Exp Exp

ti1 :: Exp
ti1 = Add (Lit 8) (Neg (Add (Lit 1) (Lit 2)))

eval :: Exp -> Int
eval (Lit n)     = n
eval (Neg e)     = - eval e
eval (Add e1 e2) = eval e1 + eval e2

type Repr = Int

view :: Exp -> String
view (Lit n) = show n
view (Neg e) = "(" ++ "-" ++ view e ++ ")"
view (Add e1 e2) = "(" ++ view e1 ++ " + " ++ view e2 ++ ")"



class ExpSYM repr where
  lit :: Int -> repr
  neg :: repr -> repr
  add :: repr -> repr -> repr
