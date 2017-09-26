module Exp.Sum where

class ExpSYM repr where
  lit :: Int -> repr
  neg :: repr -> repr
  add :: repr -> repr -> repr

tf1 :: (ExpSYM a) => a
tf1 = add (lit 8) (neg (add (lit 1) (lit 2)))
