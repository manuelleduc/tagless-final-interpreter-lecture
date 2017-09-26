module Exp.Mul where

import           Exp.Sum

class MulSYM repr where
  mul :: repr -> repr -> repr

tfm1 :: (MulSYM a, ExpSYM a) => a
tfm1 = add (lit 7) (neg (mul (lit 1) (lit 2)))
