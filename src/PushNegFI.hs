module PushNegFI () where

import           Exp.Sum  (ExpSYM (..))
import           Lib      (Exp (..))
import qualified PushNegl as I (push_neg)

instance ExpSYM Exp where
  lit = Lit
  neg = Neg
  add = Add

initialize :: Exp -> Exp
initialize = id

finalize :: ExpSYM repr => Exp -> repr
finalize (Lit n)     = lit n
finalize (Neg e)     = neg (finalize e)
finalize (Add e1 e2) = add (finalize e1) (finalize e2)

push_neg :: ExpSYM repr => Exp -> repr
push_neg = finalize . I.push_neg . initialize
