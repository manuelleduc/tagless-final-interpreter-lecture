module Sem.MulTree (fromTreeExt, tf1'_int3, tfm1'_int3) where

import           Control.Monad (liftM2)
import           Exp.Mul       (MulSYM (..), tfm1)
import           Exp.Sum       (ExpSYM)
import           Sem.Tree      (ErrMsg, Tree (Node))
import qualified Sem.Tree      as S

--fromTreeExt :: (ExpSYM repr, MulSYM repr) => (Tree -> Either ErrMsg repr) -> Tree -> Either ErrMsg repr
fromTreeExt :: (ExpSYM repr, MulSYM repr) => (Tree -> Either ErrMsg repr) -> (Tree -> Either ErrMsg repr)
fromTreeExt self (Node "Mul" [e1, e2]) = liftM2 mul (self e1) (self e2)
fromTreeExt self e                     = S.fromTreeExt self e

fromTree :: (ExpSYM repr, MulSYM repr) => (Tree -> Either ErrMsg repr)
fromTree = S.fix fromTreeExt

tf1'_int3 :: IO ()
tf1'_int3 = S.check_consume S.thrice . fromTree $ S.tf1_tree

tfm1'_int3 :: IO ()
tfm1'_int3 = S.check_consume S.thrice . fromTree $ S.toTree tfm1
