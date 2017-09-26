module Sem.Tree where

import           Control.Applicative ((<$>))
import           Control.Monad       (liftM, liftM2)
import           Exp.Mul             (MulSYM(..))
import           Exp.Sum
import           Sem.Eval
import           Sem.PrettyPrinter

data Tree = Leaf String
          | Node String [Tree]
          deriving (Eq, Read, Show)


type ErrMsg = String

instance ExpSYM Tree where
  lit n = Node "Lit" [Leaf $ show n]
  neg e = Node "Neg" [e]
  add e1 e2 = Node "Add" [e1, e2]

instance MulSYM Tree where
  mul e1 e2 = Node "Mul" [e1, e2]

instance (ExpSYM repr, ExpSYM repr') => ExpSYM (repr, repr') where
  lit x = (lit x, lit x)
  neg (e1, e2) = (neg e1, neg e2)
  add (e11, e12) (e21, e22) = (add e11 e21, add e12 e22)

instance (MulSYM repr, MulSYM repr') => MulSYM (repr, repr') where
  mul (e11, e12) (e21, e22) = (mul e11 e21, mul e12 e22)

toTree :: Tree -> Tree
toTree = id

safeRead :: Read a => String -> Either ErrMsg a
safeRead s = case reads s of
  [(x, "")] -> Right x
  _         -> Left $ "Read error " ++ s

duplicate :: (ExpSYM repr, ExpSYM repr') => (repr, repr') -> (repr, repr')
duplicate = id

check_consume :: (t -> IO ()) -> Either String t -> IO ()
check_consume _ (Left e)  = putStrLn $ "Error: " ++ e
check_consume f (Right x) = f x

dup_consume :: (ExpSYM b, ExpSYM t, Show a) => (t -> a) -> (t, b) -> IO b
dup_consume ev x = print (ev  x1) >> return x2
  where
     (x1, x2) = duplicate x

thrice :: (Int, (String, Tree)) -> IO ()
thrice x = dup_consume eval x >>= dup_consume view >>= print . toTree

tf1'_int3 :: IO ()
tf1'_int3 = check_consume thrice . fromTree $ tf1_tree

fromTree :: ExpSYM r => Tree -> Either ErrMsg r
fromTree (Node "Lit" [Leaf n]) = lit <$> safeRead n
fromTree (Node "Neg" [e])      = neg <$> fromTree e
fromTree (Node "Add" [e1, e2]) = liftM2 add (fromTree e1) (fromTree e2)
fromTree e                     = Left $ "Invalid tree " ++ show e


fromTreeExt :: (ExpSYM repr) => (Tree -> Either ErrMsg repr) -> (Tree -> Either ErrMsg repr)
fromTreeExt self (Node "Lit" [Leaf n]) = lit <$> safeRead n
fromTreeExt self (Node "Neg" [e])      = neg <$> self e
fromTreeExt self (Node "Add" [e1,e2])  = liftM2 add (self e1) (self e2)
fromTreeExt self e                     = Left $ "Invalid tree: " ++ show e

fix :: (t -> t) -> t
fix f = f (fix f)

fromTree' :: Tree -> Either ErrMsg (Int, (String, Tree))
fromTree' = fix fromTreeExt

tf1_tree :: Tree
tf1_tree = toTree tf1

tf1'_eval :: IO ()
tf1'_eval =
  let tf1' = fromTree tf1_tree
  in case tf1' of
    Left e  -> putStrLn $ "Error: " ++ e
    Right x -> print $ eval x


tf1E_int3 :: IO ()
tf1E_int3 = check_consume thrice . fromTree' $ tf1_tree

tfxE_int3 :: IO ()
tfxE_int3 = check_consume thrice . fromTree' $ Node "Lit" [Leaf "1", Leaf "2"]
