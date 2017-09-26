module Main where

import           Exp.Mul           (tfm1, tfm2)
import           Exp.Sum           (tf1)
import           Sem.Eval
import           Sem.PrettyPrinter
import           Sem.Tree

main :: IO ()
main = do print (tf1 :: Int)
          putStrLn (tf1 :: String)
          print (tfm1 :: Int)
          putStrLn (tfm1 :: String)
          print (tfm2 :: Int)
          putStrLn (tfm2 :: String)
          print tf1_tree
          tf1'_eval
          tf1'_int3
          putStrLn ">>>> with Wrapper"
          putStrLn "# tf1E_int3"
          tf1E_int3
          putStrLn "# tfxE_int3"
          tfxE_int3
