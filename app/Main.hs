module Main where

import           Exp.Mul           (tfm1, tfm2)
import           Exp.Sum           (tf1)
import qualified PushNegF          as PnF
import           PushNegl          (ti1_norm_eval, ti1_norm_view)
import           Sem.Eval
import qualified Sem.MulTree       as MT
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
          putStrLn ">>>> with MulTree"
          putStrLn "# tf1'_int3"
          MT.tf1'_int3
          putStrLn "# tfm1’_int3 "
          MT.tfm1'_int3
          putStrLn "# PushNegl"
          print ti1_norm_view
          print ti1_norm_eval
          putStrLn "# PushNegF"
          print $ view (PnF.push_neg tf1)
