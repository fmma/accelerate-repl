module Main where

import Data.Array.Accelerate
import Data.Array.Accelerate.Interpreter
import Data.Array.Accelerate.AST (Idx(..), prj)
import System.Environment (getArgs, getProgName)
import Prelude hiding (zipWith, fst, snd, map, fromIntegral)


test = loop 
       $ toStream (use (fromList (Z :. (10 :: Int)) [1,2,3,4,5,6,7,8,9,10 :: Int]))
       $ mapStream (map (+10)) ZeroIdx
       $ foldStream (zipWith (+)) (use (fromList Z [0 :: Int])) ZeroIdx
       $ foldStream (zipWith max) (use (fromList Z [0 :: Int])) ZeroIdx
       $ emptyLoop

test2 = zipWith (+) (asnd . afst $ test) (asnd test)

iota :: Int -> Acc (Vector Int)
iota n = generate (index1 (constant n)) unindex1

logsum :: Int -> Acc (Scalar Float)
logsum n = asnd $ loop
         $ toStream (iota n)
         $ mapStream (map (log . fromIntegral . (+1))) ZeroIdx
         $ foldStream (zipWith (+)) (use (fromList Z [0.0])) ZeroIdx
         $ emptyLoop

main :: IO ()
main = do
  args <- getArgs
  progname <- getProgName
  case args of
    [n] -> do 
      putStrLn $ show $ run (logsum (read n))
      return ()
    _ -> putStrLn $ "usage " Prelude.++ progname Prelude.++ " n"
