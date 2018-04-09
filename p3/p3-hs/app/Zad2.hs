module Zad2 where

import           Nonogram
import           Nonogram.Inference
import           Nonogram.Solver
import           Nonogram.Util

infer :: Nonogram -> IO Nonogram
infer n = do
  print "infer"
  case inferStep n of
    Left msg -> print msg >> return n
    Right newN ->
      if n == newN
        then return n
        else infer newN

main :: IO ()
main = do
  (rows, cols) <- readInput
--  print rows
--  print cols
  let nonogram = mkNonogram rows cols
  let res = solve nonogram
  printBoard $ board res
