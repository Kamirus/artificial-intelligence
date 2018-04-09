module Zad2 where

import           Nonogram
import           Nonogram.Inference
import           Nonogram.Solver
import           Nonogram.Util

main :: IO ()
main = do
  (rows, cols) <- readInput
--  print rows
--  print cols
  let nonogram = mkNonogram rows cols
  let res = solve nonogram
  printBoard $ board res
