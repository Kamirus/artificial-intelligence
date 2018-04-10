module Main
where

import           Nonogram
import           Nonogram.Inference
import           Nonogram.Solver
import           Nonogram.Util

main :: IO ()
main = do
  (rows, cols) <- readInput
  let nonogram = mkNonogram rows cols
  printBoard $ board $ solve nonogram
  -- case inferMax nonogram of
  --   Left msg -> print msg
  --   Right n -> printBoard $ board n

