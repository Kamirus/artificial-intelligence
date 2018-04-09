module Zad1 where

import           Nonogram
import           Nonogram.Inference
import           Nonogram.Util

infer :: Nonogram -> IO Nonogram
infer n = do
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
  res <- infer nonogram
  printBoard $ board res
