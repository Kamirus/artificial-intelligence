module Zad1
where

import           Nonogram
import           Nonogram.Inference
import           Nonogram.Util

main :: IO ()
main = do
  (rows, cols) <- readInput
  let nonogram = mkNonogram rows cols
  case inferMax nonogram of
    Left  msg -> print msg
    Right res -> printBoard $ board res
