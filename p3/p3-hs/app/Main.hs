module Main
where

import           Control.Category               ( (>>>) )
import           Nonogram
import           Nonogram.Inference
import           Nonogram.Solver
import           Nonogram.Util

main :: IO ()
main = uncurry mkNonogram <$> readInput >>= (solve >>> board >>> printBoard)
