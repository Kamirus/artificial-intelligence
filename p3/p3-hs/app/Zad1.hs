module Zad1 where

import qualified Nonogram

readInput :: IO ([[Int]], [[Int]])
readInput = do
  line <- getLine
  let n:m:_ = map read $ words line :: [Int]
  rows <- readConstraints n
  cols <- readConstraints m
  return (rows, cols)
  where
    readConstraints 0 = return []
    readConstraints n = do
      line <- getLine
      let row = map read $ words line :: [Int]
      rest <- readConstraints $ n - 1
      return $ row : rest

main :: IO ()
main = do
  (rows, cols) <- readInput
  print rows
  print cols
