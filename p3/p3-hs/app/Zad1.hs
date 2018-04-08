module Zad1 where

import qualified Nonogram as N

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
  let nonogram = N.mkNonogram rows cols
  res <- aux nonogram
  N.printBoard $ N.board res

  -- let n1 = N.inferRows nonogram
  --     n2 = N.inferCols n1
  --     n3 = N.inferRows n2
  --     n4 = N.inferCols n3
  --     n5 = N.inferRows n4
  
  -- N.printBoardDebug $ N.board nonogram; putStrLn ""
  -- N.printBoardDebug $ N.board n1; putStrLn ""
  -- N.printBoardDebug $ N.board n2; putStrLn ""
  -- N.printBoardDebug $ N.board n3; putStrLn ""
  -- N.printBoardDebug $ N.board n4; putStrLn ""
  -- N.printBoardDebug $ N.board n5; putStrLn ""

  where
    aux :: N.Nonogram -> IO N.Nonogram
    aux n = do
      N.printBoardDebug $ N.board n
      putStrLn ""
      case N.inferStep n of
        Nothing -> return n
        Just newN -> aux newN
