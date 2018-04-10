module Nonogram.Util where

import qualified Data.Vector as V
import           Nonogram

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

printBoard :: Board -> IO ()
printBoard = V.mapM_ printRow
  where
    printRow row = V.mapM_ printX row >> putStrLn ""
    printX X       = putStr "."
    printX Blank   = putStr "."
    printX Checked = putStr "#"

printBoardDebug :: Board -> IO ()
printBoardDebug = V.mapM_ printRow
  where
    printRow row = V.mapM_ printX row >> putStrLn ""
    printX X       = putStr " X "
    printX Blank   = putStr " . "
    printX Checked = putStr " # "
