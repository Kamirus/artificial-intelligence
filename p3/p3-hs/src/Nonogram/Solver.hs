module Nonogram.Solver
where

import           Data.List                      ( (\\) )
import           Data.Vector.Generic            ( (!)
                                                , (//)
                                                )
import qualified Data.Vector.Generic           as G
import           Nonogram
import           Nonogram.Inference

bestGuess :: Nonogram -> [Int] -> Maybe Int
bestGuess nonogram visited = if guesses == []
  then Nothing
  else Just $ snd $ minimum guesses
 where
  b       = board nonogram
  indexes = [0 .. G.length rowC - 1] \\ visited
  rowC    = rows nonogram
  guesses =
    map (\i -> (length $ allValidRows (b G.! i) (rowC G.! i), i)) indexes

solve :: Nonogram -> Nonogram
solve nonogram = head $ aux nonogram 0 []
 where
  n = G.length $ board nonogram
  aux :: Nonogram -> Int -> [Int] -> [Nonogram]
  aux nonogram i visited = case inferMax nonogram of
    Left  msg  -> fail msg
    Right newN -> do
      let visited' = i : visited
      guessedRow <- allValidRows (board newN ! i) (rows newN ! i)
      case bestGuess nonogram visited' of
        Nothing -> return nonogram
        Just nextI ->
          aux (newN { board = board newN // [(i, guessedRow)] }) nextI visited'

