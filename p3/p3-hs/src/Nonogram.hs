module Nonogram where

import           Data.List
import qualified Data.Vector                 as V
import qualified Data.Vector.Generic         as G
import qualified Data.Vector.Generic.Mutable as GM

type Constraint = [Int]

type Constraints = V.Vector Constraint

type Row = V.Vector Field

type Board = V.Vector Row

data Field
  = X
  | Blank
  | Checked
  deriving (Show, Eq)

data Nonogram = Nonogram
  { rows  :: Constraints
  , cols  :: Constraints
  , board :: Board
  } deriving (Show, Eq)

mkNonogram :: [[Int]] -> [[Int]] -> Nonogram
mkNonogram raw_rows raw_cols = Nonogram rows cols board
  where
    [rows, cols] = map V.fromList [raw_rows, raw_cols]
    [n, m] = map V.length [rows, cols]
    board = V.generate n $ \i -> V.generate m $ const Blank

intersectRow :: Row -> Row -> Row
intersectRow = V.zipWith aux
  where
    aux X Blank = X
    aux a b
      | a == b = a
      | otherwise = Blank

rowIn :: Row -> Row -> Bool
--rowIn a b = V.all (True ==) $ V.zipWith in' a b
rowIn a = V.ifoldr (\i x acc -> acc && (a V.! i) `in'` x) True
  where
    in' X Checked     = False
    in' Checked X     = False
    in' Checked Blank = False
    in' _ _           = True

allOffsets :: Int -> Int -> [[Int]]
allOffsets extraBlanks blocksNo = do
  firstOffset <- [0 .. extraBlanks]
  rest <- aux (extraBlanks - firstOffset) (blocksNo - 1)
  return $ firstOffset : rest
  where
    aux _ 0 = [[]]
    aux blanks n = do
      off <- [1 .. blanks]
      rest <- aux (blanks - off + 1) (n - 1)
      return $ off : rest

buildRow :: Int -> Constraint -> [Int] -> Row
buildRow n c offsets =
  if V.length row == n
    then row
    else row V.++ (V.generate (n - V.length row) $ const Blank)
  where
    f :: Int -> Int -> [Field]
    f offset blockLen = [Blank | _ <- [1 .. offset]] ++ [Checked | _ <- [1 .. blockLen]]
    row = V.fromList $ concat $ zipWith f offsets c

allRows :: Int -> Constraint -> [Row]
allRows n c = [buildRow n c off | off <- allOffsets additionalBlanks blocksNo]
  where
    blocksNo = length c
    additionalBlanks = n - sum c - (blocksNo - 1)

allValidRows :: Row -> Constraint -> [Row]
allValidRows row c = filter (rowIn row) $ allRows (V.length row) c

inferAllCommon :: Row -> Constraint -> Row
inferAllCommon row c = foldl1 intersectRow $ allValidRows row c

test =
  let row = V.generate 5 (const Blank)
      n = V.length row
      c = [3]
      blocksLen = length c
   in do print $ inferAllCommon row c
         print $ allValidRows row c
         print $ allRows n c
         print $ allOffsets (n - sum c - (length c - 1)) blocksLen
