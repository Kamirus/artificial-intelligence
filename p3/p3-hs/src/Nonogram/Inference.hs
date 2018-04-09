module Nonogram.Inference where

import           Data.List           (transpose)
import           Data.Vector.Generic ((!), (//))
import qualified Data.Vector.Generic as G
import           Nonogram

intersectRow :: Row -> Row -> Row
intersectRow = G.zipWith aux
  where
    aux a b
      | a == b = a
      | otherwise = Blank

rowIn :: Row -> Row -> Bool
rowIn a = G.ifoldr (\i x acc -> acc && (a ! i) `in'` x) True
  where
    in' Checked Checked = True
    in' X X             = True
    in' Blank _         = True
    in' _ _             = False

allOffsets :: Int -> Int -> [[Int]]
allOffsets extraBlanks blocksNo = do
  firstOffset <- [0 .. extraBlanks]
  rest <- aux (extraBlanks - firstOffset) (blocksNo - 1)
  return $ firstOffset : rest
  where
    aux 0 n = [take n $ cycle [1]]
    aux _ 0 = [[]]
    aux blanks n = do
      off <- [0 .. blanks]
      rest <- aux (blanks - off) (n - 1)
      return $ (1 + off) : rest

buildRow :: Int -> Constraint -> [Int] -> Row
buildRow n c offsets =
  if G.length row == n
    then row
    else row G.++ (G.generate (n - G.length row) $ const X)
  where
    f :: Int -> Int -> [Field]
    f offset blockLen = [X | _ <- [1 .. offset]] ++ [Checked | _ <- [1 .. blockLen]]
    row = G.fromList $ concat $ zipWith f offsets c

allRows :: Int -> Constraint -> [Row]
allRows n c = [buildRow n c off | off <- allOffsets additionalBlanks blocksNo]
  where
    blocksNo = length c
    additionalBlanks = n - sum c - (blocksNo - 1)

allValidRows :: Row -> Constraint -> [Row]
allValidRows row c = filter (rowIn row) $ allRows (G.length row) c

inferAllCommon :: Row -> Constraint -> Row
inferAllCommon row c = foldl1 intersectRow $ allValidRows row c

inferAllCommon' :: Row -> Constraint -> Either String Row
inferAllCommon' row c =
  case allValidRows row c of
    [] -> Left "allValidRows returned empty list"
    xs -> Right $ foldl1 intersectRow xs

inferRows :: Nonogram -> Either String Nonogram
inferRows nonogram = Nonogram rowC colC <$> (b //) <$> (zip [0 ..] <$> bulk)
  where
    b = board nonogram
    rowC = rows nonogram
    colC = cols nonogram
    bulk = G.ifoldr (\i c acc -> (:) <$> inferAllCommon' (b ! i) c <*> acc) (Right []) rowC

transposeBoard :: Board -> Board
transposeBoard =
  G.fromList . foldr (\rowL acc -> G.fromList rowL : acc) [] . transpose . G.foldr (\row acc -> G.toList row : acc) []

inferCols :: Nonogram -> Either String Nonogram
inferCols nonogram = Nonogram rowC colC <$> transposeBoard <$> (bT //) <$> zip [0 ..] <$> bulk
  where
    bT = transposeBoard $ board nonogram
    rowC = rows nonogram
    colC = cols nonogram
    bulk = G.ifoldr (\i c acc -> (:) <$> inferAllCommon' (bT ! i) c <*> acc) (Right []) colC

inferStep :: Nonogram -> Either String Nonogram
inferStep nonogram = inferRows nonogram >>= inferCols
