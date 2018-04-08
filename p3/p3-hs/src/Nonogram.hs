module Nonogram where

import           Control.Monad               (forM_)
import           Data.List
import qualified Data.Vector                 as V
import           Data.Vector.Generic         ((!), (//))
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

printBoard :: Board -> IO ()
printBoard b = do
  V.mapM_ printRow b
  where
    printRow row = V.mapM_ printX row >> putStrLn ""
    printX X       = putStr "."
    printX Blank   = putStr "."
    printX Checked = putStr "#"

printBoardDebug :: Board -> IO ()
printBoardDebug b = do
  V.mapM_ printRow b
  where
    printRow row = V.mapM_ printX row >> putStrLn ""
    printX X       = putStr " X "
    printX Blank   = putStr " . "
    printX Checked = putStr " # "

mkNonogram :: [[Int]] -> [[Int]] -> Nonogram
mkNonogram raw_rows raw_cols = Nonogram rows cols board
  where
    [rows, cols] = map V.fromList [raw_rows, raw_cols]
    [n, m] = map V.length [rows, cols]
    board = V.generate n $ \i -> V.generate m $ const Blank

replaceCol :: Int -> Row -> Board -> Board
replaceCol = aux
  where
    aux :: Int -> Row -> Board -> Board
    aux j col b = do
      G.create $ do
        newB <- GM.new $ G.length b
        forM_
          [0 .. G.length b - 1]
          (\i -> do
             let oldRow = b G.! i
                 newRow = oldRow // [(j, col ! i)]
             GM.unsafeWrite newB i newRow)
        return newB

getCol :: Int -> Board -> Row
getCol j b = G.generate (G.length b) $ \i -> b ! i ! j

someCol :: Int -> Row
someCol i = G.fromList $ take i $ cycle [Checked]

{- Inference -}
intersectRow :: Row -> Row -> Row
intersectRow = V.zipWith aux
  where
    aux a b
      | a == b = a
      | otherwise = Blank

rowIn :: Row -> Row -> Bool
--rowIn a b = V.all (True ==) $ V.zipWith in' a b
rowIn a = V.ifoldr (\i x acc -> acc && (a ! i) `in'` x) True
  where
    in' Checked Checked = True
    in' X X = True
    in' Blank _ = True
    in' _ _ = False
--    in' X Checked     = False
--    in' Checked X     = False
--    in' Checked Blank = False
--    in' _ _           = True

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
      return $ (1+off) : rest

buildRow :: Int -> Constraint -> [Int] -> Row
buildRow n c offsets =
  if V.length row == n
    then row
    else row V.++ (V.generate (n - V.length row) $ const X)
  where
    f :: Int -> Int -> [Field]
    f offset blockLen = [X | _ <- [1 .. offset]] ++ [Checked | _ <- [1 .. blockLen]]
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

inferRows :: Nonogram -> Nonogram
inferRows nonogram = Nonogram rowC colC $ b // bulk
  where
    b = board nonogram
    rowC = rows nonogram
    colC = cols nonogram
    bulk = G.ifoldr (\i c acc -> (i, inferAllCommon (b ! i) c) : acc) [] rowC

transposeBoard :: Board -> Board
transposeBoard =
  G.fromList . foldr (\rowL acc -> G.fromList rowL : acc) [] . transpose . G.foldr (\row acc -> G.toList row : acc) []

inferCols :: Nonogram -> Nonogram
inferCols nonogram = Nonogram rowC colC $ transposeBoard $ bT // bulk
  where
    bT = transposeBoard $ board nonogram
    rowC = rows nonogram
    colC = cols nonogram
    bulk = G.ifoldr (\i c acc -> (i, inferAllCommon (bT ! i) c) : acc) [] colC

inferStep :: Nonogram -> Maybe Nonogram
inferStep nonogram = if nonogram == newN then Nothing else Just newN
  where
    newN = aux nonogram
    aux = inferCols . inferRows

test =
  let row = V.generate 5 (const Blank)
      n = V.length row
      c = [3]
      blocksLen = length c
   in do print $ inferAllCommon row c
         print $ allValidRows row c
         print $ allRows n c
         print $ allOffsets (n - sum c - (length c - 1)) blocksLen
