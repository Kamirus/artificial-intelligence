module Nonogram where

import           Control.Monad               (forM_)
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

mkNonogram :: [Constraint] -> [Constraint] -> Nonogram
mkNonogram raw_rows raw_cols = Nonogram rows cols board
  where
    [rows, cols] = map V.fromList [raw_rows, raw_cols]
    [n, m] = map V.length [rows, cols]
    board = V.generate n $ \i -> V.generate m $ const Blank
--replaceCol :: Int -> Row -> Board -> Board
--replaceCol = aux
--  where
--    aux :: Int -> Row -> Board -> Board
--    aux j col b = do
--      G.create $ do
--        newB <- GM.new $ G.length b
--        forM_
--          [0 .. G.length b - 1]
--          (\i -> do
--             let oldRow = b G.! i
--                 newRow = oldRow // [(j, col ! i)]
--             GM.unsafeWrite newB i newRow)
--        return newB
--getCol :: Int -> Board -> Row
--getCol j b = G.generate (G.length b) $ \i -> b ! i ! j
--someCol :: Int -> Row
--someCol i = G.fromList $ take i $ cycle [Checked]
