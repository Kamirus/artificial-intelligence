module Main where

import qualified Data.Vector as V
import qualified Nonogram    as N
import           Test.Hspec

main :: IO ()
main =
  hspec $ do
    describe "Nonogram" $ do
      describe "intersectRow" $ do
        it "returns intersection of Field vectors, leaving Blank when different at position" $ do
          let v1 = V.fromList [N.Blank, N.Checked, N.Blank, N.Checked]
              v2 = V.fromList [N.Checked, N.Checked, N.Blank, N.Blank]
              rs = V.fromList [N.Blank, N.Checked, N.Blank, N.Blank]
           in N.intersectRow v1 v2 `shouldBe` rs
        it "preserves X in left, when right has Blank" $ do
          let v1 = V.fromList [N.X, N.X, N.Blank]
              v2 = V.fromList [N.Checked, N.Blank, N.X]
              rs = V.fromList [N.Blank, N.X, N.Blank]
           in N.intersectRow v1 v2 `shouldBe` rs
      describe "rowIn" $ do
        let v1 = V.fromList [N.Checked, N.Blank]
            v2 = V.fromList [N.Checked, N.Checked]
            v3 = V.fromList [N.Blank, N.Checked]
        it "tells if every Checked from 1st is in 2nd" $ do N.rowIn v1 v2 `shouldBe` True
        it "rejects when 1st has Checked where 2nd does not" $ do N.rowIn v1 v3 `shouldBe` False
        it "rejects when 1st has X and 2nd Checked" $ do
          N.rowIn (V.fromList [N.X]) (V.fromList [N.Checked]) `shouldBe` False
