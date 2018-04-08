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
      describe "rowIn" $ do
        let v1 = V.fromList [N.Checked, N.Blank]
            v2 = V.fromList [N.Checked, N.Checked]
            v3 = V.fromList [N.Blank, N.Checked]
        it "tells if every Checked from 1st is in 2nd" $ do N.rowIn v1 v2 `shouldBe` True
        it "rejects when 1st has Checked where 2nd does not" $ do N.rowIn v1 v3 `shouldBe` False
        it "rejects when 1st has X and 2nd Checked" $ do
          N.rowIn (V.fromList [N.X]) (V.fromList [N.Checked]) `shouldBe` False
      describe "allValidRows" $ do
        let empty = V.generate 4 (const N.Blank)
            one = V.fromList [N.Blank, N.Checked, N.Blank, N.Blank]
            v1 = V.fromList [N.Checked, N.Checked, N.Blank, N.Blank]
            v2 = V.fromList [N.Blank, N.Checked, N.Checked, N.Blank]
            v3 = V.fromList [N.Blank, N.Blank, N.Checked, N.Checked]
            v4 = V.fromList [N.Checked, N.Checked, N.Checked, N.Checked]
        it "empty; 2" $ do N.allValidRows empty [2] `shouldMatchList` [v1, v2, v3]
        it "one; 2" $ do N.allValidRows one [2] `shouldMatchList` [v1, v2]
        it "empty; 4" $ do N.allValidRows empty [4] `shouldMatchList` [v4]
        it "empty 5; 1-1-1" $ do
          N.allValidRows (V.generate 5 (const N.Blank)) [1, 1, 1] `shouldMatchList`
            [V.fromList [N.Checked, N.Blank, N.Checked, N.Blank, N.Checked]]
        it "partial; " $ do
          let v = V.fromList [N.Blank, N.Blank, N.Checked, N.Checked, N.Checked, N.Blank, N.Blank, N.Blank, N.Checked]
          N.allValidRows v [5, 1] `shouldMatchList`
            [ V.fromList [N.Checked, N.Checked, N.Checked, N.Checked, N.Checked, N.Blank, N.Blank, N.Blank, N.Checked]
            , V.fromList [N.Blank, N.Checked, N.Checked, N.Checked, N.Checked, N.Checked, N.Blank, N.Blank, N.Checked]
            , V.fromList [N.Blank, N.Blank, N.Checked, N.Checked, N.Checked, N.Checked, N.Checked, N.Blank, N.Checked]
            ]
