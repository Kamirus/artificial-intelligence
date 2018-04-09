module Main where

import qualified Data.Vector        as V
import           Nonogram
import           Nonogram.Inference
import           Test.Hspec

main :: IO ()
main =
  hspec $ do
    describe "Nonogram" $ do
      describe "intersectRow" $ do
        it "returns intersection of Field vectors, leaving Blank when different at position" $ do
          let v1 = V.fromList [Blank, Checked, Blank, Checked]
              v2 = V.fromList [Checked, Checked, Blank, Blank]
              rs = V.fromList [Blank, Checked, Blank, Blank]
           in intersectRow v1 v2 `shouldBe` rs
      describe "rowIn" $ do
        let v1 = V.fromList [Checked, Blank]
            v2 = V.fromList [Checked, Checked]
            v3 = V.fromList [Blank, Checked]
        it "tells if every Checked from 1st is in 2nd" $ do rowIn v1 v2 `shouldBe` True
        it "rejects when 1st has Checked where 2nd does not" $ do rowIn v1 v3 `shouldBe` False
        it "rejects when 1st has X and 2nd Checked" $ do rowIn (V.fromList [X]) (V.fromList [Checked]) `shouldBe` False
      describe "allValidRows" $ do
        let empty = V.generate 4 (const Blank)
            one = V.fromList [Blank, Checked, Blank, Blank]
            v1 = V.fromList [Checked, Checked, X, X]
            v2 = V.fromList [X, Checked, Checked, X]
            v3 = V.fromList [X, X, Checked, Checked]
            v4 = V.fromList [Checked, Checked, Checked, Checked]
        it "empty; 2" $ do allValidRows empty [2] `shouldMatchList` [v1, v2, v3]
        it "one; 2" $ do allValidRows one [2] `shouldMatchList` [v1, v2]
        it "empty; 4" $ do allValidRows empty [4] `shouldMatchList` [v4]
        it "empty 5; 1-1-1" $ do
          allValidRows (V.generate 5 (const Blank)) [1, 1, 1] `shouldMatchList`
            [V.fromList [Checked, X, Checked, X, Checked]]
        it "partial; " $ do
          let v = V.fromList [Blank, Blank, Checked, Checked, Checked, Blank, Blank, Blank, Checked]
          allValidRows v [5, 1] `shouldMatchList`
            [ V.fromList [Checked, Checked, Checked, Checked, Checked, X, X, X, Checked]
            , V.fromList [X, Checked, Checked, Checked, Checked, Checked, X, X, Checked]
            , V.fromList [X, X, Checked, Checked, Checked, Checked, Checked, X, Checked]
            ]
