module TestDisjointSet where

import Control.Monad
import Control.Monad.ST
import DisjointSet
import Test.Hspec

runTestDisjointSet :: IO ()
runTestDisjointSet =
  hspec $ do
    describe "DisjointSet" $ do
      it "creates a new disjoint set with elements in their own sets" $ do
        let result =
              runST $ do
                ds <- createDisjointSet 5
                forM [1 .. 5] $ \i -> do
                  rep <- getRepresentative ds i
                  return (i == rep)
        result `shouldBe` replicate 5 True
      it "correctly identifies elements in the same set" $ do
        let result =
              runST $ do
                ds <- createDisjointSet 5
                union ds 1 2
                union ds 2 3
                find ds 1 3
        result `shouldBe` True
      it "correctly identifies elements in different sets" $ do
        let result =
              runST $ do
                ds <- createDisjointSet 5
                union ds 1 2
                union ds 3 4
                find ds 2 4
        result `shouldBe` False
      it "maintains path compression after multiple operations" $ do
        let result =
              runST $ do
                ds <- createDisjointSet 5
                union ds 1 2
                union ds 2 3
                union ds 3 4
                union ds 4 5
                reps <- forM [1 .. 5] $ getRepresentative ds
                return $ all (== head reps) reps
        result `shouldBe` True
      it "handles rank-based union correctly" $ do
        let result =
              runST $ do
                ds <- createDisjointSet 6
                union ds 1 2
                union ds 2 3
                union ds 4 5
                union ds 5 6
                union ds 3 6
                rep1 <- getRepresentative ds 1
                rep6 <- getRepresentative ds 6
                return (rep1 == rep6)
        result `shouldBe` True
      it "maintains reflexive property" $ do
        let result =
              runST $ do
                ds <- createDisjointSet 3
                find ds 1 1
        result `shouldBe` True
      it "maintains symmetric property" $ do
        let result =
              runST $ do
                ds <- createDisjointSet 3
                union ds 1 2
                liftM2 (==) (find ds 1 2) (find ds 2 1)
        result `shouldBe` True
      it "maintains transitive property" $ do
        let result =
              runST $ do
                ds <- createDisjointSet 4
                union ds 1 2
                union ds 2 3
                find ds 1 3
        result `shouldBe` True
