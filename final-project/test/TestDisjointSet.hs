module TestDisjointSet where

import Control.Monad
import Control.Monad.ST
import DisjointSet
import Test.Hspec

inSameSet :: DisjointSet s -> Int -> Int -> ST s Bool
inSameSet ds x y = do
  rx <- find ds x
  ry <- find ds y
  return $ rx == ry

getRepresentatives :: DisjointSet s -> [Int] -> ST s [Int]
getRepresentatives ds = mapM (find ds)

runTestDisjointSet :: IO ()
runTestDisjointSet =
  hspec $ do
    describe "DisjointSet" $ do
      it "creates a new disjoint set with elements in their own sets" $ do
        let result =
              runST $ do
                ds <- createDisjointSet 5
                forM [1 .. 5] $ \i -> do
                  rep <- find ds i
                  return (i == rep)
        result `shouldBe` replicate 5 True
      it "correctly identifies elements in the same set" $ do
        let result =
              runST $ do
                ds <- createDisjointSet 5
                union ds 1 2
                union ds 2 3
                inSameSet ds 1 3
        result `shouldBe` True
      it "correctly identifies elements in different sets" $ do
        let result =
              runST $ do
                ds <- createDisjointSet 5
                union ds 1 2
                union ds 3 4
                inSameSet ds 2 4
        result `shouldBe` False
      it "maintains path compression after multiple operations" $ do
        let result =
              runST $ do
                ds <- createDisjointSet 5
                union ds 1 2
                union ds 2 3
                union ds 3 4
                union ds 4 5
                reps <- getRepresentatives ds [1 .. 5]
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
                inSameSet ds 1 6
        result `shouldBe` True
      it "maintains reflexive property" $ do
        let result =
              runST $ do
                ds <- createDisjointSet 3
                inSameSet ds 1 1
        result `shouldBe` True
      it "maintains symmetric property" $ do
        let result =
              runST $ do
                ds <- createDisjointSet 3
                union ds 1 2
                liftM2 (&&) (inSameSet ds 1 2) (inSameSet ds 2 1)
        result `shouldBe` True
      it "maintains transitive property" $ do
        let result =
              runST $ do
                ds <- createDisjointSet 4
                union ds 1 2
                union ds 2 3
                inSameSet ds 1 3
        result `shouldBe` True
