{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module TestSatTypes where

import qualified Data.Bits as B
import Data.Maybe (fromJust)

import qualified SatTypes as S
import Test.Hspec

type TestSize = 128

type OddTestSize = 100

type SmallTestSize = 17

runTestSatType :: IO ()
runTestSatType =
  hspec $ do
    describe "VarList operations" $ do
      it "creates empty VarList correctly for standard size" $ do
        let emptyList = S.createVarList @TestSize
        S.varListIsZero emptyList `shouldBe` True
        S.varListSize emptyList `shouldBe` 128
      it "creates empty VarList correctly for non-64-aligned size" $ do
        let emptyList = S.createVarList @OddTestSize
        S.varListIsZero emptyList `shouldBe` True
        S.varListSize emptyList `shouldBe` 100
        let emptyList' = S.createVarList @SmallTestSize
        S.varListIsZero emptyList' `shouldBe` True
        S.varListSize emptyList' `shouldBe` 17
      it "handles bits beyond size limit correctly" $ do
        let list = S.createVarList @SmallTestSize
            list' = fromJust $ S.setBit list 16
        B.testBit list' 16 `shouldBe` True
        S.setBit list 17 `shouldBe` Nothing
        S.setBit list (-1) `shouldBe` Nothing
        let fullList = S.createVarList @TestSize
            fullList' = fromJust $ S.setBit fullList 127
        B.testBit fullList' 127 `shouldBe` True
        B.testBit fullList' 126 `shouldBe` False
        S.setBit fullList 128 `shouldBe` Nothing
        S.setBit fullList (-1) `shouldBe` Nothing
      it "performs bitwise operations correctly with aligned size" $ do
        let list = fromJust $ S.setBit (S.createVarList @TestSize) 0
            list' = fromJust $ S.setBit (S.createVarList @TestSize) 1
            list'' = fromJust $ S.setBit list 1
            andResult = list B..&. list'
            andResult' = list'' B..&. list'
            orResult = list B..|. list'
        B.testBit andResult 0 `shouldBe` False
        B.testBit andResult' 1 `shouldBe` True
        B.testBit orResult 0 `shouldBe` True
        B.testBit orResult 1 `shouldBe` True
        B.testBit orResult 2 `shouldBe` False
      it "performs bitwise operations correctly with non-aligned size" $ do
        let list1 = fromJust $ S.setBit (S.createVarList @OddTestSize) 98
            list2 = fromJust $ S.setBit (S.createVarList @OddTestSize) 99
            list1' = fromJust $ S.setBit list1 99
            andResult = list1 B..&. list2
            andResult' = list1' B..&. list2
            orResult = list1 B..|. list2
        B.testBit andResult 98 `shouldBe` False
        B.testBit andResult 99 `shouldBe` False
        B.testBit andResult' 99 `shouldBe` True
        B.testBit orResult 98 `shouldBe` True
        B.testBit orResult 99 `shouldBe` True
        B.testBit orResult 97 `shouldBe` False
        let orResult' = fromJust $ S.clearBit orResult 98
        B.testBit orResult' 98 `shouldBe` False
      it "handles complement operation correctly with non-aligned size" $ do
        let list = S.createVarList @SmallTestSize
            complemented = B.complement list
        all (\i -> B.testBit complemented i) [0 .. 16] `shouldBe` True
        B.testBit complemented 17 `shouldBe` False
        B.testBit complemented 63 `shouldBe` False
        let list' = S.createVarList @OddTestSize
            complemented' = B.complement list'
        all (\i -> B.testBit complemented' i) [0 .. 99] `shouldBe` True
        B.testBit complemented' 100 `shouldBe` False
        B.testBit complemented' 127 `shouldBe` False
    describe "Clause operations" $ do
      it "creates empty clause correctly for non-aligned size" $ do
        let clause = S.createClause @OddTestSize
        S.isClauseEmpty clause `shouldBe` True
      it "handles variables near size boundary" $ do
        let clause = S.createClause @OddTestSize
            clause' = fromJust $ S.addPositiveVar clause 98
            clause'' = fromJust $ S.addNegativeVar clause' 99
        S.isPositiveVar clause'' 98 `shouldBe` True
        S.isNegativeVar clause'' 99 `shouldBe` True
        S.addPositiveVar clause'' 100 `shouldBe` Nothing
      it "creates clause from list correctly with non-aligned size" $ do
        let clause = S.unsafeClauseFromList @OddTestSize [99, -98, 97]
        S.isPositiveVar clause 98 `shouldBe` True
        S.isNegativeVar clause 97 `shouldBe` True
        S.isPositiveVar clause 96 `shouldBe` True
