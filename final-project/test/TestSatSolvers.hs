{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module TestSatSolvers where

import Data.Maybe (fromJust)

import SatSolvers
import qualified SatTypes as S
import Test.Hspec

runTestSatSolver :: IO ()
runTestSatSolver =
  hspec $ do
    describe "dpll" $ do
      it "solves empty problem" $ do
        let problem = fromJust $ S.satProblemFromList @4 []
        dpll
          problem
          (S.createVarAssignment (S.createVarList @4) (S.createVarList @4)) `shouldBe`
          Just (S.createVarAssignment (S.createVarList @4) (S.createVarList @4))
      it "solves single clause problem" $ do
        let problem = fromJust $ S.satProblemFromList @4 [[1]]
        dpll
          problem
          (S.createVarAssignment (S.createVarList @4) (S.createVarList @4)) `shouldBe`
          Just
            (S.createVarAssignment
               (S.createVarList @4)
               (S.varListFromList @4 [True, False, False, False]))
