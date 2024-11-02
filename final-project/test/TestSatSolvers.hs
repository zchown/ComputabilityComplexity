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
      it "unsatisfiable problem" $ do
        let problem =
              fromJust $
              S.satProblemFromList @4 [[1, 2, 3], [1, 2, 4], [1], [-1]]
        dpll
          problem
          (S.createVarAssignment (S.createVarList @4) (S.createVarList @4)) `shouldBe`
          S.Unsatisfiable
      it "solves empty problem" $ do
        let problem = fromJust $ S.satProblemFromList @4 []
        dpll
          problem
          (S.createVarAssignment (S.createVarList @4) (S.createVarList @4)) `shouldBe`
          S.Satisfiable (S.createVarList @4)
      it "solves single clause problem" $ do
        let problem = fromJust $ S.satProblemFromList @4 [[1]]
        dpll
          problem
          (S.createVarAssignment (S.createVarList @4) (S.createVarList @4)) `shouldBe`
          S.Satisfiable (S.varListFromList @4 [True, True, True, True])
      it "solves single clause problem with negative" $ do
        let problem = fromJust $ S.satProblemFromList @4 [[-1]]
        dpll
          problem
          (S.createVarAssignment (S.createVarList @4) (S.createVarList @4)) `shouldBe`
          S.Satisfiable (S.varListFromList @4 [False, True, True, True])
      it "solves simple problem" $ do
        let problem =
              fromJust $
              S.satProblemFromList @4 [[1, 2, 3], [-1, -2, -4], [3, 4]]
        dpll
          problem
          (S.createVarAssignment (S.createVarList @4) (S.createVarList @4)) `shouldBe`
          S.Satisfiable (S.varListFromList @4 [True, True, True, False])
