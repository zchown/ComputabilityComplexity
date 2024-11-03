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
      it "unsatisfiable problem - simple contradiction" $ do
        let problem = fromJust $ S.satProblemFromList @1 [[1], [-1]]
        dpll
          problem
          (S.createVarAssignment (S.createVarList @1) (S.createVarList @1)) `shouldBe`
          S.Unsatisfiable
      it "unsatisfiable problem - multiple clauses" $ do
        let problem =
              fromJust $
              S.satProblemFromList @2 [[1, 2], [1, -2], [-1, 2], [-1, -2]]
        dpll
          problem
          (S.createVarAssignment (S.createVarList @2) (S.createVarList @2)) `shouldBe`
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
          S.Satisfiable (S.varListFromList @4 [True, False, False, False])
      it "solves simple problem with multiple solutions" $ do
        let problem =
              fromJust $
              S.satProblemFromList @4 [[1, 2, 3], [-1, -2, -4], [3, 4]]
        dpll
          problem
          (S.createVarAssignment (S.createVarList @4) (S.createVarList @4)) `shouldBe`
          S.Satisfiable (S.varListFromList @4 [True, True, True, False])
      it "handles unit clauses with propagation" $ do
        let problem = fromJust $ S.satProblemFromList @3 [[1], [2, 3], [-2]]
        dpll
          problem
          (S.createVarAssignment (S.createVarList @3) (S.createVarList @3)) `shouldBe`
          S.Satisfiable (S.varListFromList @3 [True, False, True])
      it "solves problem requiring backtracking" $ do
        let problem =
              fromJust $
              S.satProblemFromList @4 [[1, 2], [-1, 2], [3, -4], [-2, -3]]
        dpll
          problem
          (S.createVarAssignment (S.createVarList @4) (S.createVarList @4)) `shouldBe`
          S.Satisfiable (S.varListFromList @4 [True, True, False, False])
      it "handles larger satisfiable problem" $ do
        let problem =
              fromJust $
              S.satProblemFromList @5
                [[1, 2, 3], [-1, 4], [2, -3, 5], [-2, -4], [3, -5]]
        dpll
          problem
          (S.createVarAssignment (S.createVarList @5) (S.createVarList @5)) `shouldBe`
          S.Satisfiable (S.varListFromList @5 [True, False, True, True, True])
      it "simple problem" $ do
        let problem =
              fromJust $ S.satProblemFromList @4 [[1, 2], [2, 3], [3, 4]]
        dpll
          problem
          (S.createVarAssignment (S.createVarList @4) (S.createVarList @4)) `shouldBe`
          S.Satisfiable (S.varListFromList @4 [True, True, True, False])
      it "handles all negative literals" $ do
        let problem = fromJust $ S.satProblemFromList @3 [[-1], [-2], [-3]]
        dpll
          problem
          (S.createVarAssignment (S.createVarList @3) (S.createVarList @3)) `shouldBe`
          S.Satisfiable (S.varListFromList @3 [False, False, False])
      it "unsatisfiable problem with unit propagation" $ do
        let problem =
              fromJust $ S.satProblemFromList @3 [[1], [-1, 2], [-2, 3], [-3]]
        dpll
          problem
          (S.createVarAssignment (S.createVarList @3) (S.createVarList @3)) `shouldBe`
          S.Unsatisfiable
