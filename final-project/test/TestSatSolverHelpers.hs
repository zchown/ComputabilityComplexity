{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module TestSatSolverHelpers where

import Data.Maybe (fromJust)

import SatSolvers
import qualified SatTypes as S
import Test.Hspec

runTestSatHelper :: IO ()
runTestSatHelper =
  hspec $ do
    describe "checkProblem" $ do
      it "returns True for a problem with no zero clauses" $ do
        let problem = fromJust $ S.satProblemFromList @4 [[1, 2, 3], [1, 2, 4]]
        checkProblem problem `shouldBe` True
      it "returns False for a problem with a zero clause" $ do
        let problem = fromJust $ S.satProblemFromList @4 [[1, 2, 3], []]
        checkProblem problem `shouldBe` False
    describe "findUnits" $ do
      it "returns Nothing for a problem with no unit clauses" $ do
        let problem = fromJust $ S.satProblemFromList @4 [[1, 2, 3], [1, 2, 4]]
        findUnits problem `shouldBe` Nothing
      it "returns Just a unit clause for a problem with a unit clause" $ do
        let problem =
              fromJust $ S.satProblemFromList @4 [[1, 2, 3], [1, 2, 4], [1]]
        findUnits problem `shouldBe`
          Just
            (UnitPropagate
               (S.createVarAssignment
                  (fromJust (S.setBit (S.createVarList @4) 0))
                  (S.createVarList @4)))
      it "successfully finds multiple unit clauses" $ do
        let problem =
              fromJust $
              S.satProblemFromList @4 [[1, 2, 3], [1, 2, 4], [1], [2], [3]]
        findUnits problem `shouldBe`
          Just
            (UnitPropagate
               (S.createVarAssignment
                  (S.varListFromList @4 [True, True, True, False])
                  (S.createVarList @4)))
      it "finds negative unit clauses" $ do
        let problem =
              fromJust $
              S.satProblemFromList @4 [[1, 2, 3], [1, 2, 4], [1], [-2], [3]]
        findUnits problem `shouldBe`
          Just
            (UnitPropagate
               (S.createVarAssignment
                  (S.varListFromList @4 [True, False, True, False])
                  (S.varListFromList @4 [False, True, False, False])))
    describe "unitPropagate" $ do
      it "unitPropagate single unit solve" $ do
        let problem =
              fromJust $ S.satProblemFromList @4 [[1, 2, 3], [1, 2, 4], [1]]
        let unit = fromJust $ findUnits problem
        let newProblem = unitPropagate problem unit
        newProblem `shouldBe` fromJust (S.satProblemFromList @4 [])
      it "unitPropagate single unit doesn't solve" $ do
        let problem =
              fromJust $ S.satProblemFromList @4 [[1, 2, 3], [2, 4], [1]]
        let unit = fromJust $ findUnits problem
        let newProblem = unitPropagate problem unit
        newProblem `shouldBe` fromJust (S.satProblemFromList @4 [[2, 4]])
      it "unitPropagate negative unit" $ do
        let problem =
              fromJust $ S.satProblemFromList @4 [[1, 2, 3], [2, 4], [-2]]
        let unit = fromJust $ findUnits problem
        let newProblem = unitPropagate problem unit
        newProblem `shouldBe` fromJust (S.satProblemFromList @4 [[1, 3], [4]])
      it "unitPropagate multiple units" $ do
        let problem =
              fromJust $ S.satProblemFromList @4 [[1, 2, 3], [2, 4], [1], [4]]
        let unit = fromJust $ findUnits problem
        let newProblem = unitPropagate problem unit
        newProblem `shouldBe` fromJust (S.satProblemFromList @4 [])
      it "unitPropagate unsat" $ do
        let problem =
              fromJust $ S.satProblemFromList @4 [[1, 2, 3], [2, 4], [-2], [-4]]
        let unit = fromJust $ findUnits problem
        let newProblem = unitPropagate problem unit
        newProblem `shouldBe` fromJust (S.satProblemFromList @4 [[1, 3], []])
    describe "addNewUnitClause" $ do
      it "addNewUnitClause empty assignment" $ do
        let problem = fromJust $ S.satProblemFromList @4 [[1, 2, 3], [2, 4]]
        let assignment =
              S.createVarAssignment (S.createVarList @4) (S.createVarList @4)
        let newProblem = addNewUnitClause problem assignment
        newProblem `shouldBe`
          Just
            ( fromJust (S.satProblemFromList @4 [[1], [1, 2, 3], [2, 4]])
            , S.createVarAssignment @4
                (fromJust (S.setBit (S.createVarList @4) 0))
                (S.createVarList @4)
            , 0)
      it "addNewUnitClause positive assignment" $ do
        let problem = fromJust $ S.satProblemFromList @4 [[1, 2, 3], [2, 3]]
        let assignment@(S.VarAssignment (p, _)) =
              S.createVarAssignment
                (fromJust (S.setBit (S.createVarList @4) 3))
                (S.createVarList @4)
        let newProblem = addNewUnitClause problem assignment
        newProblem `shouldBe`
          Just
            ( fromJust (S.satProblemFromList @4 [[1], [1, 2, 3], [2, 3]])
            , S.createVarAssignment @4
                (fromJust (S.setBit p 0))
                (S.createVarList @4)
            , 0)
    describe "unitPropagateReduce" $ do
      it "unitPropagateReduce no units" $ do
        let problem = fromJust $ S.satProblemFromList @4 [[1, 2, 3], [2, 3]]
        let assignment =
              S.createVarAssignment (S.createVarList @4) (S.createVarList @4)
        let (newProblem, newAssignment) = unitPropagateReduce problem assignment
        newProblem `shouldBe` problem
        newAssignment `shouldBe` assignment
      it "unitPropagateReduce single unit" $ do
        let problem =
              fromJust $ S.satProblemFromList @4 [[1, 2, 3], [2, 3], [1]]
        let assignment =
              S.createVarAssignment (S.createVarList @4) (S.createVarList @4)
        let (newProblem, newAssignment) = unitPropagateReduce problem assignment
        newProblem `shouldBe` fromJust (S.satProblemFromList @4 [[2, 3]])
        newAssignment `shouldBe`
          S.createVarAssignment @4
            (fromJust (S.setBit (S.createVarList @4) 0))
            (S.createVarList @4)
      it "unitPropagateReduce multiple reductions" $ do
        let problem =
              fromJust $ S.satProblemFromList @4 [[1, 2, 3, 4], [-1], [1, 2]]
        let assignment =
              S.createVarAssignment (S.createVarList @4) (S.createVarList @4)
        let (newProblem, newAssignment) = unitPropagateReduce problem assignment
        newProblem `shouldBe` fromJust (S.satProblemFromList @4 [])
        newAssignment `shouldBe`
          S.createVarAssignment @4
            (fromJust (S.setBits (S.createVarList @4) [1]))
            (fromJust (S.setBit (S.createVarList @4) 0))
      it "unitPropagateReduce complete reduction" $ do
        let problem =
              fromJust $
              S.satProblemFromList @4 [[1, 2, 3, 4], [-1], [1, -3], [3, 2]]
        let assignment =
              S.createVarAssignment @4 (S.createVarList @4) (S.createVarList @4)
        let (newProblem, newAssignment) = unitPropagateReduce problem assignment
        newProblem `shouldBe` fromJust (S.satProblemFromList @4 [])
        newAssignment `shouldBe`
          S.createVarAssignment @4
            (fromJust (S.setBit (S.createVarList @4) 1))
            (fromJust (S.setBits (S.createVarList @4) [0, 2]))
      it "unitPropagateReduce no solution" $ do
        let problem = fromJust $ S.satProblemFromList @4 [[1, 2], [-1], [-2]]
        let assignment =
              S.createVarAssignment @4 (S.createVarList @4) (S.createVarList @4)
        let (newProblem, newAssignment) = unitPropagateReduce problem assignment
        newProblem `shouldBe` fromJust (S.satProblemFromList @4 [[]])
        newAssignment `shouldBe`
          S.createVarAssignment @4
            (S.createVarList @4)
            (fromJust (S.setBits (S.createVarList @4) [0, 1]))
