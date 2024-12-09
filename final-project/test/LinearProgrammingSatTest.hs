{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}

module LinearProgrammingSatTest where

import Data.Maybe (fromJust)
import SatLinearProgramming
import qualified SatTypes as S
import Test.Hspec

runLinearProgrammingSatTest :: IO ()
runLinearProgrammingSatTest =
  hspec $ do
    describe "linear programming tests" $ do
      it "single variable, single clause" $ do
        let problem = fromJust $ S.satProblemFromList @1 [[1]]
        (S.Satisfiable sol) <- randomizedRounding problem
        let csol = fromJust $ S.setBit (S.createVarList @1) 0
        sol `shouldBe` csol
      it "two variable, two clause" $ do
        let problem = fromJust $ S.satProblemFromList @2 [[1], [2]]
        (S.Satisfiable sol) <- randomizedRounding problem
        let csol = fromJust $ S.setBits (S.createVarList @2) [0, 1]
        sol `shouldBe` csol
