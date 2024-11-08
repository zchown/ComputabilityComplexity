module TestKnapsack
  ( runKnapsackTests
  ) where

import Knapsack
import Test.Hspec

doubleConversion ::
     (KnapsackProblem Double -> KnapsackResult Double)
  -> KnapsackProblem Int
  -> KnapsackResult Int
doubleConversion f =
  convertKnapsackResultDoubleInt . f . convertKnapsackProblemIntDouble

runKnapsackTests :: IO ()
runKnapsackTests =
  hspec $ do
    describe "Knapsack Problem Tests" $ do
      context "No Items Tests" $ do
        it "Greedy Knapsack handles no items" $
          doubleConversion greedyKnapsack testKnapsackNoItems `shouldBe`
          KnapsackResult [] 0
        it "0/1 Knapsack handles no items" $
          knapsack01 testKnapsackNoItems `shouldBe` KnapsackResult [] 0
        it "Min Cost Knapsack handles no items" $
          minCostKnapsack testKnapsackNoItems `shouldBe` KnapsackResult [] 0
        it "FPTAS Knapsack handles no items" $
          fptasKnapsack testKnapsackNoItems `shouldBe` KnapsackResult [] 0
      context "Single Item Tests" $ do
        it "Greedy Knapsack handles single item" $
          doubleConversion greedyKnapsack testKnapsackSingle `shouldBe`
          KnapsackResult [Item 10 60] 60
        it "0/1 Knapsack handles single item" $
          knapsack01 testKnapsackSingle `shouldBe`
          KnapsackResult [Item 10 60] 60
        it "Min Cost Knapsack handles single item" $
          minCostKnapsack testKnapsackSingle `shouldBe`
          KnapsackResult [Item 10 60] 60
        it "FPTAS Knapsack handles single item" $
          fptasKnapsack testKnapsackSingle `shouldBe`
          KnapsackResult [Item 10 60] 60
      context "No Capacity Tests" $ do
        it "Greedy Knapsack handles no capacity" $
          doubleConversion greedyKnapsack testKnapsackNoCapacity `shouldBe`
          KnapsackResult [] 0
        it "0/1 Knapsack handles no capacity" $
          knapsack01 testKnapsackNoCapacity `shouldBe` KnapsackResult [] 0
        it "Min Cost Knapsack handles no capacity" $
          minCostKnapsack testKnapsackNoCapacity `shouldBe` KnapsackResult [] 0
        it "FPTAS Knapsack handles no capacity" $
          fptasKnapsack testKnapsackNoCapacity `shouldBe` KnapsackResult [] 0
      context "Null Tests" $ do
        it "Greedy Knapsack handles null" $
          doubleConversion greedyKnapsack testKnapsackNull `shouldBe`
          KnapsackResult [] 0
        it "0/1 Knapsack handles null" $
          knapsack01 testKnapsackNull `shouldBe` KnapsackResult [] 0
        it "Min Cost Knapsack handles null" $
          minCostKnapsack testKnapsackNull `shouldBe` KnapsackResult [] 0
        it "FPTAS Knapsack handles null" $
          fptasKnapsack testKnapsackNull `shouldBe` KnapsackResult [] 0
      context "No Fit Tests" $ do
        it "Greedy Knapsack handles no fit" $
          doubleConversion greedyKnapsack testKnapsackNoFit `shouldBe`
          KnapsackResult [] 0
        it "0/1 Knapsack handles no fit" $
          knapsack01 testKnapsackNoFit `shouldBe` KnapsackResult [] 0
        it "Min Cost Knapsack handles no fit" $
          minCostKnapsack testKnapsackNoFit `shouldBe` KnapsackResult [] 0
        it "FPTAS Knapsack handles no fit" $
          fptasKnapsack testKnapsackNoFit `shouldBe` KnapsackResult [] 0
      context "All Fit Tests" $ do
        it "Greedy Knapsack handles all fit" $
          doubleConversion greedyKnapsack testKnapsackAllFit `shouldBe`
          KnapsackResult [Item 10 60, Item 20 100, Item 30 120] 280
        it "0/1 Knapsack handles all fit" $
          knapsack01 testKnapsackAllFit `shouldBe`
          KnapsackResult [Item 10 60, Item 20 100, Item 30 120] 280
        it "Min Cost Knapsack handles all fit" $
          minCostKnapsack testKnapsackAllFit `shouldBe`
          KnapsackResult [Item 10 60, Item 20 100, Item 30 120] 280
        it "FPTAS Knapsack handles all fit" $
          fptasKnapsack testKnapsackAllFit `shouldBe`
          KnapsackResult [Item 10 60, Item 20 100, Item 30 120] 280
      context "Greedy Algorithm Specific Tests" $ do
        it "Greedy Knapsack takes max correctly" $
          doubleConversion greedyKnapsack testKnapsackGreedyTakesMaxCorrectly `shouldBe`
          KnapsackResult [Item 150 150] 150
        it "Greedy Knapsack takes max wrong" $
          doubleConversion greedyKnapsack testKnapsackGreedyTakesMaxWrong `shouldBe`
          KnapsackResult [Item 150 150] 150
        it "Greedy Knapsack wrong" $
          doubleConversion greedyKnapsack testKnapsackGreedyWrong `shouldBe`
          KnapsackResult [Item 10 60, Item 20 100] 160
        it "Greedy Knapsack correct" $
          doubleConversion greedyKnapsack testKnapsackGreedyCorrect `shouldBe`
          KnapsackResult [Item 10 60, Item 20 100] 160

-- Test problems
testKnapsackNoItems :: KnapsackProblem Int
testKnapsackNoItems =
  KnapsackProblem {pItems = [], capacity = 50, epsilon = 0.1}

testKnapsackSingle :: KnapsackProblem Int
testKnapsackSingle =
  KnapsackProblem {pItems = [Item 10 60], capacity = 50, epsilon = 0.1}

testKnapsackNoCapacity :: KnapsackProblem Int
testKnapsackNoCapacity =
  KnapsackProblem
    { pItems = [Item 10 60, Item 20 100, Item 30 120]
    , capacity = 0
    , epsilon = 0.1
    }

testKnapsackNull :: KnapsackProblem Int
testKnapsackNull = KnapsackProblem {pItems = [], capacity = 0, epsilon = 0.1}

testKnapsackNoFit :: KnapsackProblem Int
testKnapsackNoFit =
  KnapsackProblem
    { pItems = [Item 10 60, Item 20 100, Item 30 120]
    , capacity = 5
    , epsilon = 0.1
    }

testKnapsackAllFit :: KnapsackProblem Int
testKnapsackAllFit =
  KnapsackProblem
    { pItems = [Item 10 60, Item 20 100, Item 30 120]
    , capacity = 100
    , epsilon = 0.1
    }

testKnapsackGreedyTakesMaxCorrectly :: KnapsackProblem Int
testKnapsackGreedyTakesMaxCorrectly =
  KnapsackProblem
    { pItems = [Item 10 60, Item 10 60, Item 150 150]
    , capacity = 150
    , epsilon = 0.1
    }

testKnapsackGreedyTakesMaxWrong :: KnapsackProblem Int
testKnapsackGreedyTakesMaxWrong =
  KnapsackProblem
    { pItems = [Item 10 60, Item 10 60, Item 150 150]
    , capacity = 160
    , epsilon = 0.1
    }

testKnapsackGreedyWrong :: KnapsackProblem Int
testKnapsackGreedyWrong =
  KnapsackProblem
    { pItems = [Item 10 60, Item 20 100, Item 30 120]
    , capacity = 50
    , epsilon = 0.1
    }

testKnapsackGreedyCorrect :: KnapsackProblem Int
testKnapsackGreedyCorrect =
  KnapsackProblem
    { pItems = [Item 10 60, Item 30 120, Item 20 100]
    , capacity = 30
    , epsilon = 0.1
    }
