{-# LANGUAGE FlexibleInstances #-}

module FuzzKnapsack
  ( runKnapsackFuzzTests
  ) where

import GHC.Float (int2Double)
import Knapsack
import Test.QuickCheck

positiveInt :: Int -> Gen Int
positiveInt x = choose (1, (2 :: Int) ^ x)

knapsackGen :: Gen (KnapsackProblem Int)
knapsackGen = do
  nn <- choose (1, (2 :: Int) ^ (7 :: Int))
  sn <- choose (1, (2 :: Int) ^ (6 :: Int))
  bn <- choose (1, (2 :: Int) ^ (6 :: Int))
  nItems <- vectorOf nn $ Item <$> positiveInt 8 <*> positiveInt 8
  sItems <- vectorOf sn $ Item <$> positiveInt 8 <*> positiveInt 4
  bItems <- vectorOf bn $ Item <$> positiveInt 4 <*> positiveInt 8
  items <- shuffle (nItems ++ sItems ++ bItems)
  cap <- choose (0, (2 :: Int) ^ (15 :: Int))
  e <- choose (0.1, 1)
  return $ KnapsackProblem items cap e

instance Arbitrary (KnapsackProblem Int) where
  arbitrary = knapsackGen

prop_respectCapacity ::
     (KnapsackProblem Int -> KnapsackResult Int) -> KnapsackProblem Int -> Bool
prop_respectCapacity f p@(KnapsackProblem _ cap _) =
  sum (map itemWeight is) <= cap
  where
    KnapsackResult is _ = f p

prop_inAgreement :: KnapsackProblem Int -> Property
prop_inAgreement p@(KnapsackProblem _ _ e) =
  let greedy =
        convertKnapsackResultDoubleInt $
        greedyKnapsack (convertKnapsackProblemIntDouble p)
      zeroOne = knapsack01 p
      minCost = minCostKnapsack p
      fptas = fptasKnapsack p
      zeroOneValue = knapsackValue zeroOne
      greedyValue = knapsackValue greedy
      minCostValue = knapsackValue minCost
      fptasValue = knapsackValue fptas
      condition1 = zeroOneValue == minCostValue
      condition2 = zeroOneValue <= 2 * greedyValue
      condition3 =
        (int2Double fptasValue >= (1.0 - e) * int2Double zeroOneValue) &&
        (int2Double fptasValue <= int2Double zeroOneValue)
      condition4 = fptasValue <= zeroOneValue
      condition5 = greedyValue <= zeroOneValue
   in counterexample
        ("Values: 0-1 = " ++
         show zeroOneValue ++
         ", Greedy = " ++
         show greedyValue ++
         ", Min Cost = " ++
         show minCostValue ++
         ", FPTAS = " ++ show fptasValue ++ "\nEpsilon: " ++ show e) $
      condition1 .&&. condition2 .&&. condition3 .&&. condition4 .&&. condition5

runKnapsackFuzzTests :: IO ()
runKnapsackFuzzTests = do
  putStrLn "Running Knapsack Fuzz Tests"
  putStrLn "---------------------------"
  putStrLn "Testing Greedy Knapsack respectCapacity"
  quickCheck
    (prop_respectCapacity
       (convertKnapsackResultDoubleInt .
        greedyKnapsack . convertKnapsackProblemIntDouble))
  putStrLn "Testing 01 Knapsack respectCapacity"
  quickCheck (prop_respectCapacity knapsack01)
  putStrLn "Testing Min Cost Knapsack respectCapacity"
  quickCheck (prop_respectCapacity minCostKnapsack)
  putStrLn "Testing FPTAS Knapsack respectCapacity"
  quickCheck (prop_respectCapacity fptasKnapsack)
  putStrLn "---------------------------"
  putStrLn "Testing agreement between algorithms"
  quickCheck prop_inAgreement
