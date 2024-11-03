module Main
  ( main
  ) where

import TestSatSolverHelpers

import TestSatSolvers

-- import FuzzKnapsack
-- import TestKnapsack
import TestSatTypes

main :: IO ()
main = putStrLn "Running Tests..." >> runAllTests

runAllTests :: IO ()
runAllTests = do
  runTestSatType
  -- runTestSatHelper
  runTestSatSolver
  -- runKnapsackTests
  -- runKnapsackFuzzTests
