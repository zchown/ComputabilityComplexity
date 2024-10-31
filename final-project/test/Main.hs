module Main
  ( main
  ) where

import TestSatSolverHelpers

import FuzzKnapsack
import TestKnapsack
import TestSatTypes

main :: IO ()
main = putStrLn "Running Tests..." >> runAllTests

runAllTests :: IO ()
runAllTests = do
  runTestSatType
  runTestSatHelper
  runKnapsackTests
  runKnapsackFuzzTests
