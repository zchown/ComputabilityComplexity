module Main
  ( main
  ) where

-- import TestSatSolverHelpers
-- import TestSatSolvers
-- import FuzzKnapsack
-- import TestKnapsack
-- import TestSatTypes
import TspParserTest

main :: IO ()
main = putStrLn "Running Tests..." >> runAllTests

runAllTests :: IO ()
runAllTests = do
  runTspParserTest
  -- runTestSatType
  -- runTestSatHelper
  -- runTestSatSolver
  -- runKnapsackTests
  -- runKnapsackFuzzTests
