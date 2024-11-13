{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}

module Main
  ( main
  ) where

import Control.Monad (when)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import FuzzKnapsack
import System.Directory (doesFileExist)
import System.Environment (lookupEnv, setEnv)
import TestKnapsack
import TestSatSolverHelpers
import TestSatSolvers
import TestSatTypes
import TestTspParser

loadEnvFile :: IO [(String, String)]
loadEnvFile = do
  exists <- doesFileExist ".env"
  if exists
    then do
      content <- TIO.readFile ".env"
      return $ parseEnvFile $ T.unpack content
    else return []
  where
    parseEnvFile :: String -> [(String, String)]
    parseEnvFile = map parseLine . filter (not . null) . lines
    parseLine line =
      case break (== '=') line of
        (key, '=':value) ->
          ( (T.unpack . T.strip . T.pack) key
          , (T.unpack . T.strip . T.pack) value)
        _ -> ("", "")

setEnvVars :: [(String, String)] -> IO ()
setEnvVars = mapM_ (\(key, value) -> setEnv key value)

data TestConfig = TestConfig
  { runTsp :: Bool
  , runSatType :: Bool
  , runSatHelper :: Bool
  , runSatSolver :: Bool
  , runKnapsack :: Bool
  , runFuzzKnapsack :: Bool
  }

getTestConfig :: IO TestConfig
getTestConfig = do
  tsp <- lookupEnvBool "RUN_TSP_TESTS" True
  satType <- lookupEnvBool "RUN_SAT_TYPE_TESTS" True
  satHelper <- lookupEnvBool "RUN_SAT_HELPER_TESTS" True
  satSolver <- lookupEnvBool "RUN_SAT_SOLVER_TESTS" True
  knapsack <- lookupEnvBool "RUN_KNAPSACK_TESTS" True
  fuzzKnapsack <- lookupEnvBool "RUN_FUZZ_KNAPSACK_TESTS" True
  return
    TestConfig
      { runTsp = tsp
      , runSatType = satType
      , runSatHelper = satHelper
      , runSatSolver = satSolver
      , runKnapsack = knapsack
      , runFuzzKnapsack = fuzzKnapsack
      }
  where
    lookupEnvBool :: String -> Bool -> IO Bool
    lookupEnvBool name defaultValue = do
      mVal <- lookupEnv name
      return $
        case mVal of
          Nothing -> defaultValue
          Just val -> T.toLower (T.pack val) `elem` ["true", "1", "yes"]

runAllTests :: TestConfig -> IO ()
runAllTests config = do
  when (runTsp config) $
    putStrLn "Running TSP Parser Tests..." >> runTspParserTest
  when (runSatType config) $
    putStrLn "Running SAT Type Tests..." >> runTestSatType
  when (runSatHelper config) $
    putStrLn "Running SAT Helper Tests..." >> runTestSatHelper
  when (runSatSolver config) $
    putStrLn "Running SAT Solver Tests..." >> runTestSatSolver
  when (runKnapsack config) $
    putStrLn "Running Knapsack Tests..." >> runKnapsackTests
  when (runFuzzKnapsack config) $
    putStrLn "Running Knapsack Fuzz Tests..." >> runKnapsackFuzzTests

main :: IO ()
main = do
  putStrLn "Loading environment variables..."
  envVars <- loadEnvFile
  setEnvVars envVars
  putStrLn "Configuring tests..."
  config <- getTestConfig
  putStrLn "Running enabled tests..."
  runAllTests config
