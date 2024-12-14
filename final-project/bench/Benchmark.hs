{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Main where

import Control.Monad (when)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import KnapsackBenchmarks
import SatBenchmarks
import System.Directory (doesFileExist)
import System.Environment (lookupEnv, setEnv)
import TspBenchmarks

loadEnvFile :: IO [(String, String)]
loadEnvFile = do
  exists <- doesFileExist ".benv"
  if exists
    then do
      content <- TIO.readFile ".benv"
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

data BenchConfig = BenchConfig
  { runTsp :: Bool
  , runSat :: Bool
  , runKnap :: Bool
  }

getBenchConfig :: IO BenchConfig
getBenchConfig = do
  tsp <- lookupEnvBool "RUN_TSP_BENCHMARKS" True
  sat <- lookupEnvBool "RUN_SAT_BENCHMARKS" True
  knap <- lookupEnvBool "RUN_KNAP_BENCHMARKS" True
  return BenchConfig {runTsp = tsp, runSat = sat, runKnap = knap}
  where
    lookupEnvBool :: String -> Bool -> IO Bool
    lookupEnvBool key def = do
      mVal <- lookupEnv key
      return $
        case mVal of
          Nothing -> def
          Just val -> T.toLower (T.pack val) `elem` ["true", "1", "yes"]

runAllBenchmarks :: BenchConfig -> IO ()
runAllBenchmarks BenchConfig {..} = do
  when runTsp $ putStrLn "Running TSP benchmarks..." >> runTspBenchmarks
  when runSat $ putStrLn "Running SAT benchmarks..." >> runSatBenchmarks
  when runKnap $
    putStrLn "Running Knapsack benchmarks..." >> runKnapsackBenchmarks

main :: IO ()
main = do
  putStrLn "Loading environment variables..."
  envVars <- loadEnvFile
  setEnvVars envVars
  putStrLn "Configuring benchmarks..."
  config <- getBenchConfig
  putStrLn "Running benchmarks..."
  runAllBenchmarks config
