{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE BangPatterns #-}

module KnapsackBenchmarks where

import Control.DeepSeq
import Control.Monad (forM, forM_, replicateM)
import qualified Data.Array as A
import Data.Array.ST
import Data.Array.Unboxed
import Data.List (sort, transpose)
import Data.Time.Clock.POSIX (POSIXTime, getPOSIXTime)
import Knapsack
import System.IO (Handle, IOMode(..), hPutStrLn, withFile)
import System.Random
import System.Random.Shuffle (shuffleM)

instance NFData (Item Int) where
  rnf i@(Item w v) = i `seq` rnf w `seq` rnf v

instance NFData (KnapsackProblem Int) where
  rnf p@(KnapsackProblem ps c e) = p `seq` rnf ps `seq` rnf c `seq` rnf e

instance NFData (KnapsackResult Int) where
  rnf r@(KnapsackResult is v) = r `seq` rnf is `seq` rnf v

generateSmallItem :: IO (Item Int)
generateSmallItem = do
  w <- randomRIO (1, 32)
  v <- randomRIO (1, 32)
  return $! Item w v

generateMediumItem :: IO (Item Int)
generateMediumItem = do
  w <- randomRIO (1, 128)
  v <- randomRIO (1, 128)
  return $! Item w v

generateLargeItem :: IO (Item Int)
generateLargeItem = do
  w <- randomRIO (1, 512)
  v <- randomRIO (1, 512)
  return $! Item w v

generateHighValueItem :: IO (Item Int)
generateHighValueItem = do
  w <- randomRIO (1, 64)
  v <- randomRIO (256, 512)
  return $! Item w v

generateHighWeightItem :: IO (Item Int)
generateHighWeightItem = do
  w <- randomRIO (256, 512)
  v <- randomRIO (1, 64)
  return $! Item w v

generateKnapsackProblem :: IO (KnapsackProblem Int)
generateKnapsackProblem = do
  smallItems <- replicateM 256 generateSmallItem
  mediumItems <- replicateM 256 generateMediumItem
  largeItems <- replicateM 128 generateLargeItem
  highValueItems <- replicateM 128 generateHighValueItem
  highWeightItems <- replicateM 128 generateHighWeightItem
  items <-
    shuffleM $
    concat
      [smallItems, mediumItems, largeItems, highValueItems, highWeightItems]
  c <- randomRIO (1, 2 ^ 10)
  e <- randomRIO (0.1, 0.9)
  return $! KnapsackProblem items c e

tee :: Handle -> String -> IO ()
tee handle str = do
  putStrLn str
  hPutStrLn handle str

benchmarkIterations :: Int
benchmarkIterations = 10

data BenchmarkResult = BenchmarkResult
  { algorithm :: String
  , minTime :: Double
  , maxTime :: Double
  , avgTime :: Double
  , results :: [KnapsackResult Int]
  } deriving (Show)

runBenchmark ::
     (KnapsackProblem Int -> KnapsackResult Int)
  -> String
  -> [KnapsackProblem Int]
  -> IO BenchmarkResult
runBenchmark algorithm algorithmName problems = do
  results <-
    forM problems $ \problem -> do
      start <- getPOSIXTime
      let !result = algorithm problem
      end <- getPOSIXTime
      let timeTaken = realToFrac (end - start) * 1000 :: Double
      return (timeTaken, result)
  let (times, algorithmResults) = unzip results
      sortedTimes = sort times
  return
    BenchmarkResult
      { algorithm = algorithmName
      , minTime = head sortedTimes
      , maxTime = last sortedTimes
      , avgTime = sum times / fromIntegral benchmarkIterations
      , results = algorithmResults
      }

runBenchmarkDouble ::
     (KnapsackProblem Double -> KnapsackResult Double)
  -> String
  -> [KnapsackProblem Int]
  -> IO BenchmarkResult
runBenchmarkDouble algorithm algorithmName problems = do
  let doubleProblems = map convertKnapsackProblemIntDouble problems
  results <-
    forM doubleProblems $ \problem -> do
      start <- getPOSIXTime
      let !result = convertKnapsackResultDoubleInt $ algorithm problem
      end <- getPOSIXTime
      let timeTaken = realToFrac (end - start) * 1000 :: Double
      return (timeTaken, result)
  let (times, algorithmResults) = unzip results
      sortedTimes = sort times
  return
    BenchmarkResult
      { algorithm = algorithmName
      , minTime = head sortedTimes
      , maxTime = last sortedTimes
      , avgTime = sum times / fromIntegral benchmarkIterations
      , results = algorithmResults
      }

runKnapsackBenchmarks' :: IO [BenchmarkResult]
runKnapsackBenchmarks' = do
  problems <- replicateM benchmarkIterations generateKnapsackProblem
  greedyResult <-
    runBenchmarkDouble greedyKnapsack "Greedy Knapsack (Double)" problems
  zeroOneResult <- runBenchmark knapsack01 "0/1 Dynamic Programming" problems
  minCostResult <- runBenchmark minCostKnapsack "Minimum Cost Knapsack" problems
  fptasResult <- runBenchmark fptasKnapsack "FPTAS Knapsack" problems
  return [greedyResult, zeroOneResult, minCostResult, fptasResult]

outputBenchmarkResults :: [BenchmarkResult] -> IO ()
outputBenchmarkResults results =
  withFile "benchmark_results/knapsack_benchmark_results.txt" WriteMode $ \handle -> do
    hPutStrLn handle "Knapsack Algorithm Benchmarks"
    hPutStrLn handle "============================="
    forM_ results $ \result -> do
      hPutStrLn handle $ "Algorithm: " ++ algorithm result
      hPutStrLn handle $ "  Minimum Time: " ++ show (minTime result) ++ " ms"
      hPutStrLn handle $ "  Maximum Time: " ++ show (maxTime result) ++ " ms"
      hPutStrLn handle $ "  Average Time: " ++ show (avgTime result) ++ " ms"
      hPutStrLn handle ""

runKnapsackBenchmarks :: IO ()
runKnapsackBenchmarks = do
  results <- runKnapsackBenchmarks'
  outputBenchmarkResults results
  putStrLn
    "Benchmark complete. Results written to knapsack_benchmark_results.txt"
