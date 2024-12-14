{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE FlexibleContexts #-}

module SatBenchmarks where

import Control.DeepSeq
import Control.Monad (forM_, replicateM)
import Data.List (transpose)
import Data.Proxy (Proxy(..))
import Data.Time.Clock (getCurrentTime)
import Data.Time.Clock.POSIX (POSIXTime, getPOSIXTime)
import Data.Time.Format (defaultTimeLocale, formatTime)
import qualified Data.Vector.Unboxed as V
import GHC.TypeLits (KnownNat, natVal)
import SatLinearProgramming
import SatSolvers
import SatTypes
import System.IO (Handle, IOMode(..), hPutStrLn, withFile)
import qualified Data.Bits as B
import System.Random

tee :: Handle -> String -> IO ()
tee handle str = do
  putStrLn str
  hPutStrLn handle str

generate3SATClause :: Int -> IO [Int]
generate3SATClause n = do
  vars <- V.replicateM 3 (randomRIO (1, n))
  signs <- V.replicateM 3 (randomRIO (0, 1 :: Int))
  return $!
    V.toList $
    V.zipWith
      (\v s ->
         if s == 0
           then v
           else -v)
      vars
      signs

generateSATProblems ::
     forall n. KnownNat n
  => Proxy n
  -> Int
  -> Int
  -> IO [SatProblem n]
generateSATProblems _ n m = do
  clauses <- replicateM m (generate3SATClause n)
  case satProblemFromList @n clauses of
    Just problem -> return [problem]
    Nothing -> return []

timeActionDpll ::
     forall n. KnownNat n
  => Handle
  -> String
  -> SatProblem n
  -> IO (POSIXTime, SatSolution n)
timeActionDpll handle label problem = do
  start <- getPOSIXTime
  let !result = dpll problem
  end <- getPOSIXTime
  let diff = end - start
  tee handle $
    label ++
    ": " ++
    show ((realToFrac diff * 1000) :: Double) ++
    " ms" 
  return (diff, result)

timeActionGsat ::
     forall n. KnownNat n
  => Handle
  -> String
  -> Int
  -> Int
  -> SatProblem n
  -> IO (POSIXTime, SatSolution n)
timeActionGsat handle label maxTries maxFlips problem = do
  start <- getPOSIXTime
  let !result = gsat problem maxTries maxFlips
  end <- getPOSIXTime
  let diff = end - start
  tee handle $
    label ++
    ": " ++
    show ((realToFrac diff * 1000) :: Double) ++
    " ms"
  return (diff, Satisfiable (assignedPositive result))

timeActionRandomizedRounding ::
     forall n. KnownNat n
  => Handle
  -> String
  -> SatProblem n
  -> IO (POSIXTime, SatSolution n)
timeActionRandomizedRounding handle label problem = do
  start <- getPOSIXTime
  result <- randomizedRounding problem
  end <- getPOSIXTime
  let diff = end - start
  tee handle $
    label ++
    ": " ++
    show ((realToFrac diff * 1000) :: Double) ++
    " ms" 
  return (diff, result)

calculateClauseSatisfaction :: 
     forall n. KnownNat n
  => SatProblem n
  -> SatSolution n
  -> Double
calculateClauseSatisfaction p@(SatProblem cs) sol=
  case sol of
    Unsatisfiable -> 0.0
    Satisfiable assignment -> evaluateSatProblem p $ VarAssignment assignment (B.complement assignment)

benchmarkWithVars ::
     forall n. KnownNat n
  => Handle
  -> Proxy n
  -> IO ()
benchmarkWithVars handle proxy = do
  let numVars = fromIntegral $ natVal proxy
  tee handle $ "Generating test cases with " ++ show numVars ++ " variables..."
  let clauseCounts = [16,32 .. 256]
  let numProblems = 5
  let maxTries = 100
  let maxFlips = 100
  problems <-
    replicateM numProblems $
    concat <$> mapM (generateSATProblems proxy numVars) clauseCounts
  let zippedProblems = zip clauseCounts $ transpose problems
  tee handle "\nRunning benchmarks..."
  forM_ zippedProblems $ \(numClauses, problemSet) -> do
    tee handle $ "Benchmarking with " ++ show numClauses ++ " clauses..."
    dpllResults <-
      mapM
        (timeActionDpll
           handle
           ("DPLL-" ++ show numVars ++ "-vars/" ++ show numClauses ++ " clauses"))
        problemSet
    gsatResults <-
      mapM
        (timeActionGsat
           handle
           ("GSAT-" ++ show numVars ++ "-vars/" ++ show numClauses ++ " clauses")
           maxTries
           maxFlips)
        problemSet
    randomizedRoundingResults <-
      mapM
        (timeActionRandomizedRounding
           handle
           ("Randomized-Rounding-" ++ show numVars ++ "-vars/" ++ show numClauses ++ " clauses"))
        problemSet
    let computeStats results problems' = 
          let avgTime = realToFrac (sum (map fst results)) * 1000 / fromIntegral numProblems
              satisfactionPercentages = 
                zipWith calculateClauseSatisfaction problems' (map snd results)
              avgSatisfaction = 
                sum satisfactionPercentages / fromIntegral numProblems
           in (avgTime, avgSatisfaction, satisfactionPercentages)
    let (dpllAvgTime, dpllAvgSatisfaction, dpllSatisfactionPercentages) = 
          computeStats dpllResults problemSet
    let (gsatAvgTime, gsatAvgSatisfaction, gsatSatisfactionPercentages) = 
          computeStats gsatResults problemSet
    let (rndAvgTime, rndAvgSatisfaction, rndSatisfactionPercentages) = 
          computeStats randomizedRoundingResults problemSet
    tee handle $ "Average time for " ++ show numClauses ++ " clauses:"
    tee handle $ "  DPLL:               " ++ show (dpllAvgTime :: Double) ++ " ms"
    tee handle $ "  GSAT:               " ++ show (gsatAvgTime :: Double) ++ " ms"
    tee handle $ "  Randomized Rounding:" ++ show (rndAvgTime :: Double) ++ " ms"
    tee handle $ "Average clause satisfaction:"
    tee handle $ "  DPLL:               " ++ show (dpllAvgSatisfaction :: Double) ++ "%"
    tee handle $ "  GSAT:               " ++ show (gsatAvgSatisfaction :: Double) ++ "%"
    tee handle $ "  Randomized Rounding:" ++ show (rndAvgSatisfaction :: Double) ++ "%"
    tee handle "\nIndividual Problem Clause Satisfactions:"
    tee handle $ "  DPLL:               " ++ show dpllSatisfactionPercentages
    tee handle $ "  GSAT:               " ++ show gsatSatisfactionPercentages
    tee handle $ "  Randomized Rounding:" ++ show rndSatisfactionPercentages
    tee handle "\n"

runSatBenchmarks :: IO ()
runSatBenchmarks = do
  currentTime <- getCurrentTime
  let timestamp = formatTime defaultTimeLocale "%Y%m%d_%H%M%S" currentTime
  let filename = "benchmark_results/benchmark_results_" ++ timestamp ++ ".txt"
  withFile filename WriteMode $ \handle -> do
    tee handle $
      "SAT Solver Benchmark Results - " ++
      formatTime defaultTimeLocale "%Y-%m-%d %H:%M:%S" currentTime
    tee handle "================================================"
    tee handle ""
    benchmarkWithVars handle (Proxy @32)
