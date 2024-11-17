{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE FlexibleContexts #-}

module Main where

import Control.DeepSeq
import Control.Monad (forM_, replicateM)
import Data.List (transpose)
import Data.Proxy (Proxy(..))
import Data.Time.Clock (getCurrentTime)
import Data.Time.Clock.POSIX (POSIXTime, getPOSIXTime)
import Data.Time.Format (defaultTimeLocale, formatTime)
import qualified Data.Vector.Unboxed as V
import GHC.TypeLits (KnownNat, natVal)
import SatSolvers
import SatTypes
import System.IO (Handle, IOMode(..), hPutStrLn, withFile)
import System.Random

instance NFData (VarList n) where
  rnf v@(VarList !vec) = v `seq` rnf (V.toList vec)

instance NFData (Clause n) where
  rnf c@(Clause !p !n) = c `seq` rnf p `seq` rnf n

instance NFData (SatProblem n) where
  rnf p@(SatProblem !cs) = p `seq` rnf cs

instance NFData (VarAssignment n) where
  rnf a@(VarAssignment !p !n) = a `seq` rnf p `seq` rnf n

instance NFData (SatSolution n) where
  rnf s@(Satisfiable !v) = s `seq` rnf v
  rnf Unsatisfiable = ()

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

timeAction ::
     forall n. KnownNat n
  => Handle
  -> String
  -> SatProblem n
  -> IO (POSIXTime, SatSolution n)
timeAction handle label problem = do
  start <- getPOSIXTime
  let !result = dpll problem
  end <- getPOSIXTime
  let diff = end - start
  tee handle $
    label ++
    ": " ++
    show ((realToFrac diff * 1000) :: Double) ++
    " ms" ++ " - Result: " ++ show result
  return (diff, result)

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
  problems <-
    replicateM numProblems $
    concat <$> mapM (generateSATProblems proxy numVars) clauseCounts
  let zippedProblems = zip clauseCounts $ transpose problems
  tee handle "\nRunning benchmarks..."
  forM_ zippedProblems $ \(numClauses, problemSet) -> do
    tee handle $ "Benchmarking with " ++ show numClauses ++ " clauses..."
    timesAndResults <-
      mapM
        (timeAction
           handle
           ("DPLL-" ++ show numVars ++ "-vars/" ++ show numClauses ++ " clauses"))
        problemSet
    let times = map fst timesAndResults
    let satisfiableCount =
          length [r | (_, r) <- timesAndResults, isSatisfiable r]
    let avgTime = realToFrac (sum times) * 1000 / fromIntegral numProblems
    tee handle $
      "Average time for " ++
      show numClauses ++ " clauses: " ++ show (avgTime :: Double) ++ " ms"
    tee handle $
      "Satisfiable problems: " ++
      show satisfiableCount ++ " out of " ++ show numProblems ++ "\n"
  where
    isSatisfiable (Satisfiable _) = True
    isSatisfiable Unsatisfiable = False

main :: IO ()
main = do
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
