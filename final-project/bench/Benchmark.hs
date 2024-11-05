{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE BangPatterns #-}

module Main where

import Control.DeepSeq
import Control.Monad (forM_, replicateM)
import Data.List (transpose)
import Data.Time.Clock.POSIX (POSIXTime, getPOSIXTime)
import qualified Data.Vector.Unboxed as V
import SatSolvers
import SatTypes
import System.Random

instance NFData (VarList n) where
  rnf !v@(VarList vec) = v `seq` rnf (V.toList vec)

instance NFData (Clause n) where
  rnf !c@(Clause p n) = c `seq` rnf p `seq` rnf n

instance NFData (SatProblem n) where
  rnf !p@(SatProblem cs) = p `seq` rnf cs

instance NFData (VarAssignment n) where
  rnf !a@(VarAssignment (!p, !n)) = a `seq` rnf p `seq` rnf n

instance NFData (SatSolution n) where
  rnf !s@(Satisfiable v) = s `seq` rnf v
  rnf Unsatisfiable = ()

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

generateSATProblems :: Int -> Int -> IO [SatProblem 32]
generateSATProblems n m = do
  clauses <- replicateM m (generate3SATClause n)
  case satProblemFromList @32 clauses of
    Just problem -> return [problem]
    Nothing -> return []

timeAction :: String -> SatProblem 32 -> IO POSIXTime
timeAction label problem = do
  start <- getPOSIXTime
  let !result = dpll problem emptyAssignment
  end <- getPOSIXTime
  let diff = end - start
  putStrLn $ label ++ ": " ++ show (realToFrac diff * 1000) ++ " ms"
  return diff

emptyAssignment :: VarAssignment 32
emptyAssignment = createVarAssignment createVarList createVarList

main :: IO ()
main = do
  putStrLn "Generating test cases with 32 variables..."
  let clauseCounts = [16,32 .. 256]
  let numProblems = 5
  problems <-
    sequence $
    replicate numProblems $
    concat <$> mapM (\c -> generateSATProblems 32 c) clauseCounts
  let zippedProblems = zip clauseCounts $ transpose $ problems
  putStrLn "\nRunning benchmarks..."
  forM_ zippedProblems $ \(numClauses, problemSet) -> do
    putStrLn $ "Benchmarking with " ++ show numClauses ++ " clauses..."
    times <-
      mapM
        (timeAction ("DPLL-32-vars/" ++ show numClauses ++ " clauses"))
        problemSet
    let avgTime = realToFrac (sum times) * 1000 / fromIntegral numProblems
    putStrLn $
      "Average time for " ++
      show numClauses ++ " clauses: " ++ show avgTime ++ " ms\n"
