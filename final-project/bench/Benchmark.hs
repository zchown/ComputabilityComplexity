{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE BangPatterns #-}

module Main where

import Control.DeepSeq
import Control.Monad (forM_, replicateM)
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

generate3SATProblem :: Int -> Int -> IO (SatProblem 256)
generate3SATProblem n m = do
  clauses <- replicateM m (generate3SATClause n)
  case satProblemFromList @256 clauses of
    Just problem -> return $!! problem
    Nothing -> error "Failed to generate valid SAT problem"

emptyAssignment :: VarAssignment 256
emptyAssignment = createVarAssignment createVarList createVarList

{-# NOINLINE emptyAssignment #-}
timeAction :: String -> SatProblem 256 -> IO POSIXTime
timeAction label problem = do
  start <- getPOSIXTime
  let !result = dpll problem emptyAssignment -- Force evaluation
  end <- getPOSIXTime
  let diff = end - start
  putStrLn $ label ++ ": " ++ show (realToFrac diff * 1000) ++ " ms"
  return diff

main :: IO ()
main = do
  putStrLn "Generating test cases with 256 variables..."
  problems <- mapM (\m -> generate3SATProblem 256 m) clauseSizes
  putStrLn "\nRunning benchmarks..."
  forM_ (zip clauseSizes problems) $ \(numClauses, problem) -> do
    times <-
      replicateM 5 $
      timeAction ("DPLL-256-vars/" ++ show numClauses ++ " clauses") problem
    let avgTime = realToFrac (sum times) * 1000 / 5.0
    putStrLn $
      "Average time for " ++
      show numClauses ++ " clauses: " ++ show avgTime ++ " ms\n"
  where
    clauseSizes = [128, 256, 512, 1024, 1536, 2048]
