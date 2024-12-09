module TspBenchmarks where

import GraphTypes
import System.Directory (doesFileExist)
import TspLibParser
import TspSolvers

runTspBenchmarks :: IO ()
runTspBenchmarks = do
  putStrLn "Running TSP benchmarks..."
  let problems =
        [ ("tsp_problems/burma14.tsp.gz", 3323)
        , ("tsp_problems/ali535.tsp.gz", 202310)
        , ("tsp_problems/berlin52.tsp.gz", 7542)
        , ("tsp_problems/bier127.tsp.gz", 118282)
        , ("tsp_problems/ch130.tsp.gz", 6110)
        , ("tsp_problems/ch150.tsp.gz", 6528)
        , ("tsp_problems/d198.tsp.gz", 15780)
        , ("tsp_problems/d493.tsp.gz", 35002)
        , ("tsp_problems/d657.tsp.gz", 48912)
        , ("tsp_problems/eil51.tsp.gz", 426)
        ]
  mapM_ runTsp problems

runTsp :: (String, Double) -> IO ()
runTsp (file, csol) = do
  exists <- doesFileExist file
  if exists
    then do
      result <- parseTspFile file
      case result of
        Left err -> putStrLn $ "FAILED\n    Error: " ++ show err
        Right p -> do
          let basic = tspProblemToBasic p
          case basic of
            Right err -> putStrLn $ "FAILED\n    Error: " ++ show err
            Left b -> do
              let sol = tspSolver b
              putStrLn $
                file ++
                ": " ++
                (show . tspCost) sol ++
                " - " ++ show csol ++ " - " ++ show (tspCost sol / csol)
    else putStrLn $ "FAILED\n    Error: File not found: " ++ file
