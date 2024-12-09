module TspBenchmarks where

import GraphTypes
import System.Directory (doesFileExist, listDirectory)
import System.FilePath ((</>))
import TspLibParser
import TspSolvers
import TspTypes

runTspBenchmarks :: IO ()
runTspBenchmarks = do
  putStrLn "Running TSP benchmarks..."
  let problems =
        [ "burma14.tsp.gz"
        , "ali535.tsp.gz"
        , "berlin52.tsp.gz"
        , "bier127.tsp.gz"
        , "ch130.tsp.gz"
        , "ch150.tsp.gz"
        , "d198.tsp.gz"
        , "d493.tsp.gz"
        , "d657.tsp.gz"
        , "eil51.tsp.gz"
        ]
  let dp = map ("tsp_problems/" ++) problems
  mapM_ runTsp dp

runTsp :: String -> IO ()
runTsp file = do
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
              putStrLn $ file ++ ": " ++ (show . tspCost) sol
    else putStrLn $ "FAILED\n    Error: File not found: " ++ file
