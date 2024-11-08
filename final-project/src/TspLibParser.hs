module TsplibParser where

import Control.Monad (void)
import Data.List.Split (splitOn)
import qualified Data.Map as Map
import DistanceFunctions
import Text.Read (readMaybe)
import TspTypes

parseTspFile :: String -> IO (Either String TspProblem)
parseTspFile filename = do
  contents <- readFile filename
  let linesOfFile = lines contents
  let (headerLines, bodyLines) = break (== "NODE_COORD_SECTION") linesOfFile
  let header = parseHeader headerLines
  let body = parseBody bodyLines
  return $ do
    h <- header
    b <- body
    return $ tspProblemFromHeaderBody h b
