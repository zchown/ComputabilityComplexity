module TspSolvers where

import GraphTypes
import Kruskal

data TspSolution = TspSolution
  { tspCost :: Double
  , tspPath :: [Edge]
  } deriving (Show, Eq)

tspSolver :: BasicGraph -> TspSolution
tspSolver g = TspSolution cost path
  where
    (BasicGraph _ mstEdges) = kruskal g
    path = findAndBacktrack mstEdges
    (EdgeWeight cost) = sum . map (\(Edge (_, _, w)) -> w) $ path

findAndBacktrack :: [Edge] -> [Edge]
findAndBacktrack [] = []
findAndBacktrack (e:es) = (e : go) ++ [e]
  where
    es' = filter (/= e) es
    go = findAndBacktrack es'
