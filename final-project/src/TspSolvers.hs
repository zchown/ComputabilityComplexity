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
    path = concat $ scanl (flip (:)) [last mstEdges] (init mstEdges)
    (EdgeWeight cost) = sum . map (\(Edge (_, _, w)) -> w) $ path
