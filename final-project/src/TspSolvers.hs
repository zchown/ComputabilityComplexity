module TspSolvers where

import GraphTypes
import Kruskal

tspSolver :: BasicGraph -> BasicGraph
tspSolver = backtrack . kruskal
  where
    backtrack (BasicGraph nodes edges) =
      BasicGraph nodes $ concatMap backtrackEdge edges
