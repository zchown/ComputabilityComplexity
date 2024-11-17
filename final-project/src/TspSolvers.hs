module TspSolvers where

import Data.List (foldl')
import Data.Maybe (fromJust, isNothing)
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
-- nearestNeighbor :: BasicGraph -> TspSolution
-- nearestNeighbor g@(ns, es) = TspSolution cost path
--   where
--     path = go ns es
--     cost =  sum . map (\(Edge (_, _, w)) -> unEdgeWeight w) $ path
--     go :: [NodeId] -> [Edge] -> [Edge]
--     go [] _ = []
--     go (x:xs) edges = tour
--       where
--         nn =
--           foldl'
--             (\acc e@(Edge (y, z, w)) ->
--                if (x == y || x == z) &&
--                   (isNothing acc || unEdgeWeight w < unEdgeWeight (fromJust acc))
--                  then Just e
--                  else acc)
--             Nothing
--             edges
--         tour =
--           case nn of
--             Nothing -> []
--             Just e@(Edge (y, z, _)) ->
--               e :
--               go
--                 (if x == y
--                    then z : xs
--                    else y : xs)
--                 (filter (/= e) edges)
--     unEdgeWeight (EdgeWeight w) = w
