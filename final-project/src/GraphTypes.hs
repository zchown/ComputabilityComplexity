{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}

module GraphTypes where

import qualified Data.Vector as V
import qualified TspTypes as T

newtype NodeId =
  NodeId Int
  deriving (Show, Eq, Ord)

newtype EdgeWeight =
  EdgeWeight Double
  deriving (Show, Eq, Ord, Num)

newtype Edge =
  Edge (NodeId, NodeId, EdgeWeight)
  deriving (Show, Eq)

data BasicGraph = BasicGraph
  { bgNodes :: [NodeId]
  , bgEdges :: [Edge]
  } deriving (Show, Eq)

data Node = Node
  { nNodeId :: NodeId
  , nEdges :: [(NodeId, EdgeWeight)]
  } deriving (Show, Eq)

newtype NodeGraph = NodeGraph
  { ngNodes :: [Node]
  } deriving (Show, Eq)

newtype AdjacencyMatrix =
  AdjacencyMatrix (V.Vector (V.Vector (Maybe EdgeWeight)))

------------------------------
-- | Conversion functions | --
------------------------------
basicToNode :: BasicGraph -> NodeGraph
basicToNode (BasicGraph ns es) = NodeGraph $ map mkNode ns
  where
    mkNode n = Node n (map (\(Edge (_, to, w)) -> (to, w)) $ connecting n)
    connecting n = filter (\(Edge (from, _, _)) -> from == n) es

nodeToBasic :: NodeGraph -> BasicGraph
nodeToBasic (NodeGraph ns) = BasicGraph (map nNodeId ns) (concatMap mkEdges ns)
  where
    mkEdges (Node n es) = map (\(to, w) -> Edge (n, to, w)) es

nodeToMatrix :: NodeGraph -> AdjacencyMatrix
nodeToMatrix (NodeGraph ns) = AdjacencyMatrix $ V.fromList $ map mkRow ns
  where
    mkRow (Node _ es) = V.fromList $ map (\(_, w) -> Just w) es

basicToMatrix :: BasicGraph -> AdjacencyMatrix
basicToMatrix = nodeToMatrix . basicToNode

matrixToBasic :: AdjacencyMatrix -> BasicGraph
matrixToBasic = nodeToBasic . matrixToNode

matrixToNode :: AdjacencyMatrix -> NodeGraph
matrixToNode (AdjacencyMatrix m) =
  NodeGraph $ zipWith mkNode [0 ..] (V.toList m)
  where
    mkNode i row = Node (NodeId i) (zipWith mkEdge [0 ..] (V.toList row))
    mkEdge j (Just w) = (NodeId j, w)
    mkEdge _ Nothing = error "matrixToNode: invalid edge"
-----------------------------
-- | TSP-Lib Conversions | --
-----------------------------
-- tspProblemToBasic :: T.TspProblem -> BasicGraph
-- tspProblemToBasic (T.TspProblem _ _ _ dims ewt ewf edf nct tdata)
