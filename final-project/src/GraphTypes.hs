{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}

module GraphTypes where

import qualified Data.Vector as V
import DistanceFunctions
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

newtype TspError =
  TspError String
  deriving (Show, Eq)

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
tspProblemToBasic :: T.TspProblem -> Either BasicGraph TspError
tspProblemToBasic p@(T.TspProblem _ _ _ dims ewt ewf edf nct tdata) =
  case ewt of
    Just T.GEO -> geoToBasic p
    Just T.EUC_2D -> euc2dToBasic p
    Nothing -> Right $ TspError "tspProblemToBasic: no edge weight type"

geoToBasic :: T.TspProblem -> Either BasicGraph TspError
geoToBasic (T.TspProblem _ _ _ _ _ _ _ _ (T.EdgeWeightData _)) =
  Right $ TspError "geoToBasic: no edge weight data"
geoToBasic (T.TspProblem _ _ _ _ _ _ _ _ (T.FixedEdgesData _)) =
  Right $ TspError "geoToBasic: fixed edges not supported"
geoToBasic (T.TspProblem _ _ _ _ _ _ _ _ (T.CombinedData {})) =
  Right $ TspError "geoToBasic: combined data not supported"
geoToBasic (T.TspProblem _ _ _ dims ewt ewf edf nct tdata) =
  Left $ BasicGraph nodes edges
  where
    nodes = [NodeId i | i <- [1 .. dims]]
    edges =
      [ Edge (NodeId i, NodeId j, edgeWeightFromGeo tdata i j)
      | i <- [1 .. dims]
      , j <- [1 .. dims]
      , i /= j
      ]

edgeWeightFromGeo :: T.TspData -> Int -> Int -> EdgeWeight
edgeWeightFromGeo tdata i j =
  EdgeWeight $ fromIntegral $ geoDistance (geo i) (geo j)
  where
    geo n =
      case tdata of
        T.NodeCoordData ns ->
          case ns !! (n - 1) of
            T.Node2D _ p -> p
            _ -> error "edgeWeightFromGeo: invalid node data"
        _ -> error "edgeWeightFromGeo: invalid node data"

euc2dToBasic :: T.TspProblem -> Either BasicGraph TspError
euc2dToBasic (T.TspProblem _ _ _ _ _ _ _ _ (T.EdgeWeightData _)) =
  Right $ TspError "euc2dToBasic: no edge weight data"
euc2dToBasic (T.TspProblem _ _ _ _ _ _ _ _ (T.FixedEdgesData _)) =
  Right $ TspError "euc2dToBasic: fixed edges not supported"
euc2dToBasic (T.TspProblem _ _ _ _ _ _ _ _ (T.CombinedData {})) =
  Right $ TspError "euc2dToBasic: combined data not supported"
euc2dToBasic (T.TspProblem _ _ _ dims ewt ewf edf nct tdata) =
  Left $ BasicGraph nodes edges
  where
    nodes = [NodeId i | i <- [1 .. dims]]
    edges =
      [ Edge (NodeId i, NodeId j, edgeWeightFromEuc2d tdata i j)
      | i <- [1 .. dims]
      , j <- [1 .. dims]
      , i /= j
      ]

edgeWeightFromEuc2d :: T.TspData -> Int -> Int -> EdgeWeight
edgeWeightFromEuc2d tdata i j =
  EdgeWeight $ fromIntegral $ euclideanDistance (euc2d i) (euc2d j)
  where
    euc2d n =
      case tdata of
        T.NodeCoordData ns ->
          case ns !! (n - 1) of
            T.Node2D _ p -> p
            _ -> error "edgeWeightFromEuc2d: invalid node data"
        _ -> error "edgeWeightFromEuc2d: invalid node data"
