{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}

module GraphTypes where

import Control.Monad (forM_)
import Control.Monad.ST (ST)
import Data.Array.Base
import Data.Array.ST
import Data.Array.Unboxed (UArray)

newtype NodeId =
  NodeId Int
  deriving (Show, Eq, Ord, Ix)

newtype EdgeWeight =
  EdgeWeight Double
  deriving (Show, Eq, Ord, Num)

-- instance MArray (STUArray s) EdgeWeight ST where
--     getBounds = getBounds . castSTUArray
--     getNumElements = getNumElements . castSTUArray
--     newArray b e = newArray b (realToFrac e) >>= return . castSTUArray
--     newArray_ b = newArray_ b >>= return . castSTUArray
--     unsafeRead arr i = unsafeRead (castSTUArray arr) i >>= return . realToFrac
--     unsafeWrite arr i e = unsafeWrite (castSTUArray arr) i (realToFrac e)
--
-- instance IArray UArray EdgeWeight where
--     bounds = bounds . castUArray
--     numElements = numElements . castUArray
--     unsafeArray b es = castUArray $ unsafeArray b [(i, realToFrac e) | (i,e) <- es]
--     unsafeAt arr i = EdgeWeight $ realToFrac (unsafeAt (castUArray arr) i)
-- newtype AdjacencyMatrix = AdjacencyMatrix { matrix :: UArray (NodeId, NodeId) EdgeWeight }
--   deriving (Show, Eq)
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
------------------------------
-- | Conversion functions | --
------------------------------
-- basicToNode :: BasicGraph -> NodeGraph
-- basicToNode (BasicGraph ns es) = NodeGraph $ map mkNode ns
--   where
--     mkNode n = Node n (map (\(from, to, w) -> (to, w)) $ connecting n)
--     connecting n = filter (\(from, _, _) -> from == n) es
--
-- nodeToBasic :: NodeGraph -> BasicGraph
-- nodeToBasic (NodeGraph ns) = BasicGraph (map nNodeId ns) (concatMap mkEdges ns)
--   where
--     mkEdges (Node n es) = map (\(to, w) -> (n, to, w)) es
--
-- basicToAdjacency :: BasicGraph -> AdjacencyMatrix
-- basicToAdjacency bg@(BasicGraph ns es) = AdjacencyMatrix table
--   where
--     size = maximum $ map (\(NodeId x) -> x) ns
--     table :: UArray (NodeId, NodeId) EdgeWeight
--     table = runSTUArray $ do
--       arr <- newArray ((NodeId 1, NodeId 1), (NodeId size, NodeId size)) (EdgeWeight 0)
--       forM_ es $ \(from, to, w) -> writeArray arr (from, to) w
--       return arr
--
-- nodeToAdjacency :: NodeGraph -> AdjacencyMatrix
-- nodeToAdjacency = basicToAdjacency . nodeToBasic
