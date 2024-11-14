{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}

module Kruskal where

import Control.Monad
import Control.Monad.ST
import DisjointSet
import GraphTypes

kruskal :: BasicGraph -> BasicGraph
kruskal (BasicGraph nodes edges) =
  BasicGraph nodes $
  runST $ do
    ds <- createDisjointSet (length nodes)
    let sortedEdges = sortEdges edges
    foldM
      (\acc e@(Edge (NodeId u, NodeId v, _)) -> do
         ux <- find ds u
         vx <- find ds v
         if ux /= vx
           then do
             union ds u v
             return (e : acc)
           else return acc)
      []
      sortedEdges

sortEdges :: [Edge] -> [Edge]
sortEdges = mergeAll . runs
  where
    runs [] = []
    runs [x] = [[x]]
    runs xs = frontRun xs : runs (drop (length (frontRun xs)) xs)
    frontRun [] = []
    frontRun (x:y:zs)
      | x `f` y = x : frontRun (y : zs)
      | otherwise = [x]
    frontRun [x] = [x]
    mergeAll [] = []
    mergeAll [x] = x
    mergeAll xs = (mergeAll . mergePairs) xs
    mergePairs (x:y:ys) = merge x y : mergePairs ys
    mergePairs xs = xs
    merge xs [] = xs
    merge [] ys = ys
    merge (x:xs) (y:ys)
      | x `f` y = x : merge xs (y : ys)
      | otherwise = y : merge (x : xs) ys
    f (Edge (_, _, w1)) (Edge (_, _, w2)) = w1 < w2
