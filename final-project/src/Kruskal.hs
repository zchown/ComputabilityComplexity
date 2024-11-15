{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}

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

-- this implementation is stolen from Data.List
sortEdges :: [Edge] -> [Edge]
sortEdges = mergeAll . runs
  where
    runs (x:y:zs)
      | x `f` y == GT = descending y [x] zs
      | otherwise = ascending y (x :) zs
    runs xs = [xs]
    descending y acc (x:xs)
      | y `f` x == GT = descending x (y : acc) xs
    descending y acc xs = (y : acc) : runs xs
    ascending y acc (x:xs)
      | y `f` x /= GT = ascending x (\ys -> acc (y : ys)) xs
    ascending y acc xs =
      let !x = acc [y]
       in x : runs xs
    mergeAll [x] = x
    mergeAll xs = mergeAll (mergePairs xs)
    mergePairs (x:y:zs) =
      let !a = merge x y
       in a : mergePairs zs
    mergePairs xs = xs
    merge xs@(x:xs') ys@(y:ys')
      | x `f` y == GT = y : merge xs ys'
      | otherwise = x : merge xs' ys
    merge [] ys = ys
    merge xs [] = xs
    f (Edge (_, _, w1)) (Edge (_, _, w2)) = compare w1 w2

-- this is the best implementation I came up with
-- it is based on the same ideas as the previous one
-- but is less optimized and less readable :(
sortEdges' :: [Edge] -> [Edge]
sortEdges' = mergeAll . runs
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
