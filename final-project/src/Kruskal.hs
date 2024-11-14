module Kruskal where

import DisjointSet
import GraphTypes (BasicGraph)

sortEdges :: BasicGraph -> BasicGraph
sortEdges (BasicGraph ns es) = BasicGraph ns (mergeSort es)
  where
    mergeSort = mergeAll . runs
    runs [] = []
    runs [x] = [x]
    runs xs = frontRun xs : runs (dropFrontRun xs)
    frontRun = takeWhile (uncurry (f)) . pairs
    dropFrontRun = dropWhile (uncurry (f)) . pairs
    mergeAll [] = []
    mergeAll [x] = [x]
    mergeAll xs = (mergeAll . mergePairs) xs
    mergePairs (x:y:ys) = merge x y : mergePairs ys
    mergePairs xs = xs
    merge xs [] = xs
    merge [] ys = ys
    merge (x:xs) (y:ys)
      | x `f` y = x : merge xs (y : ys)
      | otherwise = y : merge (x : xs) ys
    pairs xs = zip xs (tail xs)
    f (_, _, x) (_, _, y) = x <= y

kruskal :: BasicGraph -> BasicGraph
kruskal g@(BasicGraph ns es) = krusk n (sortEdges g) m (BasicGraph ns []) ds
  where
    n = length ns
    ds = createDisjointSet n
    m = zip ns [1 .. n]

krusk ::
     Int
  -> BasicGraph
  -> [(NodeId, Int)]
  -> BasicGraph
  -> DisjointSet
  -> BasicGraph
krusk n (BasicGraph ns (e@(u, v, _):es)) m mst ds
  | n == 1 + length (bgEdges mst) = mst
  | equality u v = k mst ds
  | otherwise = k (addEdge e mst) (union ds u v)
  where
    k = krusk n es m
    equality a b = fst (find ds m a) == fst (find ds m b)
