{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}

module DisjointSet where

import Control.Monad
import Control.Monad.ST
import Data.Array.MArray
import Data.Array.ST

data DisjointSet s = DisjointSet
  { parent :: STUArray s Int Int
  , rank :: STUArray s Int Int
  }

createDisjointSet :: Int -> ST s (DisjointSet s)
createDisjointSet n =
  liftM2 DisjointSet (newListArray (1, n) [1 .. n]) (newArray (1, n) 0)

getRepresentative :: DisjointSet s -> Int -> ST s Int
getRepresentative ds x = do
  p <- readArray (parent ds) x
  if p /= x
    then do
      rep <- getRepresentative ds p
      writeArray (parent ds) x rep
      return rep
    else return x

find :: DisjointSet s -> Int -> Int -> ST s Bool
find ds x y = liftM2 (==) (getRepresentative ds x) (getRepresentative ds y)

link :: DisjointSet s -> Int -> Int -> ST s ()
link ds x y = do
  rx <- readArray (rank ds) x
  ry <- readArray (rank ds) y
  case compare rx ry of
    GT -> writeArray (parent ds) y x
    LT -> writeArray (parent ds) x y
    EQ -> do
      writeArray (parent ds) y x
      writeArray (rank ds) x (rx + 1)

union :: DisjointSet s -> Int -> Int -> ST s ()
union ds x y = do
  rx <- getRepresentative ds x
  ry <- getRepresentative ds y
  unless (rx == ry) $ link ds rx ry
