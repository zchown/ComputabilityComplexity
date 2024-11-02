{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE BangPatterns #-}

module Knapsack where

import Control.Monad (forM_)
import qualified Data.Array as A
import Data.Array.ST
import Data.Array.Unboxed
import Data.Foldable (foldl', foldr')
import Data.List (sortBy)
import GHC.Float (int2Double)

data Item a = Item
  { itemWeight :: a
  , itemValue :: a
  } deriving (Show)

compareByRatio :: (Ord a, Fractional a) => Item a -> Item a -> Ordering
compareByRatio (Item w1 v1) (Item w2 v2) = compare (v1 / w1) (v2 / w2)

compareByValues :: Ord a => Item a -> Item a -> Ordering
compareByValues (Item w1 v1) (Item w2 v2)
  | v1 == v2 =
    if w1 < w2
      then LT
      else GT
  | v1 < v2 = LT
  | otherwise = GT

data KnapsackProblem a = KnapsackProblem
  { pItems :: [Item a]
  , capacity :: a
  , epsilon :: Double
  } deriving (Show)

data KnapsackResult a = KnapsackResult
  { rItems :: [Item a]
  , knapsackValue :: a
  } deriving (Show)

checkItemEquality :: Eq a => Item a -> Item a -> Bool
checkItemEquality (Item w1 v1) (Item w2 v2) = w1 == w2 && v1 == v2

checkItemListEquality :: (Ord a) => [Item a] -> [Item a] -> Bool
checkItemListEquality xs ys =
  length xs == length ys &&
  all
    (uncurry checkItemEquality)
    (zip (sortBy compareByValues xs) (sortBy compareByValues ys))

instance (Ord a) => Eq (KnapsackResult a) where
  (==) (KnapsackResult is1 v1) (KnapsackResult is2 v2) =
    checkItemListEquality is1 is2 && v1 == v2

greedyKnapsack :: (Ord a, Fractional a) => KnapsackProblem a -> KnapsackResult a
greedyKnapsack (KnapsackProblem [] _ _) = KnapsackResult [] 0
greedyKnapsack (KnapsackProblem is c _)
  | tv == 0 = KnapsackResult [] 0
  | itemValue mx < tv = KnapsackResult its tv
  | otherwise = KnapsackResult [mx] (itemValue mx)
  where
    (tv, _, mx, its) =
      foldl' selectItem (0, c, Item 0 0, []) (sortBy (flip compareByRatio) is)
    selectItem (totalValue, remainingCap, curMax, items) i@(Item weight value)
      | weight <= remainingCap =
        ( totalValue + value
        , remainingCap - weight
        , if (value > itemValue curMax) && (itemWeight i <= c)
            then i
            else curMax
        , i : items)
      | otherwise =
        ( totalValue
        , remainingCap
        , if (value > itemValue curMax) && (itemWeight i <= c)
            then i
            else curMax
        , items)

knapsack01 :: KnapsackProblem Int -> KnapsackResult Int
knapsack01 (KnapsackProblem is c _) =
  KnapsackResult (backTrack n c) (table ! (n, c))
  where
    n = length is
    !items = A.listArray (1, n) is
    table :: UArray (Int, Int) Int
    !table =
      runSTUArray $ do
        t <- newArray ((0, 0), (n, c)) 0
        forM_ [1 .. n] $ \i -> do
          let !item = items A.! i
          forM_ [1 .. c] $ \w -> do
            if itemWeight item <= w
              then do
                without <- readArray t (i - 1, w)
                with <-
                  (\x -> x + itemValue item) <$>
                  readArray t (i - 1, w - itemWeight item)
                writeArray t (i, w) (max without with)
              else readArray t (i - 1, w) >>= writeArray t (i, w)
        return t
    backTrack i w
      | i == 0 || w == 0 = []
      | itemWeight (items A.! i) <= w && table ! (i, w) /= table ! (i - 1, w) =
        (items A.! i) : backTrack (i - 1) (w - itemWeight (items A.! i))
      | otherwise = backTrack (i - 1) w

minCostKnapsack :: KnapsackProblem Int -> KnapsackResult Int
minCostKnapsack (KnapsackProblem [] _ _) = KnapsackResult [] 0
minCostKnapsack (KnapsackProblem is c _)
  | sumWeight <= c = KnapsackResult is sumValue
  | otherwise = KnapsackResult takeList takeValue
  where
    (sumValue, sumWeight, n) =
      foldr'
        (\(Item w v) (totalValue, totalWeight, l) ->
           (totalValue + v, totalWeight + w, l + 1))
        (0, 0, 0)
        is
    !items = A.listArray (1, n) is
    table :: UArray (Int, Int) Int
    !table =
      runSTUArray $ do
        t <- newArray ((0, 0), (n, sumValue)) (sumWeight + 1)
        forM_ [0 .. n] $ \i -> writeArray t (i, 0) 0
        let Item w1 v1 = items A.! 1
        forM_ [1 .. v1] $ \v -> do
          writeArray t (1, v) w1
        forM_ [2 .. n] $ \i -> do
          let Item iw iv = items A.! i
          forM_ [0 .. sumValue] $ \v -> do
            let nextT = max 0 (v - iv)
            x <- readArray t (i - 1, v)
            y <- (iw +) <$> readArray t (i - 1, nextT)
            if x <= y
              then writeArray t (i, v) x
              else writeArray t (i, v) y
        return t
    binarySearch :: Int -> Int -> Int
    binarySearch l r
      | l > r = l - 1
      | otherwise =
        let m = floor ((int2Double l + int2Double r) / 2.0)
         in if table ! (n, m) <= c
              then binarySearch (m + 1) r
              else binarySearch l (m - 1)
    backTrack :: Int -> Int -> [Item Int]
    backTrack i v
      | i == 0 || v == 0 = []
      | table ! (i, v) == table ! (i - 1, v) = backTrack (i - 1) v
      | otherwise =
        let it@(Item _ iv) = items A.! i
         in it : backTrack (i - 1) (max 0 (v - iv))
    takeValue = binarySearch 0 sumValue
    takeList = backTrack n takeValue

fptasKnapsack :: KnapsackProblem Int -> KnapsackResult Int
fptasKnapsack (KnapsackProblem [] _ _) = KnapsackResult [] 0
fptasKnapsack (KnapsackProblem is c e)
  | sumWeight <= c = KnapsackResult is sumValue'
  | otherwise = KnapsackResult takeList takeValue
  where
    (sumValue', sumWeight, maxVal, n) =
      foldr'
        (\(Item w v) (totalValue, totalWeight, curMax, l) ->
           (totalValue + v, totalWeight + w, max curMax v, l + 1))
        (0, 0, 0, 0)
        is
    f = e * (int2Double maxVal / int2Double n)
    adjust :: Int -> Int
    adjust v = max 1 $ floor (int2Double v / f)
    !sis = map (\(Item w v) -> Item w (adjust v)) is
    sumValue = sum $ map itemValue sis
    !items = A.listArray (1, n) is
    !sitems = A.listArray (1, n) sis
    table :: UArray (Int, Int) Int
    !table =
      runSTUArray $ do
        t <- newArray ((0, 0), (n, sumValue)) (sumWeight + 1)
        forM_ [0 .. n] $ \i -> writeArray t (i, 0) 0
        let Item w1 v1 = sitems A.! 1
        forM_ [1 .. v1] $ \v -> do
          writeArray t (1, v) w1
        forM_ [2 .. n] $ \i -> do
          let Item iw iv = sitems A.! i
          forM_ [0 .. sumValue] $ \v -> do
            let nextT = max 0 (v - iv)
            x <- readArray t (i - 1, v)
            y <- (iw +) <$> readArray t (i - 1, nextT)
            if x <= y
              then writeArray t (i, v) x
              else writeArray t (i, v) y
        return t
    binarySearch :: Int -> Int -> Int
    binarySearch l r
      | l > r = l - 1
      | otherwise =
        let m = floor ((int2Double l + int2Double r) / 2.0)
         in if table ! (n, m) <= c
              then binarySearch (m + 1) r
              else binarySearch l (m - 1)
    backTrack :: Int -> Int -> [Item Int]
    backTrack i v
      | i == 0 || v == 0 = []
      | table ! (i, v) == table ! (i - 1, v) = backTrack (i - 1) v
      | otherwise =
        let it = items A.! i
            Item _ siv = sitems A.! i
         in it : backTrack (i - 1) (v - siv)
    sol = binarySearch 0 sumValue
    takeList = backTrack n sol
    takeValue = sum $ map itemValue takeList

-- Knapsack problems and solutions should really be integers
-- but being able to convert between double and int is useful
-- because double allows for better calculations of v/w ratios
-- really only needed for greedyKnapsack
convertKnapsackProblemIntDouble :: KnapsackProblem Int -> KnapsackProblem Double
convertKnapsackProblemIntDouble (KnapsackProblem is c e) =
  KnapsackProblem (map convertItem is) (fromIntegral c) e
  where
    convertItem (Item w v) = Item (fromIntegral w) (fromIntegral v)

convertKnapsackProblemDoubleInt :: KnapsackProblem Double -> KnapsackProblem Int
convertKnapsackProblemDoubleInt (KnapsackProblem is c e) =
  KnapsackProblem (map convertItem is) (round c) e
  where
    convertItem (Item w v) = Item (round w) (round v)

convertKnapsackResultIntDouble :: KnapsackResult Int -> KnapsackResult Double
convertKnapsackResultIntDouble (KnapsackResult is v) =
  KnapsackResult (map convertItem is) (fromIntegral v)
  where
    convertItem (Item w val) = Item (fromIntegral w) (fromIntegral val)

convertKnapsackResultDoubleInt :: KnapsackResult Double -> KnapsackResult Int
convertKnapsackResultDoubleInt (KnapsackResult is v) =
  KnapsackResult (map convertItem is) (round v)
  where
    convertItem (Item w val) = Item (round w) (round val)
