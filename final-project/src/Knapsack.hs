{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE BangPatterns #-}

-- FlexibleInstances is needed for the instance of Eq for KnapsackResult
-- BangPattern is used to force strict evaluation in some places
-- This removes thunk build up and can help memory usage and performance
module Knapsack where

-- Allows for monadic iteration which we need because we are using ST monads
-- and will be doing iteration to construct DP tables
import Control.Monad (forM_)

--import as qualified to prevent collision with UArray
-- Used less then UArray so its less annoying to not qualify UArray
import qualified Data.Array as A
import Data.Array.ST -- Allows for mutable state in arrays

-- Using UArray because its more efficient items are unboxed
-- in a normal array items are pointers to the actual data
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

-- Not going to discuss this implementation much
-- Pretty normal foldl' using strictness for performance
-- If the max value is larger then greedy solution return max val
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

-- this implementation makes use of some more advanced Haskell features
-- in order to make some performance improvements
-- (at least I hope) some basic testing suggests it does
-- I will try to explain the reasoning behind the complexities of the
-- implementation as they come up as they will be reused for
-- minCostKnapsack and FPTASKnapsack
knapsack01 :: KnapsackProblem Int -> KnapsackResult Int
knapsack01 (KnapsackProblem is c _) =
  KnapsackResult (backTrack n c) (table ! (n, c))
  where
    n = length is
    -- convert to normal array we need to do lookups by index which we cant 
    -- do  with a list, sense this is items we cant use an unboxed array
    -- unboxed arrays have better performance but can only be used with
    -- c types, also we do this strictly sense we know we will need it
    !items = A.listArray (1, n) is
    -- create the table strictly sense we know we will need it
    table :: UArray (Int, Int) Int
    !table =
      runSTUArray $
      -- runSTUArray allows us to create a mutable array and then freeze it
      -- this is useful for performance and doesn't break Haskell's purity
      -- as we don't actually ever want to mutate the array or share it 
      -- outside this function
       do
        t <- newArray ((0, 0), (n, c)) 0
        -- forM_ is a monadic version of forM it can be thought of as an 
        -- iterable and is allowed to have side effects
        -- in this case we are using it to iterate over the items
        forM_ [1 .. n] $ \i
          -- bang patterns are used to force strict evaluation
          -- we know we will need this item so we force it to be evaluated now
         -> do
          let !item = items A.! i
          -- now we use forM_ again to iterate weights
          forM_ [1 .. c] $ \w -> do
            if itemWeight item <= w
                -- without is the value without the current item
              then do
                without <- readArray t (i - 1, w)
                -- with is the value with the current item
                with <-
                  (\x -> x + itemValue item) <$>
                  readArray t (i - 1, w - itemWeight item)
                writeArray t (i, w) (max without with)
              else readArray t (i - 1, w) >>= writeArray t (i, w)
        return t
    -- backTrack is pretty straight forward it just checks if an item should
    -- be included in the knapsack by comparing the value at i w with the value
    -- at i - 1 w or in otherwords the value with and without the item
    backTrack i w
      | i == 0 || w == 0 = []
      | itemWeight (items A.! i) <= w && table ! (i, w) /= table ! (i - 1, w) =
        (items A.! i) : backTrack (i - 1) (w - itemWeight (items A.! i))
      | otherwise = backTrack (i - 1) w

-- sumWeight + 1 is equivalent to infinite as reachable values will
-- be capped at sumWeight
-- this is nice because we can avoid maxBound which can cause some
--  overflow issues, I assume sumValue and sumWeight are not going to
--  overflow the int bounds otherwise we would need to rethink this and
--  us a different approach probably with Integers
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
