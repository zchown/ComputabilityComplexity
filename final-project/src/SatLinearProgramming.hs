{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}

module SatLinearProgramming where

import qualified Data.Vector as V
import GHC.TypeLits
import Numeric.LinearProgramming
import SatTypes
import System.Random (RandomGen, newStdGen, uniformR)

randomizedRounding ::
     forall n. KnownNat n
  => SatProblem n
  -> IO (SatSolution n)
randomizedRounding p@(SatProblem cs) = do
  g <- newStdGen
  case runLinear p of
    Nothing -> return Unsatisfiable
    Just xs ->
      return . Satisfiable . varListFromList . randomlyRound (take n xs) $ g
  where
    n = varListSize . positive $ cs V.! 0

runLinear ::
     forall n. KnownNat n
  => SatProblem n
  -> Maybe [Double]
runLinear p@(SatProblem cs) = do
  case simplex (createObjective p) (createConstraints p) b of
    Undefined -> Nothing
    NoFeasible -> Nothing
    Unbounded -> Nothing
    Infeasible _ -> Nothing
    Feasible (_, xs) -> Just xs
    Optimal (_, xs) -> Just xs
  where
    n = varListSize . positive $ cs V.! 0
    b = map (:&: (0, 1)) [0 .. length cs + n]

randomlyRound :: RandomGen g => [Double] -> g -> [Bool]
randomlyRound xs g =
  (fst .
   foldr
     (\x (acc, g') ->
        let (r, g'') = uniformR (0.0, 1.0) g'
         in ((x > r) : acc, g''))
     ([], g))
    xs

createObjective ::
     forall n. KnownNat n
  => SatProblem n
  -> Optimization
createObjective (SatProblem cs) = Maximize $ replicate n 0 ++ replicate m 1
  where
    n = varListSize . positive $ cs V.! 0
    m = length cs

clauseToConstraint ::
     forall n. KnownNat n
  => Clause n
  -> Int
  -> Bound [(Double, Int)]
clauseToConstraint c i = ((-1.0, i) : bs) :>=: b
  where
    (bs, b) = go (clauseToList c) [] 0
    go :: [Int] -> [(Double, Int)] -> Double -> ([(Double, Int)], Double)
    go [] acc j = (acc, j)
    go (x:xs) acc j
      | x < 0 = go xs ((-1.0, -1 * x) : acc) (j - 1)
      | otherwise = go xs ((1.0, x) : acc) j

createConstraints ::
     forall n. KnownNat n
  => SatProblem n
  -> Constraints
createConstraints (SatProblem cs) =
  General $ zipWith clauseToConstraint (V.toList cs) [n + 1 ..]
  where
    n = varListSize . positive $ cs V.! 0
