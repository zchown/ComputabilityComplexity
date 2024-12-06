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
randomizedRounding p = do
  g <- newStdGen
  case runLinear p of
    Nothing -> return Unsatisfiable
    Just xs -> return . Satisfiable . varListFromList . randomlyRound xs $ g

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
    lo = map (:>=: 1) [0 .. length cs + n]
    gz = map (:<=: 0) [0 .. length cs + n]
    b = lo ++ gz

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
clauseToConstraint c i = cc :>=: 0
  where
    cc = (-1.0, i) : (map (1.0, ) . clauseToList) c

createConstraints ::
     forall n. KnownNat n
  => SatProblem n
  -> Constraints
createConstraints (SatProblem cs) =
  Sparse $ zipWith clauseToConstraint (V.toList cs) [n ..]
  where
    n = varListSize . positive $ cs V.! 0
