{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE BangPatterns #-}

module SatSolvers where

import Control.Monad
import Control.Monad.ST
import Data.Array.MArray
import Data.Array.ST
import Data.Bits
import Data.Foldable (find, foldr')
import Data.List (elemIndex)
import Data.Maybe (fromJust, fromMaybe)
import qualified Data.Vector as V
import GHC.TypeLits
import SatTypes

gsat ::
     forall n. KnownNat n
  => SatProblem n
  -> Int
  -> Int
  -> VarAssignment n
gsat p maxTries maxFlips = go maxTries (createVarAssignment @n)
  where
    go :: Int -> VarAssignment n -> VarAssignment n
    go 0 !b = b
    go i best = go (i - 1) $ gogo maxFlips best (createRandomVarAssignment @n)
    gogo :: Int -> VarAssignment n -> VarAssignment n -> VarAssignment n
    gogo 0 !b _ = b
    gogo j !b va@(VarAssignment !vp !_)
      | numberOfUnsatisfiedClauses p va' < numberOfUnsatisfiedClauses p b =
        gogo (j - 1) va' va'
      | otherwise = gogo (j - 1) b va'
      where
        va' = VarAssignment vp' (complement vp')
        vp' :: VarList n
        vp' = fromJust $ flipBit vp (findFlipVar p va)

dpll ::
     forall n. KnownNat n
  => SatProblem n
  -> SatSolution n
dpll !p = dpll' p (createVarAssignment @n) 0

dpll' ::
     forall n. KnownNat n
  => SatProblem n
  -> VarAssignment n
  -> Int
  -> SatSolution n
dpll' !p !a !j =
  if V.null cs
    then Satisfiable vp
    else case checkClauses cs a of
           EmptyClause -> Unsatisfiable
           AllSatisfied -> Satisfiable vp
           NeedBranch ->
             case selectVariable a j of
               Nothing -> Unsatisfiable
               Just !i -> tryBothAssignments p a i
  where
    (SatProblem !cs, VarAssignment !vp _) = unitPropagateReduce p a

-------------------------------
-- | GSAT Helper Functions | --
-------------------------------
findFlipVar ::
     forall n. KnownNat n
  => SatProblem n
  -> VarAssignment n
  -> Int
findFlipVar (SatProblem cs) (VarAssignment vp vn) = go
  where
    s = varListSize vp
    uc =
      V.filter (\(Clause p n) -> varListIsZero ((vp .&. p) .|. (vn .&. n))) cs
    go =
      runST $ do
        t <-
          newListArray (0, s - 1) (replicate s 0) :: ST s (STUArray s Int Int)
        forM_ uc $ \(Clause p n) -> do
          forM_ [0 .. s - 1] $ \i -> do
            when (testBit p i) $ do
              v <- readArray t i
              writeArray t i (v + 1)
            when (testBit n i) $ do
              v <- readArray t i
              writeArray t i (v + 1)
        scores <- getElems t
        let m = maximum scores
        return $ fromMaybe 0 $ elemIndex m scores

numberOfUnsatisfiedClauses ::
     forall n. KnownNat n
  => SatProblem n
  -> VarAssignment n
  -> Int
numberOfUnsatisfiedClauses (SatProblem cs) (VarAssignment vp vn) = length uc
  where
    uc =
      V.filter (\(Clause p n) -> varListIsZero ((vp .&. p) .|. (vn .&. n))) cs

-------------------------------
-- | DPLL Helper Functions | --
-------------------------------
data ClauseStatus
  = EmptyClause
  | AllSatisfied
  | NeedBranch
  deriving (Show, Eq)

tryBothAssignments ::
     forall n. KnownNat n
  => SatProblem n
  -> VarAssignment n
  -> Int
  -> SatSolution n
tryBothAssignments !prob (VarAssignment !vp !vn) !i =
  case SatTypes.setBit vp i of
    Just !vp' ->
      case dpll' prob (VarAssignment vp' vn) i of
        Satisfiable s -> Satisfiable s
        Unsatisfiable ->
          case SatTypes.setBit vn i of
            Just !vn' -> dpll' prob (VarAssignment vp vn') i
            Nothing -> Unsatisfiable
    Nothing -> Unsatisfiable

checkClauses ::
     forall n. KnownNat n
  => V.Vector (Clause n)
  -> VarAssignment n
  -> ClauseStatus
checkClauses !cs (VarAssignment !vp !vn)
  | V.null cs = AllSatisfied
  | V.any isEmptyClause cs = EmptyClause
  | V.all isSatisfiedClause cs = AllSatisfied
  | otherwise = NeedBranch
  where
    isEmptyClause (Clause !p !n) =
      varListIsZero (p .&. (p `xor` vn)) && varListIsZero (n .&. (n `xor` vp))
    isSatisfiedClause (Clause !p !n) =
      not (varListIsZero (p .&. vp)) || not (varListIsZero (n .&. vn))

selectVariable ::
     forall n. KnownNat n
  => VarAssignment n
  -> Int
  -> Maybe Int
selectVariable (VarAssignment !vp !vn) !i =
  let !assigned = vp .|. vn
      !n = varListSize assigned
   in find (not . testBit assigned) [i .. n - 1]

--------------------------
-- | Unit Propagation | --
--------------------------
newtype UnitPropagate n =
  UnitPropagate (VarAssignment n)
  deriving (Show, Eq)

unitPropagateReduce ::
     forall n. KnownNat n
  => SatProblem n
  -> VarAssignment n
  -> (SatProblem n, VarAssignment n)
unitPropagateReduce !p cva@(VarAssignment !vap !van) =
  case findUnits p of
    Nothing -> (p, cva)
    Just up@(UnitPropagate (VarAssignment !vp !vn)) ->
      if not (varListIsZero (vp .&. vn))
        then (SatProblem V.empty, cva)
        else let !p' = unitPropagate p up
                 !cva' = VarAssignment (vap .|. vp) (van .|. vn)
              in unitPropagateReduce p' cva'

unitPropagate ::
     forall n. KnownNat n
  => SatProblem n
  -> UnitPropagate n
  -> SatProblem n
unitPropagate (SatProblem !cs) (UnitPropagate (VarAssignment !vp !vn)) =
  SatProblem $ V.map g $ V.filter f cs
  where
    g (Clause !p !n) = Clause (p .&. (p `xor` vn)) (n .&. (n `xor` vp))
    f (Clause !p !n) = varListIsZero ((vp .&. p) .|. (vn .&. n))

findUnits ::
     forall n. KnownNat n
  => SatProblem n
  -> Maybe (UnitPropagate n)
findUnits (SatProblem !clauses) = actual
  where
    !actual =
      if result == VarAssignment (createVarList @n) (createVarList @n)
        then Nothing
        else Just (UnitPropagate result)
    !result =
      foldr' f (VarAssignment (createVarList @n) (createVarList @n)) clauses
    f (Clause !p !n) (VarAssignment !vp !vn)
      | pp == 1 && pn == 0 && varListIsZero (vn .&. p) =
        VarAssignment (vp .|. p) vn
      | pp == 0 && pn == 1 && varListIsZero (vp .&. n) =
        VarAssignment vp (vn .|. n)
      | otherwise = VarAssignment vp vn
      where
        !pp = popCount p
        !pn = popCount n
