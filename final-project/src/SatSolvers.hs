{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module SatSolvers where

import Data.Bits
import Data.Foldable (find, foldr')
import Data.Maybe (fromJust)
import qualified Data.Vector as V
import GHC.TypeLits
import SatTypes

dpll ::
     forall n. KnownNat n
  => SatProblem n
  -> VarAssignment n
  -> SatSolution n
dpll p@(SatProblem cs) va@(VarAssignment (vap, van)) =
  case checkClauses cs va of
    EmptyClause -> Unsatisfiable
    AllSatisfied -> Satisfiable vap
    NeedBranch ->
      case findUnits p of
        Nothing -> prop p va
        Just up@(UnitPropagate (VarAssignment (vp, vn))) ->
          prop p (VarAssignment (vap .|. vp, van .|. vn))
  where
    prop p va = uncurry quickCheck $ unitPropagateReduce p va
    quickCheck p@(SatProblem cs) va@(VarAssignment (vp, vn)) =
      case checkClauses cs va of
        EmptyClause -> Unsatisfiable
        AllSatisfied -> Satisfiable vp
        NeedBranch -> continueDpll p va
    continueDpll p@(SatProblem cs) va@(VarAssignment (vp, vn)) =
      case selectVariable p va of
        Just i -> tryBothAssignments p va i
        Nothing ->
          case checkClauses cs va of
            AllSatisfied -> Satisfiable vp
            _ -> Unsatisfiable

--------------------------
-- | Helper Functions | --
--------------------------
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
tryBothAssignments p@(SatProblem cs) va@(VarAssignment (vp, vn)) i =
  case trueResult of
    Satisfiable s -> Satisfiable s
    Unsatisfiable -> tryFalse
  where
    trueResult =
      case SatTypes.setBit vp i of
        Just vp' ->
          uncurry dpll $ unitPropagateReduce p (VarAssignment (vp', vn))
        Nothing -> Unsatisfiable
    tryFalse =
      case SatTypes.setBit vn i of
        Just vn' ->
          uncurry dpll $ unitPropagateReduce p (VarAssignment (vp, vn'))
        Nothing -> Unsatisfiable

checkClauses ::
     forall n. KnownNat n
  => V.Vector (Clause n)
  -> VarAssignment n
  -> ClauseStatus
checkClauses cs va@(VarAssignment (vp, vn))
  | V.null cs = AllSatisfied
  | V.any (isEmptyClause va) cs = EmptyClause
  | V.all (isSatisfiedClause va) cs = AllSatisfied
  | otherwise = NeedBranch
  where
    isEmptyClause (VarAssignment (vp, vn)) (Clause p n) =
      varListIsZero ((p .&. complement vn) .|. (n .&. complement vp))
    isSatisfiedClause (VarAssignment (vp, vn)) (Clause p n) =
      not (varListIsZero (p .&. vp)) || not (varListIsZero (n .&. vn))

selectVariable ::
     forall n. KnownNat n
  => SatProblem n
  -> VarAssignment n
  -> Maybe Int
selectVariable (SatProblem cs) (VarAssignment (vp, vn)) =
  let assigned = vp .|. vn
      n = varListSize assigned
      cvs = V.foldl' (\acc (Clause p n) -> acc .|. p .|. n) createVarList cs
   in find (\i -> not (testBit assigned i)) [0 .. n - 1]

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
unitPropagateReduce p cva@(VarAssignment (vap, van)) =
  case findUnits p of
    Nothing -> (p, cva)
    Just up@(UnitPropagate (VarAssignment (vp, vn))) ->
      if not (varListIsZero (vp .&. vn))
        then (SatProblem V.empty, cva)
        else let p' = unitPropagate p up
                 cva' = VarAssignment (vap .|. vp, van .|. vn)
              in unitPropagateReduce p' cva'

unitPropagate ::
     forall n. KnownNat n
  => SatProblem n
  -> UnitPropagate n
  -> SatProblem n
unitPropagate (SatProblem cs) (UnitPropagate (VarAssignment (vp, vn))) =
  SatProblem $ V.map g $ V.filter f cs
  where
    g (Clause p n) = Clause (p .&. (p `xor` vn)) (n .&. (n `xor` vp))
    f (Clause p n) = varListIsZero ((vp .&. p) .|. (vn .&. n))

findUnits ::
     forall n. KnownNat n
  => SatProblem n
  -> Maybe (UnitPropagate n)
findUnits (SatProblem clauses) = actual
  where
    actual =
      if result == VarAssignment (createVarList @n, createVarList @n)
        then Nothing
        else Just (UnitPropagate result)
    result =
      foldr' f (VarAssignment (createVarList @n, createVarList @n)) clauses
    f (Clause p n) (VarAssignment (vp, vn))
      | pp == 1 && pn == 0 && varListIsZero (vn .&. p) =
        VarAssignment (vp .|. p, vn)
      | pp == 0 && pn == 1 && varListIsZero (vp .&. n) =
        VarAssignment (vp, vn .|. n)
      | otherwise = VarAssignment (vp, vn)
      where
        pp = popCount p
        pn = popCount n
