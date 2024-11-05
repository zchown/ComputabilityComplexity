{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module SatSolvers where

import Data.Bits
import Data.Foldable (find, foldr')
import qualified Data.Vector as V
import GHC.TypeLits
import SatTypes

dpll ::
     forall n. KnownNat n
  => SatProblem n
  -> VarAssignment n
  -> SatSolution n
dpll problem assignment =
  case unitPropagateReduce problem assignment of
    (SatProblem cs, va@(VarAssignment (vp', _))) ->
      if V.null cs
        then Satisfiable vp'
        else case checkClauses cs va of
               EmptyClause -> Unsatisfiable
               AllSatisfied -> Satisfiable vp'
               NeedBranch ->
                 case selectVariable va of
                   Nothing -> Unsatisfiable
                   Just i -> tryBothAssignments (SatProblem cs) va i

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
tryBothAssignments prob (VarAssignment (vp, vn)) i =
  case SatTypes.setBit vp i of
    Just vp' ->
      case dpll prob (VarAssignment (vp', vn)) of
        Satisfiable s -> Satisfiable s
        Unsatisfiable ->
          case SatTypes.setBit vn i of
            Just vn' -> dpll prob (VarAssignment (vp, vn'))
            Nothing -> Unsatisfiable
    Nothing -> Unsatisfiable

checkClauses ::
     forall n. KnownNat n
  => V.Vector (Clause n)
  -> VarAssignment n
  -> ClauseStatus
checkClauses cs (VarAssignment (vp, vn))
  | V.null cs = AllSatisfied
  | V.any isEmptyClause cs = EmptyClause
  | V.all isSatisfiedClause cs = AllSatisfied
  | otherwise = NeedBranch
  where
    isEmptyClause (Clause p n) =
      varListIsZero (p .&. (p `xor` vn)) && varListIsZero (n .&. (n `xor` vp))
    isSatisfiedClause (Clause p n) =
      not (varListIsZero (p .&. vp)) || not (varListIsZero (n .&. vn))

selectVariable ::
     forall n. KnownNat n
  => VarAssignment n
  -> Maybe Int
selectVariable (VarAssignment (vp, vn)) =
  let assigned = vp .|. vn
      n = varListSize assigned
   in find (not . testBit assigned) [0 .. n - 1]

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
