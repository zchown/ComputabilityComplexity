{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module SatSolvers where

import Data.Bits
import Data.Foldable (find, foldl', foldr')
import GHC.TypeLits
import SatTypes

gsat ::
     forall n. KnownNat n
  => SatProblem n
  -> Maybe (VarAssignment n)
gsat = undefined

dpll ::
     forall n. KnownNat n
  => SatProblem n
  -> VarAssignment n
  -> SatSolution n
dpll p va =
  case findUnits p of
    Just (UnitPropagate (VarAssignment (up, un))) ->
      if not (varListIsZero (up .&. un))
        then Unsatisfiable
        else continueSearch
    Nothing -> continueSearch
  where
    continueSearch =
      let (np@(SatProblem cs), na@(VarAssignment (nap, _))) =
            unitPropagateReduce p va
       in if null cs
            then if evaluateSatProblem p na
                   then Satisfiable nap
                   else Unsatisfiable
            else case checkClauses cs of
                   EmptyClause -> Unsatisfiable
                   AllSatisfied ->
                     if evaluateSatProblem p na
                       then Satisfiable nap
                       else case selectVariable np na of
                              Nothing -> Unsatisfiable
                              Just i -> tryBothAssignments p na i
                   NeedBranch ->
                     case selectVariable np na of
                       Nothing ->
                         if evaluateSatProblem p na
                           then Satisfiable nap
                           else Unsatisfiable
                       Just i -> tryBothAssignments p na i

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
tryBothAssignments p va@(VarAssignment (vp, vn)) i =
  case trueResult of
    Satisfiable s -> Satisfiable s
    Unsatisfiable -> tryFalse
  where
    trueResult =
      case SatTypes.setBit vp i of
        Just vp' -> dpll p (VarAssignment (vp', vn))
        Nothing -> error "Error setting bit"
    tryFalse =
      case SatTypes.setBit vn i of
        Just vn' -> dpll p (VarAssignment (vp, vn'))
        Nothing -> error "Error setting bit"

checkClauses ::
     forall n. KnownNat n
  => [Clause n]
  -> ClauseStatus
checkClauses [] = AllSatisfied
checkClauses cs
  | any isEmptyClause cs = EmptyClause
  | all isSatisfiedClause cs = AllSatisfied
  | otherwise = NeedBranch
  where
    isEmptyClause (Clause p n) = varListIsZero (p .|. n)
    isSatisfiedClause (Clause p n) = not $ varListIsZero (p .|. n)

selectVariable ::
     forall n. KnownNat n
  => SatProblem n
  -> VarAssignment n
  -> Maybe Int
selectVariable (SatProblem cs) (VarAssignment (vp, vn)) =
  let a = vp .|. vn
      n = varListSize a
      cvs = foldl' collectV createVarList cs
   in case find (\i -> not (testBit a i) && testBit cvs i) [0 .. n - 1] of
        Just idx -> Just idx
        Nothing -> find (\i -> not (testBit a i)) [0 .. n - 1]
  where
    collectV acc (Clause p n) = acc .|. p .|. n

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
      let p' = unitPropagate p up
          cva' = VarAssignment (vap .|. vp, van .|. vn)
       in unitPropagateReduce p' cva'

addNewUnitClause ::
     forall n. KnownNat n
  => SatProblem n
  -> VarAssignment n
  -> Maybe (SatProblem n, VarAssignment n, Int)
addNewUnitClause (SatProblem cs) (VarAssignment (vp, vn)) = do
  let allAssigned = vp .|. vn
      findFirstUnassigned :: Int -> Maybe Int
      findFirstUnassigned i
        | i >= varListSize allAssigned = Nothing
        | testBit allAssigned i = findFirstUnassigned (i + 1)
        | otherwise = Just i
  newBit <- findFirstUnassigned 0
  np' <- SatTypes.setBit createVarList newBit
  let newVarAssignment = VarAssignment (np' .|. vp, vn)
      newClause = Clause np' createVarList
  return (SatProblem (newClause : cs), newVarAssignment, newBit)

unitPropagate ::
     forall n. KnownNat n
  => SatProblem n
  -> UnitPropagate n
  -> SatProblem n
unitPropagate (SatProblem cs) (UnitPropagate (VarAssignment (vp, vn))) =
  SatProblem $ (map g . filter f) cs
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
      | pp == 1 && pn == 0 = VarAssignment (vp .|. p, vn)
      | pp == 0 && pn == 1 = VarAssignment (vp, vn .|. n)
      | otherwise = VarAssignment (vp, vn)
      where
        pp = popCount p
        pn = popCount n
