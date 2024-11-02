{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module SatSolvers where

import Data.Bits
import Data.Foldable (foldr')
import Data.Maybe (fromJust, isNothing)
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
  -> Maybe (VarAssignment n)
dpll (SatProblem []) va = Just va
dpll p va =
  case (flippedAssignment, result) of
    (Just flipped, Nothing) -> dpll newFormula flipped
    (_, Just r)
      | evaluateSatProblem p r -> Just r
    _ -> Nothing
  where
    (p'@(SatProblem cs'), va') = unitPropagateReduce p va
    (newFormula, newAssignment@(VarAssignment (nap, nan)), nb) =
      case addNewUnitClause p' va' of
        Nothing -> (p', va', 0)
        Just (a, b, c) -> (a, b, c)
    flippedAssignment = do
      flippedP <- flipBit nap nb
      flippedN <- flipBit nan nb
      return $ VarAssignment (flippedP, flippedN)
    result
      | checkProblem p' = Nothing
      | null cs' = Just va
      | va' == newAssignment = Nothing
      | otherwise = dpll newFormula newAssignment

--------------------------
-- | Helper Functions | --
--------------------------
checkProblem ::
     forall n. KnownNat n
  => SatProblem n
  -> Bool
checkProblem (SatProblem clauses) =
  not $ any (varListIsZero . (\(Clause p n) -> p .|. n)) clauses

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
