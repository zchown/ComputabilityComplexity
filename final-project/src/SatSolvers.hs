{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module SatSolvers where

import Data.Bits
import Data.Foldable (foldr')
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
  -> Maybe (VarAssignment n)
dpll = undefined

-- dpllRecursive ::
--      forall n. KnownNat n
--   => SatProblem n
--   -> VarAssignment n
--   -> Maybe (VarAssignment n)
-- dpllRecursive p@(SatProblem cs) va
--   | not isValid = Nothing
--   | all (\(Clause p n) -> varListIsZero (p .|. n)) cs = Just va
--   | otherwise = cs
--   where
--     (newProblem, newAssignment) = unitPropagateAll p va
--     isValid = checkProblem newProblem
--------------------------
-- | Helper Functions | --
--------------------------
checkProblem ::
     forall n. KnownNat n
  => SatProblem n
  -> Bool
checkProblem (SatProblem clauses) = all f clauses
  where
    f (Clause p n) = (not . varListIsZero) (p .|. n)

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
  -> Maybe (SatProblem n, VarAssignment n)
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
  return (SatProblem (newClause : cs), newVarAssignment)

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
