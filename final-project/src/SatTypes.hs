{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE UnboxedTuples #-}

module SatTypes where

import Control.Monad (foldM)
import Data.Bits
import Data.Foldable (foldl', foldr')
import Data.Proxy
import qualified Data.Vector as VE
import Data.Vector.Unboxed (Vector)
import qualified Data.Vector.Unboxed as V
import Data.Word (Word64)
import GHC.TypeLits

--------------------------------
-- | Core SAT Problem Types | --
--------------------------------
newtype SatProblem (n :: Nat) =
  SatProblem (VE.Vector (Clause n))
  deriving (Eq, Show)

data SatSolution (n :: Nat)
  = Satisfiable !(VarList n)
  | Unsatisfiable
  deriving (Eq)

instance Show (SatSolution n) where
  show (Satisfiable _) = "Satisfiable "
  show Unsatisfiable = "Unsatisfiable"

newtype BoolSatSolution (n :: Nat) =
  BoolSatSolution [Bool]
  deriving (Eq, Show)

newtype BoolAssignment (n :: Nat) =
  BoolAssignment [Maybe Bool]
  deriving (Eq, Show)

data VarAssignment (n :: Nat) = VarAssignment
  { assignedPositive :: {-# UNPACK #-} !(VarList n)
  , assignedNegative :: {-# UNPACK #-} !(VarList n)
  } deriving (Eq, Show)

data Clause (n :: Nat) = Clause
  { positive :: {-# UNPACK #-} !(VarList n)
  , negative :: {-# UNPACK #-} !(VarList n)
  } deriving (Eq)

newtype VarList (n :: Nat) =
  VarList (Vector Word64)
  deriving (Eq)

--------------------------------
-- | SAT Problem Operations | --
--------------------------------
evaluateSatProblem ::
     forall n. KnownNat n
  => SatProblem n
  -> VarAssignment n
  -> Bool
evaluateSatProblem (SatProblem !cs) !v = VE.all (evaluateClause v) cs

{-# INLINE evaluateSatProblem #-}
satProblemFromList ::
     forall n. KnownNat n
  => [[Int]]
  -> Maybe (SatProblem n)
satProblemFromList !xs = do
  !clauses <- mapM clauseFromList xs
  return $! SatProblem $! VE.fromList clauses

{-# INLINE satProblemFromList #-}
--------------------------------
-- | Clause Type Operations | --
--------------------------------
instance KnownNat n => Show (Clause n) where
  show (Clause !p !n) =
    "Clause {positive = " ++
    show (toBitList p) ++ ", negative = " ++ show (toBitList n) ++ "}"

createClause ::
     forall n. KnownNat n
  => Clause n
createClause =
  let !emptyList = createVarList @n
   in Clause emptyList emptyList

{-# INLINE createClause #-}
isClauseEmpty ::
     forall n. KnownNat n
  => Clause n
  -> Bool
isClauseEmpty (Clause !p !n) = varListIsZero $! p .|. n

{-# INLINE isClauseEmpty #-}
addClause :: Clause n -> SatProblem n -> SatProblem n
addClause !c (SatProblem !cs) = SatProblem $! cs VE.++ VE.singleton c

{-# INLINE addClause #-}
evaluateClause ::
     forall n. KnownNat n
  => VarAssignment n
  -> Clause n
  -> Bool
evaluateClause (VarAssignment !vp !vn) (Clause !p !n) = not x || not y
  where
    !x = varListIsZero $! vp .&. p
    !y = varListIsZero $! vn .&. n

{-# INLINE evaluateClause #-}
clauseFromList ::
     forall n. KnownNat n
  => [Int]
  -> Maybe (Clause n)
clauseFromList !xs = do
  let !maxVar = varListSize (undefined :: VarList n) - 1
      (!p, !n) = foldl' categorize ([], []) xs
      !valid = all (\x -> (abs x - 1) <= maxVar) xs
  if not valid
    then Nothing
    else do
      let !clause = createClause @n
      !c' <- foldM (\c x -> addPositiveVar c (x - 1)) clause p
      foldM (\c x -> addNegativeVar c (abs x - 1)) c' n
  where
    categorize (!p, !n) !x
      | x > 0 = (x : p, n)
      | x < 0 = (p, x : n)
      | otherwise = (p, n)

{-# INLINE clauseFromList #-}
unsafeClauseFromList ::
     forall n. KnownNat n
  => [Int]
  -> Clause n
unsafeClauseFromList !xs =
  case clauseFromList xs of
    Just !c -> c
    Nothing -> error "Invalid clause"

{-# INLINE unsafeClauseFromList #-}
------------------------------------
-- | Clause Variable Operations | --
------------------------------------
addPositiveVar ::
     forall n. KnownNat n
  => Clause n
  -> Int
  -> Maybe (Clause n)
addPositiveVar (Clause !p !n) !i = do
  !p' <- SatTypes.setBit p i
  return $! Clause p' n

{-# INLINE addPositiveVar #-}
addNegativeVar ::
     forall n. KnownNat n
  => Clause n
  -> Int
  -> Maybe (Clause n)
addNegativeVar (Clause !p !n) !i = do
  !n' <- SatTypes.setBit n i
  return $! Clause p n'

{-# INLINE addNegativeVar #-}
removePositiveVar ::
     forall n. KnownNat n
  => Clause n
  -> Int
  -> Maybe (Clause n)
removePositiveVar (Clause !p !n) !i = do
  !p' <- SatTypes.clearBit p i
  return $! Clause p' n

{-# INLINE removePositiveVar #-}
removeNegativeVar ::
     forall n. KnownNat n
  => Clause n
  -> Int
  -> Maybe (Clause n)
removeNegativeVar (Clause !p !n) !i = do
  !n' <- SatTypes.clearBit n i
  return $! Clause p n'

{-# INLINE removeNegativeVar #-}
isPositiveVar ::
     forall n. KnownNat n
  => Clause n
  -> Int
  -> Bool
isPositiveVar (Clause !p _) = testBit p

{-# INLINE isPositiveVar #-}
isNegativeVar ::
     forall n. KnownNat n
  => Clause n
  -> Int
  -> Bool
isNegativeVar (Clause _ !n) = testBit n

{-# INLINE isNegativeVar #-}
getPositiveVars ::
     forall n. KnownNat n
  => Clause n
  -> [Int]
getPositiveVars c@(Clause !p _) =
  [i | i <- [0 .. varListSize p - 1], isPositiveVar c i]

{-# INLINE getPositiveVars #-}
getNegativeVars ::
     forall n. KnownNat n
  => Clause n
  -> [Int]
getNegativeVars c@(Clause _ !n) =
  [i | i <- [0 .. varListSize n - 1], isNegativeVar c i]

{-# INLINE getNegativeVars #-}
-------------------------------------
-- | VarList Type Core Operations |--
-------------------------------------
instance KnownNat n => Show (VarList n) where
  show !v = "VarList " ++ show (toBitList v)

minWords :: Integer -> Int
minWords !n = fromInteger ((n + 63) `div` 64)

{-# INLINE minWords #-}
createVarList ::
     forall n. KnownNat n
  => VarList n
createVarList =
  let !n = natVal (Proxy @n)
   in VarList $! V.replicate (minWords n) 0

{-# INLINE createVarList #-}
varListFromList ::
     forall n. KnownNat n
  => [Bool]
  -> VarList n
varListFromList !bs =
  let !n = natVal (Proxy @n)
      !m = minWords n
      !valid = (length bs == fromInteger n)
   in if valid
        then VarList $!
             V.generate m $ \i ->
               let !start = i * 64
                   !end = min (start + 64) (fromInteger n)
                   !bits = take (end - start) $ drop start bs
                in foldr'
                     (\b w ->
                        w `shiftL` 1 .|.
                        if b
                          then 1
                          else 0)
                     0
                     bits
        else error "Invalid list length"

{-# INLINE varListFromList #-}
validateVarList ::
     forall n. KnownNat n
  => Vector Word64
  -> Maybe (VarList n)
validateVarList !ws =
  let !n = natVal (Proxy @n)
      !m = minWords n
      !lastWordBits = fromInteger $ n `mod` 64
      !valid =
        not (V.null ws) &&
        (lastWordBits == 0 ||
         (V.last ws .&. complement (bit lastWordBits - 1)) == 0)
   in if V.length ws == m && valid
        then Just $! VarList ws
        else Nothing

{-# INLINE validateVarList #-}
varListSize ::
     forall n. KnownNat n
  => VarList n
  -> Int
varListSize _ = fromInteger $! natVal (Proxy @n)

{-# INLINE varListSize #-}
varListIsZero :: VarList n -> Bool
varListIsZero (VarList xs) = V.all (== 0) xs

{-# INLINE varListIsZero #-}
--------------------------------
-- | VarList Bit Operations | --
--------------------------------
instance KnownNat n => Bits (VarList n) where
  {-# INLINE (.&.) #-}
  VarList xs .&. VarList ys = VarList $! V.zipWith (.&.) xs ys
  {-# INLINE (.|.) #-}
  VarList xs .|. VarList ys = VarList $! V.zipWith (.|.) xs ys
  {-# INLINE xor #-}
  xor (VarList xs) (VarList ys) = VarList $! V.zipWith xor xs ys
  {-# INLINE complement #-}
  complement (VarList xs) =
    let !n = natVal (Proxy @n)
        !lastWordBits = fromInteger $ n `mod` 64
        maskLastWord !w =
          if lastWordBits == 0
            then complement w
            else complement w .&. (bit lastWordBits - 1)
        !result =
          if V.null xs
            then V.empty
            else V.generate len $ \i ->
                   if i < len - 1
                     then complement (xs V.! i)
                     else maskLastWord (xs V.! i)
          where
            !len = V.length xs
     in VarList result
  shift v _ = v
  rotate v _ = v
  {-# INLINE bitSize #-}
  bitSize = varListSize
  {-# INLINE bitSizeMaybe #-}
  bitSizeMaybe = Just . varListSize
  isSigned _ = False
  {-# INLINE testBit #-}
  testBit (VarList xs) !i
    | i < 0 || i >= varListSize (undefined :: VarList n) = False
    | otherwise = testBit (xs V.! (i `div` 64)) (i `mod` 64)
  {-# INLINE bit #-}
  bit !i
    | i < 0 || i >= varListSize (undefined :: VarList n) = createVarList @n
    | otherwise =
      VarList $! bit64VectorBounded (varListSize (undefined :: VarList n)) i
  {-# INLINE popCount #-}
  popCount (VarList xs) = V.sum $! V.map popCount xs

setBit ::
     forall n. KnownNat n
  => VarList n
  -> Int
  -> Maybe (VarList n)
setBit = bitChangePattern Data.Bits.setBit

{-# INLINE setBit #-}
setBits ::
     forall n. KnownNat n
  => VarList n
  -> [Int]
  -> Maybe (VarList n)
setBits = foldM SatTypes.setBit

{-# INLINE setBits #-}
clearBit ::
     forall n. KnownNat n
  => VarList n
  -> Int
  -> Maybe (VarList n)
clearBit = bitChangePattern Data.Bits.clearBit

{-# INLINE clearBit #-}
flipBit ::
     forall n. KnownNat n
  => VarList n
  -> Int
  -> Maybe (VarList n)
flipBit = bitChangePattern Data.Bits.complementBit

{-# INLINE flipBit #-}
bitChangePattern ::
     forall n. KnownNat n
  => (Word64 -> Int -> Word64)
  -> VarList n
  -> Int
  -> Maybe (VarList n)
bitChangePattern !f v@(VarList !xs) !i
  | i < 0 || i >= varListSize v = Nothing
  | otherwise =
    Just $!
    VarList $!
    let (!index, !offset) = i `divMod` 64
        !len = V.length xs
     in V.generate len $ \j ->
          if j == index
            then f (xs V.! j) offset
            else xs V.! j

{-# INLINE bitChangePattern #-}
bit64VectorBounded :: Int -> Int -> Vector Word64
bit64VectorBounded totalBits i =
  let fullWords = i `div` 64
      bits = i `mod` 64
      numWords = minWords $ toInteger totalBits
   in V.take numWords $
      V.concat
        [ V.replicate fullWords 0
        , V.singleton (bit bits)
        , V.replicate numWords 0
        ]

--------------------------------------
-- | VarList Conversion Operations |--
--------------------------------------
toBitList :: KnownNat n => VarList n -> [Bool]
toBitList v@(VarList xs) =
  let totalBits = varListSize v
      allBits = V.toList $ V.concatMap wordToBits xs
   in take totalBits allBits
  where
    wordToBits :: Word64 -> Vector Bool
    wordToBits w = V.fromList $ map (testBit w) [0 .. 63]

--------------------------------------------
-- | VarAssignment Conversion Operations |--
--------------------------------------------
varAssignmentToBoolAssignment ::
     forall n. KnownNat n
  => VarAssignment n
  -> BoolAssignment n
varAssignmentToBoolAssignment (VarAssignment vp vn) =
  BoolAssignment $ map (f vp vn) [0 .. varListSize vp - 1]
  where
    f p n i
      | testBit p i = Just True
      | testBit n i = Just False
      | otherwise = Nothing

createVarAssignment :: VarList n -> VarList n -> VarAssignment n
createVarAssignment = VarAssignment

------------------------------------------
-- | SatSolution Conversion Operations |--
------------------------------------------
satSolutionToBoolSatSolution ::
     forall n. KnownNat n
  => SatSolution n
  -> BoolSatSolution n
satSolutionToBoolSatSolution (Satisfiable v) = BoolSatSolution $ toBitList v
satSolutionToBoolSatSolution Unsatisfiable = BoolSatSolution []
