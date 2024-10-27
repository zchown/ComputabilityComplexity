{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

{-
 - This modules defines the types and helper function used to solve and
 - represent SAT problems. A SAT problem is just a list of clauses. A clause
 - is just two VarLists one for the positive variables in a clause and one for
 - the negative variables in a clause. A VarList is just a bitmask of size n
 - where n is the number of variables in the SAT problem. The VarList uses a
 - list of Word64 to store the bits. We also ensure that the VarList is always
 - using as few Word64 as possible by using the minWords function.
 -
 - This sounded like a cool solution to represent SAT problems although as 
 - you might be able to tell it is definitely a bit involved. Fortunately I was
 - able to cheat to an extent. Bit shifts and rotates are the hardest to 
 - implement, but they don't really make sense for this use case. Therefore
 - while I do have to provide a definition for them they both just do nothing.
 - My hope was that because we can do bitwise operation on the VarList we might
 - be able to gain  some performance by using this representation. Although I 
 - am not actually sure if this is the case in Haksell or not, I think it would
 - be in a language like C/C++/Rust. Inefficiency in the list is a potential 
 - concern. But that also depends on how many variables you have in your SAT 
 - problems. And probably would have been more of an issue if I had done this 
 - as a List of Ints as was suggested in class. An Array of Ints would be an 
 - improvement from that but thats just not as much fun. I don't know of a 
 - better way though to get an arbitrary length bit mask in Haskell. Maybe I 
 - should have used Zig where you can have arbitrary bit-width integers.
 -
 - One thing that is nice about doing this in Haskell though is that the types
 - can be very expressive and we can encode the number of variables into the
 - type system. This means that we can't accidentally try to do bitwise 
 - operations on two VarLists of different sizes. That is what all of the 
 - KnownNat constraints are for.
 -
 - I'm pretty sure you could actually just go about solving the SAT problems
 - using the type system entirely and never actually run the code... Haskell
 - is weird. If you use it wrong (or maybe right) its a dynamically typed 
 - interpreted language as the joke goes.
 -}
module SatTypes where

import Control.Monad (foldM)
import Data.Bits
import Data.Foldable (foldl', foldr')
import Data.Proxy
import Data.Vector.Unboxed (Vector)
import qualified Data.Vector.Unboxed as V
import Data.Word (Word64)
import GHC.TypeLits

--------------------------------
-- | Core SAT Problem Types | --
--------------------------------
newtype SatProblem (n :: Nat) =
  SatProblem [Clause n]
  deriving (Eq, Show)

newtype SatSolution (n :: Nat) =
  SatSolution (VarList n)
  deriving (Eq, Show)

newtype BoolSatSolution (n :: Nat) =
  BoolSatSolution [Bool]
  deriving (Eq, Show)

newtype BoolAssignment (n :: Nat) =
  BoolAssignment [Maybe Bool]
  deriving (Eq, Show)

newtype VarAssignment (n :: Nat) =
  VarAssignment (VarList n, VarList n)
  deriving (Eq, Show)

data Clause (n :: Nat) = Clause
  { positive :: VarList n
  , negative :: VarList n
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
evaluateSatProblem (SatProblem cs) v = all (evaluateClause v) cs

ratioSatProblem ::
     forall n. KnownNat n
  => SatProblem n
  -> VarAssignment n
  -> Double
ratioSatProblem (SatProblem cs) v =
  fromIntegral (length (filter (evaluateClause v) cs)) /
  fromIntegral (length cs)

satProblemFromList ::
     forall n. KnownNat n
  => [[Int]]
  -> Maybe (SatProblem n)
satProblemFromList xs = do
  clauses <- mapM clauseFromList xs
  return $ SatProblem clauses

--------------------------------
-- | Clause Type Operations | --
--------------------------------
instance KnownNat n => Show (Clause n) where
  show (Clause p n) =
    "Clause {positive = " ++
    show (toBitList p) ++ ", negative = " ++ show (toBitList n) ++ "}"

createClause ::
     forall n. KnownNat n
  => Clause n
createClause = Clause createVarList createVarList

isClauseEmpty ::
     forall n. KnownNat n
  => Clause n
  -> Bool
isClauseEmpty (Clause p n) = varListIsZero $ p .|. n

addClause :: Clause n -> SatProblem n -> SatProblem n
addClause c (SatProblem cs) = SatProblem (c : cs)

evaluateClause ::
     forall n. KnownNat n
  => VarAssignment n
  -> Clause n
  -> Bool
evaluateClause (VarAssignment (vp, vn)) (Clause p n) = not x || not y
  where
    x = varListIsZero $ vp .&. p
    y = varListIsZero $ vn .&. n

clauseFromList ::
     forall n. KnownNat n
  => [Int]
  -> Maybe (Clause n)
clauseFromList xs = do
  let maxVar = varListSize (undefined :: VarList n) - 1
      (p, n) = foldl' categorize ([], []) xs
      valid = all (\x -> (abs x - 1) <= maxVar) xs
  if not valid
    then Nothing
    else do
      let clause = createClause @n
      c' <- foldM (\c x -> addPositiveVar c (x - 1)) clause p
      foldM (\c x -> addNegativeVar c (abs x - 1)) c' n
  where
    categorize (p, n) x
      | x > 0 = (x : p, n)
      | x < 0 = (p, x : n)
      | otherwise = (p, n)

unsafeClauseFromList ::
     forall n. KnownNat n
  => [Int]
  -> Clause n
unsafeClauseFromList xs =
  case clauseFromList xs of
    Just c -> c
    Nothing -> error "Invalid clause"

------------------------------------
-- | Clause Variable Operations | --
------------------------------------
addPositiveVar ::
     forall n. KnownNat n
  => Clause n
  -> Int
  -> Maybe (Clause n)
addPositiveVar (Clause p n) i = do
  p' <- SatTypes.setBit p i
  return $ Clause p' n

addNegativeVar ::
     forall n. KnownNat n
  => Clause n
  -> Int
  -> Maybe (Clause n)
addNegativeVar (Clause p n) i = do
  n' <- SatTypes.setBit n i
  return $ Clause p n'

removePositiveVar ::
     forall n. KnownNat n
  => Clause n
  -> Int
  -> Maybe (Clause n)
removePositiveVar (Clause p n) i = do
  p' <- SatTypes.clearBit p i
  return $ Clause p' n

removeNegativeVar ::
     forall n. KnownNat n
  => Clause n
  -> Int
  -> Maybe (Clause n)
removeNegativeVar (Clause p n) i = do
  n' <- SatTypes.clearBit n i
  return $ Clause p n'

isPositiveVar ::
     forall n. KnownNat n
  => Clause n
  -> Int
  -> Bool
isPositiveVar (Clause p _) = testBit p

isNegativeVar ::
     forall n. KnownNat n
  => Clause n
  -> Int
  -> Bool
isNegativeVar (Clause _ n) = testBit n

getPositiveVars ::
     forall n. KnownNat n
  => Clause n
  -> [Int]
getPositiveVars c@(Clause p _) =
  [i | i <- [0 .. varListSize p - 1], isPositiveVar c i]

getNegativeVars ::
     forall n. KnownNat n
  => Clause n
  -> [Int]
getNegativeVars c@(Clause _ n) =
  [i | i <- [0 .. varListSize n - 1], isNegativeVar c i]

-------------------------------------
-- | VarList Type Core Operations |--
-------------------------------------
instance KnownNat n => Show (VarList n) where
  show v = "VarList " ++ show (toBitList v)

minWords :: Integer -> Int
minWords n = fromInteger ((n + 63) `div` 64)

createVarList ::
     forall n. KnownNat n
  => VarList n
createVarList =
  let n = natVal (Proxy @n)
   in VarList $ V.replicate (minWords n) 0

varListFromList ::
     forall n. KnownNat n
  => [Bool]
  -> VarList n
varListFromList bs =
  let n = natVal (Proxy @n)
      m = minWords n
      lastWordBits = fromInteger $ n `mod` 64
      valid = (length bs == fromInteger n)
   in if valid
        then VarList $
             V.generate m $ \i ->
               let start = i * 64
                   end = min (start + 64) (fromInteger n)
                   bits = take (end - start) $ drop start bs
                in foldr'
                     (\b w ->
                        w `shiftL` 1 .|.
                        if b
                          then 1
                          else 0)
                     0
                     bits
        else error "Invalid list length"

validateVarList ::
     forall n. KnownNat n
  => Vector Word64
  -> Maybe (VarList n)
validateVarList ws =
  let n = natVal (Proxy @n)
      m = minWords n
      lastWordBits = fromInteger $ n `mod` 64
      valid =
        not (V.null ws) &&
        (lastWordBits == 0 ||
         (V.last ws .&. complement (bit lastWordBits - 1)) == 0)
   in if V.length ws == m && valid
        then Just $ VarList ws
        else Nothing

varListSize ::
     forall n. KnownNat n
  => VarList n
  -> Int
varListSize _ = fromInteger $ natVal (Proxy @n)

varListIsZero :: VarList n -> Bool
varListIsZero (VarList xs) = 0 == V.foldr' (.|.) 0 xs

--------------------------------
-- | VarList Bit Operations | --
--------------------------------
instance KnownNat n => Bits (VarList n) where
  VarList xs .&. VarList ys = VarList $ V.zipWith (.&.) xs ys
  VarList xs .|. VarList ys = VarList $ V.zipWith (.|.) xs ys
  xor (VarList xs) (VarList ys) = VarList $ V.zipWith xor xs ys
  complement (VarList xs) =
    let n = natVal (Proxy @n)
        lastWordBits = fromInteger $ n `mod` 64
        maskLastWord w =
          if lastWordBits == 0
            then complement w
            else complement w .&. (bit lastWordBits - 1)
        result =
          if V.null xs
            then V.empty
            else V.generate len $ \i ->
                   if i < len - 1
                     then complement (xs V.! i)
                     else maskLastWord (xs V.! i)
          where
            len = V.length xs
     in VarList result
  shift v _ = v
  rotate v _ = v
  bitSize = varListSize
  bitSizeMaybe = Just . varListSize
  isSigned _ = False
  testBit (VarList xs) i
    | i < 0 || i >= varListSize (undefined :: VarList n) = False
    | otherwise = testBit (xs V.! (i `div` 64)) (i `mod` 64)
  bit i
    | i < 0 || i >= varListSize (undefined :: VarList n) = createVarList @n
    | otherwise =
      VarList $ bit64VectorBounded (varListSize (undefined :: VarList n)) i
  popCount (VarList xs) = V.sum $ V.map popCount xs

setBit ::
     forall n. KnownNat n
  => VarList n
  -> Int
  -> Maybe (VarList n)
setBit = bitChangePattern Data.Bits.setBit

clearBit ::
     forall n. KnownNat n
  => VarList n
  -> Int
  -> Maybe (VarList n)
clearBit = bitChangePattern Data.Bits.clearBit

flipBit ::
     forall n. KnownNat n
  => VarList n
  -> Int
  -> Maybe (VarList n)
flipBit = bitChangePattern Data.Bits.complementBit

bitChangePattern ::
     forall n. KnownNat n
  => (Word64 -> Int -> Word64)
  -> VarList n
  -> Int
  -> Maybe (VarList n)
bitChangePattern f v@(VarList xs) i
  | i < 0 || i >= varListSize v = Nothing
  | otherwise =
    Just $
    VarList $
    let (index, offset) = i `divMod` 64
        len = V.length xs
     in V.generate len $ \j ->
          if j == index
            then f (xs V.! j) offset
            else xs V.! j

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
    -- recursiveRotateRight :: Int -> Word64 -> VarList n
    -- recursiveRotateRight i carry

--
-- rotateVarList ::
--      forall n. KnownNat n
--   => VarList n
--   -> Int
--   -> VarList n
-- rotateVarList v 0 = v
-- rotateVarList v i
--   | i < 0 = rotateVarListLeft v (-i)
--   | otherwise = rotateVarListRight v i
--
-- rotateVarListRight ::
--      forall n. KnownNat n
--   => VarList n
--   -> Int
--   -> VarList n
-- rotateVarListRight v@(VarList xs) i =
--   let (fullWords, bits) = i `divMod` 64
--       bitmask = ((2 ^ bits) - 1) :: Word64
--       len = V.length xs
--    in (V.map) V.++ recursiveRotateRight fullWords 0
--   where
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
varAssignmentToBoolAssignment (VarAssignment (vp, vn)) =
  BoolAssignment $ map (f vp vn) [0 .. varListSize vp - 1]
  where
    f p n i
      | testBit p i = Just True
      | testBit n i = Just False
      | otherwise = Nothing

createVarAssignment ::
     forall n. KnownNat n
  => VarList n
  -> VarList n
  -> VarAssignment n
createVarAssignment = curry VarAssignment

------------------------------------------
-- | SatSolution Conversion Operations |--
------------------------------------------
satSolutionToBoolSatSolution ::
     forall n. KnownNat n
  => SatSolution n
  -> BoolSatSolution n
satSolutionToBoolSatSolution (SatSolution v) = BoolSatSolution $ toBitList v
