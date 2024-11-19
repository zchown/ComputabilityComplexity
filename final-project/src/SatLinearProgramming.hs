{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE Haskell98 #-}

module SatLinearProgramming where

import Control.Monad
import Control.Monad.LPMonad
import Data.Bits
import Data.LinearProgram
import Data.LinearProgram.Common
import Data.LinearProgram.GLPK
import Data.LinearProgram.LinExpr
import qualified Data.Map as M
import qualified Data.Vector as V
import GHC.TypeLits
import SatTypes

-- randomizedRounding ::
--      forall n. KnownNat n
--   => SatProblem n
--   -> SatSolution n
-- randomizedRounding p = 
-- integerProgrammingSat ::
--      forall n. KnownNat n
--   => SAtProblem n
--   -> SatSolution n
-- integerProgrammingSat p =
--   let lp = toLinearProgram p
--       solution = glpSolveVars mipOpts lp
--    in case solution of
--         (Success, Just (opt, vars)) ->
--           let n = fromIntegral $ natVal (Proxy :: Proxy n)
--               varAssign = V.fromList $ map (round . snd) vars
--            in Satisfiable $ VarAssignment varAssign (complement varAssign)
--         _ -> Unsatisfiable
---------------------
-- | Solver Opts | --
---------------------
simplexOpts :: GLPKOpts
simplexOpts = simplexDefaults {msgLev = msgAll}

mipOpts :: GLPKOpts
mipOpts = mipDefaults {msgLev = msgAll}

---------------------
-- | Conversions | --
---------------------
toLinearProgram ::
     forall n. KnownNat n
  => SatProblem n
  -> LP (VarList n) Double
toLinearProgram p@(SatProblem cs) =
  execLPM $ do
    setDirection Max
    setObjective (objFun p)
    setVarBounds (varBounds p)
    setVarTypes (varTypes p)
    mapM_ addConstraint (toConstraints p)

objFun ::
     forall n. KnownNat n
  => SatProblem n
  -> ObjectiveFunc (VarList n) Double
objFun p@(SatProblem cs) =
  let n = fromIntegral $ natVal (Proxy :: Proxy n)
      vars = [show i | i <- [1 .. n]]
      coeffs = replicate n 1
   in linCombination (zip coeffs vars)

varBounds ::
     forall n. KnownNat n
  => SatProblem n
  -> VarBounds (VarList n) Double
varBounds p@(SatProblem cs) = M.fromList $ zip vars (repeat (0, 1))
  where
    n = fromIntegral $ natVal (Proxy :: Proxy n)
    vars = [show i | i <- [1 .. n]]

varTypes ::
     forall n. KnownNat n
  => SatProblem n
  -> VarTypes (VarList n)
varTypes p@(SatProblem cs) = M.fromList $ zip vars (repeat ContVar)
  where
    n = fromIntegral $ natVal (Proxy :: Proxy n)
    vars = [show i | i <- [1 .. n]]

toConstraints ::
     forall n. KnownNat n
  => SatProblem n
  -> [Constraint (VarList n) Double]
toConstraints p@(SatProblem cs) = map toConstraint cs
  where
    toConstraint :: Clause n -> Constraint (VarList n) Double
    toConstraint c =
      let n = fromIntegral $ natVal (Proxy :: Proxy n)
          vars = [show i | i <- [1 .. n]]
          coeffs =
            [ if lit > 0
              then 1
              else -1
            | l <- ls
            ]
          expr = linCombination (zip coeffs vars)
       in expr :==: 1
      where
        ls = clauseToList c
