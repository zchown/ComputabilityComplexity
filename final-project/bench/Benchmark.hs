{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Main where

import Control.DeepSeq
import Control.Monad (replicateM)
import Criterion.Main
import Criterion.Types
import Data.Bits
import SatSolvers
import SatTypes
import System.Random
