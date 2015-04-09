module Data.Matrix.Mutable
    ( MMatrix
    , module Data.Matrix.Dense.Generic.Mutable
    ) where

import qualified Data.Matrix.Dense.Generic.Mutable as MG
import Data.Matrix.Dense.Generic.Mutable hiding (MMatrix)
import qualified Data.Vector.Mutable as VM

type MMatrix a = MG.MMatrix VM.MVector a
