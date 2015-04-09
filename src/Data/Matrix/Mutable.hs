module Data.Matrix.Mutable
    ( MMatrix
    , module Data.Matrix.Generic.Mutable
    ) where

import qualified Data.Matrix.Generic.Mutable as MG
import Data.Matrix.Generic.Mutable hiding (MMatrix)
import qualified Data.Vector.Mutable as VM

type MMatrix a = MG.MMatrix VM.MVector a
