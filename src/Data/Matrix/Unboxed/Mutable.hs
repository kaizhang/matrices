module Data.Matrix.Unboxed.Mutable
    ( MMatrix
    , module Data.Matrix.Generic.Mutable
    ) where

import qualified Data.Matrix.Generic.Types as MG
import Data.Matrix.Generic.Mutable
import qualified Data.Vector.Unboxed.Mutable as UM

type MMatrix a = MG.MMatrix UM.MVector a
