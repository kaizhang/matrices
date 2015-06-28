module Data.Matrix.Storable.Mutable
    ( MMatrix
    , module Data.Matrix.Dense.Generic.Mutable
    ) where

import           Data.Matrix.Dense.Generic.Mutable hiding (MMatrix)
import qualified Data.Matrix.Dense.Generic.Mutable as MG
import qualified Data.Vector.Storable.Mutable      as VM

type MMatrix a = MG.MMatrix VM.MVector a
