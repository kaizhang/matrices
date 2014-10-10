module Data.Matrix.Mutable
    ( MMatrix
    , module Data.Matrix.Generic.Mutable
    ) where

import qualified Data.Matrix.Generic.Types as MG
import Data.Matrix.Generic.Mutable
import qualified Data.Vector.Mutable as VM

type MMatrix a = MG.MMatrix VM.MVector a
