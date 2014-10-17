module Data.Matrix.Storable.Mutable
    ( MMatrix
    , module Data.Matrix.Generic.Mutable
    ) where

import qualified Data.Matrix.Generic.Types as MG
import Data.Matrix.Generic.Mutable
import qualified Data.Vector.Storable.Mutable as SM

type MMatrix a = MG.MMatrix SM.MVector a
