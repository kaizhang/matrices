module Data.Matrix.Unboxed
    ( Matrix
    , module Data.Matrix.Generic
    ) where

import qualified Data.Matrix.Generic as MG
import Data.Matrix.Generic hiding (Matrix)
import qualified Data.Vector.Unboxed as V

type Matrix a = MG.Matrix V.Vector a
