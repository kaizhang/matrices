module Data.Matrix.Storable
    ( Matrix
    , module Data.Matrix.Generic
    ) where

import qualified Data.Matrix.Generic as MG
import Data.Matrix.Generic hiding (Matrix)
import qualified Data.Vector.Storable as V

type Matrix a = MG.Matrix V.Vector a
