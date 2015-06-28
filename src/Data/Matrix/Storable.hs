module Data.Matrix.Storable
    ( Matrix
    , module Data.Matrix.Dense.Generic
    ) where

import           Data.Matrix.Dense.Generic hiding (Matrix)
import qualified Data.Matrix.Dense.Generic as MG
import qualified Data.Vector.Storable      as V

type Matrix a = MG.Matrix V.Vector a
