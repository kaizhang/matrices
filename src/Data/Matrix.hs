module Data.Matrix
    ( Matrix
    , module Data.Matrix.Dense.Generic
    )where

import qualified Data.Matrix.Dense.Generic as MG
import Data.Matrix.Dense.Generic hiding (Matrix)
import qualified Data.Vector as V

type Matrix = MG.Matrix V.Vector
