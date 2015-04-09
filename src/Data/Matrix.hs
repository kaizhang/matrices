module Data.Matrix
    ( Matrix
    , module Data.Matrix.Generic
    )where

import qualified Data.Matrix.Generic as MG
import Data.Matrix.Generic hiding (Matrix)
import qualified Data.Vector as V

type Matrix = MG.Matrix V.Vector
