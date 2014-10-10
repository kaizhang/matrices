module Data.Matrix
    ( Matrix
    , module Data.Matrix.Generic.Base
    ) where

import qualified Data.Matrix.Generic.Types as MG
import Data.Matrix.Generic.Base
import qualified Data.Vector as V

type Matrix a = MG.Matrix V.Vector a
