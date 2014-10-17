module Data.Matrix.Storable
    ( Matrix
    , module Data.Matrix.Generic.Base
    ) where

import qualified Data.Matrix.Generic.Types as MG
import Data.Matrix.Generic.Base
import qualified Data.Vector.Storable as S

type Matrix a = MG.Matrix S.Vector a
