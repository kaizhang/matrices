{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeFamilies #-}
module Data.Matrix.Symmetric
    ( SymMatrix(..)
    ) where

import Control.Monad (liftM)
import Data.Bits (shiftR)
import qualified Data.Vector.Generic as G

import Data.Matrix.Generic
import Data.Matrix.Symmetric.Mutable (SymMMatrix(..))

-- | Symmetric square matrix
data SymMatrix v a = SymMatrix !Int !(v a)
    deriving (Show)

type instance Mutable SymMatrix = SymMMatrix

instance G.Vector v a => Matrix SymMatrix v a where
    dim (SymMatrix n _) = (n,n)
    {-# INLINE dim #-}

    unsafeIndex (SymMatrix n vec) (i,j) = vec `G.unsafeIndex` idx n i j
    {-# INLINE unsafeIndex #-}

    unsafeFromVector (r,c) vec | r /= c = error "columns /= rows"
                               | otherwise = SymMatrix r . G.concat . map f $ [0..r-1]
      where
        f i = G.slice (i*(c+1)) (c-i) vec
--        n = ((r+1)*r) `shiftR` 1
    {-# INLINE unsafeFromVector #-}

    thaw (SymMatrix n v) = SymMMatrix n `liftM` G.thaw v
    {-# INLINE thaw #-}

    unsafeThaw (SymMatrix n v) = SymMMatrix n `liftM` G.thaw v
    {-# INLINE unsafeThaw #-}

    freeze (SymMMatrix n v) = SymMatrix n `liftM` G.freeze v
    {-# INLINE freeze #-}

    unsafeFreeze (SymMMatrix n v) = SymMatrix n `liftM` G.unsafeFreeze v
    {-# INLINE unsafeFreeze #-}

-- row major upper triangular indexing
idx :: Int -> Int -> Int -> Int
idx n i j | i <= j = (i * (2 * n - i - 3)) `shiftR` 1 + j - 1
          | otherwise = (j * (2 * n - j - 3)) `shiftR` 1 + i - 1
{-# INLINE idx #-}
