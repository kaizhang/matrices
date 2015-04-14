{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
module Data.Matrix.Symmetric
    ( SymMatrix(..)
    ) where

import Data.Bits (shiftR)
import qualified Data.Vector.Generic as G

import Data.Matrix.Generic

-- | Symmetric square matrix
data SymMatrix v a = SymMatrix !Int
                               !(v a)
    deriving (Show)

instance G.Vector v a => Matrix SymMatrix v a where
    dim (SymMatrix n _) = (n,n)
    {-# INLINE dim #-}

    unsafeIndex (SymMatrix n vec) (i,j) = vec `G.unsafeIndex` idx n i j
    {-# INLINE unsafeIndex #-}

    unsafeFromVector (r,c) vec = undefined
      where
        n = ((r+1)*r) `shiftR` 1
    {-# INLINE unsafeFromVector #-}

-- row major upper triangular indexing
idx :: Int -> Int -> Int -> Int
idx n i j | i <= j = (i * (2 * n - i - 3)) `shiftR` 1 + j - 1
          | otherwise = (j * (2 * n - j - 3)) `shiftR` 1 + i - 1
{-# INLINE idx #-}
