{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies #-}
module Data.Matrix.Symmetric
    ( SymMatrix(..)
    , dim
    , rows
    , cols
    , unsafeIndex
    , (!)
    , flatten
    , unsafeFromVector
    , fromVector
    , takeRow
    , thaw
    , unsafeThaw
    , freeze
    , unsafeFreeze
    , zip
    , zipWith
    ) where

import Prelude hiding (zip, zipWith)
import Control.Monad (liftM)
import Data.Bits (shiftR)
import Data.Binary
import qualified Data.Vector.Generic as G

import Data.Matrix.Generic
import Data.Matrix.Symmetric.Mutable (SymMMatrix(..))

type instance Mutable SymMatrix = SymMMatrix

-- | Symmetric square matrix
data SymMatrix v a = SymMatrix !Int !(v a)
    deriving (Show)

instance (G.Vector v a, Binary a) => Binary (SymMatrix v a) where
    put (SymMatrix n vec) = do put (52 :: Int)
                               put n
                               G.mapM_ put vec
    get = do
        magic <- get :: Get Int
        if magic == 52
           then do
               n <- get 
               vec <- G.replicateM ((n+1)*n `shiftR` 1) get
               return $ SymMatrix n vec
           else error "not a valid file"

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
idx n i j | i <= j = (i * (2 * n - i - 1)) `shiftR` 1 + j
          | otherwise = (j * (2 * n - j - 1)) `shiftR` 1 + i
{-# INLINE idx #-}

zip :: (G.Vector v a, G.Vector v b, G.Vector v (a,b))
    => SymMatrix v a -> SymMatrix v b -> SymMatrix v (a,b)
zip (SymMatrix n1 v1) (SymMatrix n2 v2)
    | n1 /= n2 = error "imcompatible size"
    | otherwise = SymMatrix n1 $ G.zip v1 v2
{-# INLINE zip #-}

zipWith :: (G.Vector v a, G.Vector v b, G.Vector v c)
        => (a -> b -> c) -> SymMatrix v a -> SymMatrix v b -> SymMatrix v c
zipWith f (SymMatrix n1 v1) (SymMatrix n2 v2)
    | n1 /= n2 = error "imcompatible size"
    | otherwise = SymMatrix n1 . G.zipWith f v1 $ v2
{-# INLINE zipWith #-}
