{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE BangPatterns #-}
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
    , Data.Matrix.Symmetric.map
    , imap
    , zip
    , zipWith
    ) where

import Prelude hiding (zip, zipWith)
import Control.Monad (liftM, guard)
import Data.Bits (shiftR)
import Data.Binary (Binary(..), Get, Put, Word32)
import Data.Binary.IEEE754 (getFloat64le, putFloat64le)
import Data.Binary.Get (getWord64le, getWord32le)
import Data.Binary.Put (putWord64le, putWord32le)
import qualified Data.Vector.Generic as G

import Data.Matrix.Generic
import Data.Matrix.Symmetric.Mutable (SymMMatrix(..), unsafeWrite, new)

type instance Mutable SymMatrix = SymMMatrix

-- | Symmetric square matrix
data SymMatrix v a = SymMatrix !Int !(v a)
    deriving (Show)


--------------------------------------------------------------------------------
-- Instances
--------------------------------------------------------------------------------

instance G.Vector v a => Matrix SymMatrix v a where
    dim (SymMatrix n _) = (n,n)
    {-# INLINE dim #-}

    unsafeIndex (SymMatrix n vec) (i,j) = vec `G.unsafeIndex` idx n i j
    {-# INLINE unsafeIndex #-}

    unsafeFromVector (r,c) vec | r /= c = error "columns /= rows"
                               | otherwise = SymMatrix r . G.concat . Prelude.map f $ [0..r-1]
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

magic :: Word32
magic = 0x33D31A66

instance (G.Vector v a, Binary a) => Binary (SymMatrix v a) where
    put = putMatrix put
    get = getMatrix get

instance G.Vector v Double => Binary (SymMatrix v Double) where
    put = putMatrix putFloat64le
    get = getMatrix getFloat64le

instance G.Vector v Int => Binary (SymMatrix v Int) where
    put = putMatrix $ putWord64le . fromIntegral
    get = getMatrix $ fromIntegral <$> getWord64le

putMatrix :: G.Vector v a => (a -> Put) -> SymMatrix v a -> Put
putMatrix putElement (SymMatrix n vec) = do
    putWord32le magic
    putWord64le . fromIntegral $ n
    G.mapM_ putElement vec
{-# INLINE putMatrix #-}

getMatrix :: G.Vector v a => Get a -> Get (SymMatrix v a)
getMatrix getElement = do
    m <- getWord32le
    guard $ m == magic
    n <- fromIntegral <$> getWord64le
    vec <- G.replicateM (((n+1)*n) `shiftR` 1) getElement
    return $ SymMatrix n vec
{-# INLINE getMatrix #-}

--------------------------------------------------------------------------------

map :: (G.Vector v a, G.Vector v b) => (a -> b) -> SymMatrix v a -> SymMatrix v b
map f (SymMatrix n vec) = SymMatrix n $ G.map f vec
{-# INLINE map #-}

-- | Upper triangular imap, i.e., i <= j
imap :: (G.Vector v a, G.Vector v b) => ((Int, Int) -> a -> b) -> SymMatrix v a -> SymMatrix v b
imap f mat = create $ do
    mat' <- new (n,n)
    let loop m !i !j
            | i >= n = return ()
            | j >= n = loop m (i+1) (i+1)
            | otherwise = unsafeWrite m (i,j) (f (i,j) x) >>
                          loop m i (j+1)
          where
            x = unsafeIndex mat (i,j)
    loop mat' 0 0
    return mat'
  where
    n = rows mat
{-# INLINE imap #-}

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


-- helper

-- row major upper triangular indexing
idx :: Int -> Int -> Int -> Int
idx n i j | i <= j = (i * (2 * n - i - 1)) `shiftR` 1 + j
          | otherwise = (j * (2 * n - j - 1)) `shiftR` 1 + i
{-# INLINE idx #-}
