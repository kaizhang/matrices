{-# LANGUAGE GADTs #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverlappingInstances #-}
--------------------------------------------------------------------------------
-- |
-- Module      :  $Header$
-- Copyright   :  (c) 2014 Kai Zhang
-- License     :  BSD3

-- Maintainer  :  kai@kzhang.org
-- Stability   :  experimental
-- Portability :  portable

--
--------------------------------------------------------------------------------

module Data.Matrix.Generic.Types
    ( Matrix(..)
    , MMatrix(..)
    ) where

import Data.Binary
import qualified Data.Vector as V
import qualified Data.Vector.Unboxed as U
import qualified Data.Vector.Storable as S
import qualified Data.Vector.Binary ()
import qualified Data.Vector.Generic as G
import qualified Data.Vector.Generic.Mutable as GM

-- | row-major matrix supporting efficient slice
data Matrix v a where
    Matrix :: G.Vector v a
           => !Int    -- number of rows
           -> !Int    -- number of cols
           -> !Int    -- physical row dimension
           -> !Int    -- offset
           -> !(v a)  -- flat matrix
           -> Matrix v a

instance Binary a => Binary (Matrix V.Vector a) where
    put = putGeneric
    get = getGeneric

instance (U.Unbox a, Binary a) => Binary (Matrix U.Vector a) where
    put = putGeneric
    get = getGeneric

instance (S.Storable a, Binary a) => Binary (Matrix S.Vector a) where
    put = putGeneric
    get = getGeneric

getGeneric :: (Binary (v a), G.Vector v a) => Get (Matrix v a)
getGeneric = do
    r <- get
    c <- get
    tda <- get
    offset <- get
    vec <- get
    return $ Matrix r c tda offset vec

putGeneric :: (Binary (v a), G.Vector v a) => Matrix v a -> Put
putGeneric (Matrix r c tda offset vec) = do
    put r
    put c
    put tda
    put offset
    put vec

-- | mutable matrix
data MMatrix v m a where
    MMatrix :: GM.MVector v a
            => !Int
            -> !Int
            -> !Int
            -> !Int
            -> !(v m a)
            -> MMatrix v m a

instance (G.Vector v a, Show a) => Show (Matrix v a) where
    show mat = unlines . map (unwords . map show) . toLists $ mat

instance G.Vector v Bool => Show (Matrix v Bool) where
    show mat = unlines . map (unwords . map showBool) . toLists $ mat
      where
        showBool x = if x then "1" else "0"

toRows :: G.Vector v a => Matrix v a -> [v a]
toRows (Matrix m n tda offset vec) = loop 0
    where
    loop !i | i < m = G.slice (f i) n vec : loop (i+1)
            | otherwise = []
    f i = offset + i * tda

toLists :: G.Vector v a => Matrix v a -> [[a]]
toLists = map G.toList . toRows
