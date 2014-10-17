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
