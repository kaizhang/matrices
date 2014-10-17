{-# LANGUAGE GADTs #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE FlexibleContexts #-}
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
module Data.Matrix.Generic.Base
    ( rows
    , cols
    , (!)
    , unsafeIndex
    , matrix
    , flatten
    , fromVector
    , toRows
    , fromRows
    , toList
    , toLists
    , fromLists
    , tr
    , subMatrix
    , ident
    , diag
    , diagRect
    , fromBlocks
    , isSymmetric
    ) where

import Control.Arrow ((***), (&&&))
import Control.Monad
import qualified Data.Foldable as F
import qualified Data.Vector.Generic as G
import qualified Data.Vector.Generic.Mutable as GM
import Data.Matrix.Generic.Types

rows :: G.Vector v a => Matrix v a -> Int
rows (Matrix m _ _ _ _) = m

cols :: G.Vector v a => Matrix v a -> Int
cols (Matrix _ n _ _ _) = n

(!) :: G.Vector v a => Matrix v a -> (Int, Int) -> a
(!) (Matrix _ _ tda offset vec) (i, j) = vec G.! idx
  where
    idx = offset + i * tda + j
{-# INLINE (!) #-}

unsafeIndex :: G.Vector v a => Matrix v a -> (Int, Int) -> a
unsafeIndex (Matrix _ _ tda offset vec) (i,j) = vec `G.unsafeIndex` idx
  where
    idx = offset + i * tda + j
{-# INLINE unsafeIndex #-}

matrix :: G.Vector v a => Int -> [a] -> Matrix v a
matrix ncol xs | n `mod` ncol /= 0 = error "incorrect length"
               | otherwise = fromVector nrow ncol vec
  where
    vec = G.fromList xs
    nrow = n `div` ncol
    n = G.length vec
{-# INLINE matrix #-}

flatten :: Matrix v a -> v a
flatten (Matrix m n tda offset vec)
    | n == tda = G.slice offset (m * n) vec
    | otherwise = G.generate (m * n) f
  where
    f i = (G.!) vec $ offset + (i `div` n) * tda + i `mod` n
{-# INLINE flatten #-}

fromVector :: G.Vector v a => Int -> Int -> v a -> Matrix v a
fromVector r c = Matrix r c c 0
{-# INLINE fromVector #-}

toList :: G.Vector v a => Matrix v a -> [a]
toList = G.toList . flatten
{-# INLINE toList #-}

toRows :: G.Vector v a => Matrix v a -> [v a]
toRows (Matrix m n tda offset vec) = loop 0
  where
    loop !i | i < m = G.slice (f i) n vec : loop (i+1)
            | otherwise = []
    f i = offset + i * tda
{-# INLINE toRows #-}

fromRows :: G.Vector v a => [v a] -> Matrix v a
fromRows xs = fromVector r c . G.concat $ xs
  where
    r = length xs
    c = G.length . head $ xs
{-# INLINE fromRows #-}

toLists :: G.Vector v a => Matrix v a -> [[a]]
toLists = map G.toList . toRows
{-# INLINE toLists #-}

-- | doesn't check if the list of list is a valid matrix
fromLists :: G.Vector v a => [[a]] -> Matrix v a
fromLists xs = fromVector r c . G.fromList . concat $ xs
  where
    r = length xs
    c = length .head $ xs
{-# INLINE fromLists #-}

subMatrix :: G.Vector v a => (Int, Int) -> (Int, Int) -> Matrix v a -> Matrix v a
subMatrix (ri, rj) (ci, cj) (Matrix _ n tda offset vec) =
    Matrix m' n' tda offset' vec
  where
    m' = rj - ri + 1
    n' = cj - ci + 1
    offset' = offset + ri * n + ci
{-# INLINE subMatrix #-}

tr :: G.Vector v a => Matrix v a -> Matrix v a
tr (Matrix r c tda offset vec) = fromVector c r $ G.generate (r*c) f
  where
    f i = vec G.! (offset + i `mod` r * tda + i `div` r)
{-# INLINE tr #-}

ident :: (Num a, G.Vector v a) => Int -> Matrix v a
ident n = diagRect 0 n n $ replicate n 1
{-# INLINE ident #-}

-- | create a square matrix with given diagonal, other entries default to 0
diag :: (Num a, G.Vector v a, F.Foldable t)
     => t a  -- ^ diagonal
     -> Matrix v a
diag d = diagRect 0 n n d
  where n = length . F.toList $ d
{-# INLINE diag #-}

-- | create a rectangular matrix with default values and given diagonal
diagRect :: (G.Vector v a, F.Foldable t)
         => a         -- ^ default value
         -> Int       -- ^ number of rows
         -> Int       -- ^ number of columns
         -> t a       -- ^ diagonal
         -> Matrix v a
diagRect z0 r c d = fromVector r c $ G.create $ GM.replicate n z0 >>= go d c
  where
    go xs c' v = F.foldlM f 0 xs >> return v
      where
        f !i x = GM.unsafeWrite v (i*(c'+1)) x >> return (i+1)
    n = r * c
{-# INLINE diagRect #-}

fromBlocks :: G.Vector v a
           => a               -- ^ default value
           -> [[Matrix v a]]
           -> Matrix v a
fromBlocks d ms = fromVector m n $ G.create $ GM.replicate (m*n) d >>= go n ms
  where
    go n' xss v = foldM_ f 0 xss >> return v
      where
        f !cr xs = do (r', _) <- foldM g (0, 0) xs
                      return $ cr + r'
          where
            g (!maxR, !cc) x = do
                let c = cols x
                    r = rows x
                    vec = flatten x
                    step i u = do GM.unsafeWrite v ((cr + i `div` c) * n' + i `mod` c + cc) u
                                  return (i+1)
                G.foldM'_ step (0::Int) vec
                return (max maxR r, cc + c)
    -- figure out the dimension of the new matrix
    (m, n) = (sum *** maximum) . unzip . map ((maximum *** sum) . unzip . map (rows &&& cols)) $ ms
{-# INLINE fromBlocks #-}

isSymmetric :: (Eq a, G.Vector v a) => Matrix v a -> Bool
isSymmetric m@(Matrix r c _ _ _) | r /= c = False
                                 | otherwise = all f [0 .. r-1]
  where
    f i = all g [i + 1 .. c-1]
      where g j = m ! (i,j) == m ! (j,i)
{-# INLINE isSymmetric #-}
