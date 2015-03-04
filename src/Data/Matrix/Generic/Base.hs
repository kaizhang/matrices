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
    , dim
    , (!)
    , unsafeIndex
    , empty

    -- * Conversions
    , matrix
    , flatten
    , fromVector
    , toRows
    , toColumns
    , fromRows
    , fromColumns
    , toList
    , toLists
    , fromLists

    -- * Different matrix types
    , convert

    , tr
    , takeRow
    , takeColumn
    , subMatrix
    , ident
    , diag
    , diagRect
    , takeDiag
    , fromBlocks
    , isSymmetric
    , force
    , Data.Matrix.Generic.Base.foldl

    -- * Mapping
    , imap
    , Data.Matrix.Generic.Base.map

    -- * Monadic mapping
    , Data.Matrix.Generic.Base.mapM
    , Data.Matrix.Generic.Base.mapM_
    , Data.Matrix.Generic.Base.forM
    , Data.Matrix.Generic.Base.forM_

    , generate

    ) where

import Control.Arrow ((***), (&&&))
import Control.Monad
import qualified Data.Foldable as F
import qualified Data.Vector.Generic as G
import qualified Data.Vector.Generic.Mutable as GM
import Data.Matrix.Generic.Types

rows :: Matrix v a -> Int
rows (Matrix m _ _ _ _) = m

cols :: Matrix v a -> Int
cols (Matrix _ n _ _ _) = n

dim :: Matrix v a -> (Int, Int)
dim (Matrix r c _ _ _) = (r,c)
{-# INLINE dim #-}

-- TODO: better error message
(!) :: Matrix v a -> (Int, Int) -> a
(!) (Matrix _ _ tda offset vec) (i, j) = vec G.! idx
  where
    idx = offset + i * tda + j
{-# INLINE (!) #-}

unsafeIndex :: Matrix v a -> (Int, Int) -> a
unsafeIndex (Matrix _ _ tda offset vec) (i,j) = vec `G.unsafeIndex` idx
  where
    idx = offset + i * tda + j
{-# INLINE unsafeIndex #-}


--reshape :: G.Vector v a => Matrix v a -> (Int, Int) -> Matrix v a

empty :: G.Vector v a => Matrix v a
empty = Matrix 0 0 0 0 G.empty
{-# INLINE empty #-}

matrix :: G.Vector v a => Int -> [a] -> Matrix v a
matrix ncol xs | n `mod` ncol /= 0 = error "incorrect length"
               | otherwise = fromVector (nrow,ncol) vec
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

fromVector :: G.Vector v a => (Int, Int) -> v a -> Matrix v a
fromVector (r,c) = Matrix r c c 0
{-# INLINE fromVector #-}

toList :: Matrix v a -> [a]
toList m@(Matrix _ _ _ _ _) = G.toList $ flatten m
{-# INLINE toList #-}

toRows :: Matrix v a -> [v a]
toRows (Matrix m n tda offset vec) = loop 0
  where
    loop !i | i < m = G.slice (f i) n vec : loop (i+1)
            | otherwise = []
    f i = offset + i * tda
{-# INLINE toRows #-}

toColumns :: Matrix v a -> [v a]
toColumns m = Prelude.map (takeColumn m) [0 .. c-1]
  where c = cols m
{-# INLINE toColumns #-}

fromRows :: G.Vector v a => [v a] -> Matrix v a
fromRows xs | any (\x -> G.length x /= c) xs = error "inequal length"
            | otherwise = fromVector (r,c) . G.concat $ xs
  where
    r = length xs
    c = G.length . head $ xs
{-# INLINE fromRows #-}

fromColumns :: G.Vector v a => [v a] -> Matrix v a
fromColumns = tr . fromRows
{-# INLINE fromColumns #-}

toLists :: Matrix v a -> [[a]]
toLists m@(Matrix _ _ _ _ _) = Prelude.map G.toList $ toRows m
{-# INLINE toLists #-}

-- | doesn't check if the list of list is a valid matrix
fromLists :: G.Vector v a => [[a]] -> Matrix v a
fromLists xs = fromVector (r,c) . G.fromList . concat $ xs
  where
    r = length xs
    c = length .head $ xs
{-# INLINE fromLists #-}

---- | construct upper triangular matrix from vector
--upperTriangular :: (Num a, G.Vector v a) => Int -> v a -> Matrix v a
--upperTriangular n vec =

-- | convert different matrix type
convert :: G.Vector w a => Matrix v a -> Matrix w a
convert (Matrix r c tda offset vec) = Matrix r c tda offset . G.convert $ vec
{-# INLINE convert #-}

takeRow :: Matrix v a -> Int -> v a
takeRow (Matrix _ c tda offset vec) i = G.slice i' c vec
  where
    i' = offset + i * tda
{-# INLINE takeRow #-}

takeColumn :: Matrix v a -> Int -> v a
takeColumn (Matrix r _ tda offset vec) j = G.create $ GM.new r >>= go idx vec r 0
  where
    go f vec' r' !i v | i >= r' = return v
                      | otherwise = do GM.unsafeWrite v i $ vec' G.! f i
                                       go f vec' r' (i+1) v
    idx i = offset + i * tda + j
{-# INLINE takeColumn #-}

-- | O(1) extract sub matrix
subMatrix :: (Int, Int)  -- ^ upper left corner of the submatrix
          -> (Int, Int)  -- ^ bottom right corner of the submatrix
          -> Matrix v a -> Matrix v a
subMatrix (i,j) (i',j') (Matrix _ n tda offset vec)
    | m' <= 0 || n' <= 0 = empty
    | otherwise = Matrix m' n' tda offset' vec
  where
    m' = i' - i + 1
    n' = j' - j + 1
    offset' = offset + i * n + j
{-# INLINE subMatrix #-}

tr :: Matrix v a -> Matrix v a
tr (Matrix r c tda offset vec) = fromVector (c,r) $ G.generate (r*c) f
  where
    f i = vec G.! (offset + i `mod` r * tda + i `div` r)
{-# INLINE tr #-}

ident :: (Num a, G.Vector v a) => Int -> Matrix v a
ident n = diagRect 0 (n,n) $ replicate n 1
{-# INLINE ident #-}

-- | create a square matrix with given diagonal, other entries default to 0
diag :: (Num a, G.Vector v a, F.Foldable t)
     => t a  -- ^ diagonal
     -> Matrix v a
diag d = diagRect 0 (n,n) d
  where n = length . F.toList $ d
{-# INLINE diag #-}

-- | create a rectangular matrix with default values and given diagonal
diagRect :: (G.Vector v a, F.Foldable t)
         => a         -- ^ default value
         -> (Int, Int)
         -> t a       -- ^ diagonal
         -> Matrix v a
diagRect z0 (r,c) d = fromVector (r,c) $ G.create $ GM.replicate n z0 >>= go d c
  where
    go xs c' v = F.foldlM f 0 xs >> return v
      where
        f !i x = GM.unsafeWrite v (i*(c'+1)) x >> return (i+1)
    n = r * c
{-# INLINE diagRect #-}

-- | extracts the diagonal from a rectangular matrix
takeDiag :: Matrix v a -> v a
takeDiag mat@(Matrix r c _ _ _) = G.generate n $ \i -> unsafeIndex mat (i,i)
  where
    n = min r c
{-# INLINE takeDiag #-}

fromBlocks :: G.Vector v a
           => a               -- ^ default value
           -> [[Matrix v a]]
           -> Matrix v a
fromBlocks d ms = fromVector (m,n) $ G.create $ GM.replicate (m*n) d >>= go n ms
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
                    step i u = do
                        GM.unsafeWrite v ((cr + i `div` c) * n' + i `mod` c + cc) u
                        return (i+1)
                G.foldM'_ step (0::Int) vec
                return (max maxR r, cc + c)
    -- figure out the dimension of the new matrix
    (m, n) = (sum *** maximum) . unzip . Prelude.map ((maximum *** sum) .
                unzip . Prelude.map (rows &&& cols)) $ ms
{-# INLINE fromBlocks #-}

isSymmetric :: Eq a => Matrix v a -> Bool
isSymmetric m@(Matrix r c _ _ _) | r /= c = False
                                 | otherwise = all f [0 .. r-1]
  where
    f i = all g [i + 1 .. c-1]
      where g j = m ! (i,j) == m ! (j,i)
{-# INLINE isSymmetric #-}

force :: Matrix v a -> Matrix v a
force m@(Matrix r c _ _ _) = fromVector (r,c) . G.force . flatten $ m
{-# INLINE force #-}

imap :: G.Vector v b => ((Int, Int) -> a -> b) -> Matrix v a -> Matrix v b
imap f m@(Matrix r c _ _ _) = fromVector (r,c) $ G.imap f' . flatten $ m
  where
    f' i = f (i `div` c, i `mod` c)
{-# INLINE imap #-}

map :: G.Vector v b => (a -> b) -> Matrix v a -> Matrix v b
map f m@(Matrix r c _ _ _) = fromVector (r,c) $ G.map f . flatten $ m
{-# INLINE map #-}

foldl :: (a -> b -> a) -> a -> Matrix v b -> a
foldl f acc m@(Matrix _ _ _ _ _) = G.foldl f acc . flatten $ m
{-# INLINE foldl #-}

mapM :: (G.Vector v b, Monad m) => (a -> m b) -> Matrix v a -> m (Matrix v b)
mapM f m@(Matrix r c _ _ _) = liftM (fromVector (r,c)) . G.mapM f . flatten $ m
{-# INLINE mapM #-}

mapM_ :: Monad m => (a -> m b) -> Matrix v a -> m ()
mapM_ f m@(Matrix _ _ _ _ _) = G.mapM_ f $ flatten m
{-# INLINE mapM_ #-}

forM :: (G.Vector v b, Monad m) => Matrix v a -> (a -> m b) -> m (Matrix v b)
forM = flip Data.Matrix.Generic.Base.mapM
{-# INLINE forM #-}

forM_ :: Monad m => Matrix v a -> (a -> m b) -> m ()
forM_ = flip Data.Matrix.Generic.Base.mapM_
{-# INLINE forM_ #-}

generate :: G.Vector v a => (Int, Int) -> ((Int, Int) -> a) -> Matrix v a
generate (r,c) f = fromVector (r,c) . G.generate (r*c) $ \i -> f (i `div` c, i `mod` c)
{-# INLINE generate #-}
