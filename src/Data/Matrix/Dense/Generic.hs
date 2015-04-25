{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
module Data.Matrix.Dense.Generic
    ( 
    -- * Immutable Matrix
      Matrix(..)

    -- * Accessors
    -- ** length information
    , MG.dim
    , MG.rows
    , MG.cols

    -- ** Indexing
    , MG.unsafeIndex
    , (MG.!)
    , MG.takeRow
    , MG.takeColumn
    , MG.takeDiag

    -- * Construction
    , MG.unsafeFromVector
    , MG.fromVector
    , MG.matrix
    , MG.fromLists
    , MG.fromRows
    , fromColumns
    , MG.empty

    -- * Conversions
    , MG.flatten
    , MG.toRows
    , MG.toColumns
    , MG.toList
    , MG.toLists

    -- * Different matrix types
    , convert

    , tr
    , subMatrix
    , ident
    , diag
    , diagRect
    , fromBlocks
    , isSymmetric
    , force
    
    , Data.Matrix.Dense.Generic.foldl

    -- * Mapping
    , imap
    , Data.Matrix.Dense.Generic.map
    -- * Monadic mapping
    , mapM
    , mapM_
    , forM
    , forM_

    -- * Monadic sequencing
    , Data.Matrix.Dense.Generic.sequence
    , Data.Matrix.Dense.Generic.sequence_

    , generate

    -- * Mutable matrix
    , MG.thaw
    , MG.unsafeThaw
    , MG.freeze
    , MG.unsafeFreeze
    , MG.create
    ) where

import Prelude hiding (mapM_, mapM)
import qualified Data.Vector.Generic as G
import Control.Arrow ((***), (&&&))
import Control.Monad (liftM, foldM, foldM_)
import qualified Data.Foldable as F
import qualified Data.Vector.Generic.Mutable as GM

import qualified Data.Matrix.Generic as MG
import Data.Matrix.Dense.Generic.Mutable (MMatrix(..))

type instance MG.Mutable Matrix = MMatrix

-- | row-major matrix supporting efficient slice
data Matrix v a = Matrix !Int    -- number of rows
                         !Int    -- number of cols
                         !Int    -- physical row dimension
                         !Int    -- offset
                         !(v a)  -- flat matrix
    deriving (Show)


instance G.Vector v a => MG.Matrix Matrix v a where
    -- | O(1) Return the size of matrix.
    dim (Matrix r c _ _ _) = (r,c)
    {-# INLINE dim #-}

    -- | O(1) Unsafe indexing without bound check.
    unsafeIndex (Matrix _ _ tda offset vec) (i,j) = vec `G.unsafeIndex` idx
      where
        idx = offset + i * tda + j
    {-# INLINE unsafeIndex #-}

    -- | O(1) Create matrix from vector.
    unsafeFromVector (r,c) = Matrix r c c 0
    {-# INLINE unsafeFromVector #-}

    -- | O(1) Extract a row.
    unsafeTakeRow (Matrix _ c tda offset vec) i = G.slice i' c vec
      where
        i' = offset + i * tda
    {-# INLINE unsafeTakeRow #-}

    -- | Create a vector by concatenating rows.
    flatten (Matrix r c tda offset vec)
        | c == tda = G.slice offset (r*c) vec
        | otherwise = G.generate (r*c) $ \i ->
            vec `G.unsafeIndex` (offset + (i `div` c) * tda + (i `mod` c))
    {-# INLINE flatten #-}

    thaw (Matrix r c tda offset v) = MMatrix r c tda offset `liftM` G.thaw v
    {-# INLINE thaw #-}

    unsafeThaw (Matrix r c tda offset v) = MMatrix r c tda offset `liftM` G.unsafeThaw v
    {-# INLINE unsafeThaw #-}

    freeze (MMatrix r c tda offset v) = Matrix r c tda offset `liftM` G.freeze v
    {-# INLINE freeze #-}

    unsafeFreeze (MMatrix r c tda offset v) = Matrix r c tda offset `liftM` G.unsafeFreeze v
    {-# INLINE unsafeFreeze #-}

--reshape :: G.Vector v a => Matrix v a -> (Int, Int) -> Matrix v a

-- | O(m*n) Create matrix from columns
fromColumns :: G.Vector v a => [v a] -> Matrix v a
fromColumns = tr . MG.fromRows
{-# INLINE fromColumns #-}

---- | construct upper triangular matrix from vector
--upperTriangular :: (Num a, G.Vector v a) => Int -> v a -> Matrix v a
--upperTriangular n vec =

-- | O(m*n) Convert different matrix type
convert :: (G.Vector v a, G.Vector w a) => Matrix v a -> Matrix w a
convert (Matrix r c tda offset vec) = Matrix r c tda offset . G.convert $ vec
{-# INLINE convert #-}

-- | O(1) Extract sub matrix
subMatrix :: G.Vector v a
          => (Int, Int)  -- ^ upper left corner of the submatrix
          -> (Int, Int)  -- ^ bottom right corner of the submatrix
          -> Matrix v a -> Matrix v a
subMatrix (i,j) (i',j') (Matrix _ n tda offset vec)
    | m' <= 0 || n' <= 0 = MG.empty
    | otherwise = Matrix m' n' tda offset' vec
  where
    m' = i' - i + 1
    n' = j' - j + 1
    offset' = offset + i * n + j
{-# INLINE subMatrix #-}

-- | O(m*n) Matrix transpose
tr :: G.Vector v a => Matrix v a -> Matrix v a
tr (Matrix r c tda offset vec) = MG.fromVector (c,r) $ G.generate (r*c) f
  where
    f i = vec G.! (offset + i `mod` r * tda + i `div` r)
{-# INLINE tr #-}

-- | O(m*n) Create an identity matrix
ident :: (Num a, G.Vector v a) => Int -> Matrix v a
ident n = diagRect 0 (n,n) $ replicate n 1
{-# INLINE ident #-}

-- | O(m*n) Create a square matrix with given diagonal, other entries default to 0
diag :: (Num a, G.Vector v a, F.Foldable t)
     => t a  -- ^ diagonal
     -> Matrix v a
diag d = diagRect 0 (n,n) d
  where n = length . F.toList $ d
{-# INLINE diag #-}

-- | O(m*n) Create a rectangular matrix with default values and given diagonal
diagRect :: (G.Vector v a, F.Foldable t)
         => a         -- ^ default value
         -> (Int, Int)
         -> t a       -- ^ diagonal
         -> Matrix v a
diagRect z0 (r,c) d = MG.fromVector (r,c) $ G.create $ GM.replicate n z0 >>= go d c
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
fromBlocks d ms = MG.fromVector (m,n) $ G.create $ GM.replicate (m*n) d >>= go n ms
  where
    go n' xss v = foldM_ f 0 xss >> return v
      where
        f !cr xs = do (r', _) <- foldM g (0, 0) xs
                      return $ cr + r'
          where
            g (!maxR, !cc) x = do
                let (r,c) = MG.dim x
                    vec = MG.flatten x
                    step i u = do
                        GM.unsafeWrite v ((cr + i `div` c) * n' + i `mod` c + cc) u
                        return (i+1)
                G.foldM'_ step (0::Int) vec
                return (max maxR r, cc + c)
    -- figure out the dimension of the new matrix
    (m, n) = (sum *** maximum) . unzip . Prelude.map ((maximum *** sum) .
                unzip . Prelude.map (MG.rows &&& MG.cols)) $ ms
{-# INLINE fromBlocks #-}

isSymmetric :: (Eq a, G.Vector v a) => Matrix v a -> Bool
isSymmetric m@(Matrix r c _ _ _) | r /= c = False
                                 | otherwise = all f [0 .. r-1]
  where
    f i = all g [i + 1 .. c-1]
      where g j = m MG.! (i,j) == m MG.! (j,i)
{-# INLINE isSymmetric #-}

force :: G.Vector v a => Matrix v a -> Matrix v a
force m@(Matrix r c _ _ _) = MG.fromVector (r,c) . G.force . MG.flatten $ m
{-# INLINE force #-}

imap :: (G.Vector v a, G.Vector v b) => ((Int, Int) -> a -> b) -> Matrix v a -> Matrix v b
imap f m@(Matrix r c _ _ _) = MG.fromVector (r,c) $ G.imap f' . MG.flatten $ m
  where
    f' i = f (i `div` c, i `mod` c)
{-# INLINE imap #-}

map :: (G.Vector v a, G.Vector v b) => (a -> b) -> Matrix v a -> Matrix v b
map f m@(Matrix r c _ _ _) = MG.fromVector (r,c) $ G.map f . MG.flatten $ m
{-# INLINE map #-}

foldl :: G.Vector v b => (a -> b -> a) -> a -> Matrix v b -> a
foldl f acc m = G.foldl f acc . MG.flatten $ m
{-# INLINE foldl #-}

mapM :: (G.Vector v a, G.Vector v b, Monad m) => (a -> m b) -> Matrix v a -> m (Matrix v b)
mapM f m@(Matrix r c _ _ _) = liftM (MG.fromVector (r,c)) . G.mapM f . MG.flatten $ m
{-# INLINE mapM #-}

mapM_ :: (G.Vector v a, Monad m) => (a -> m b) -> Matrix v a -> m ()
mapM_ f = G.mapM_ f . MG.flatten
{-# INLINE mapM_ #-}

forM :: (G.Vector v a, G.Vector v b, Monad m) => Matrix v a -> (a -> m b) -> m (Matrix v b)
forM = flip mapM
{-# INLINE forM #-}

forM_ :: (G.Vector v a, Monad m) => Matrix v a -> (a -> m b) -> m ()
forM_ = flip mapM_
{-# INLINE forM_ #-}

sequence :: (G.Vector v a, G.Vector v (m a), Monad m)
         => Matrix v (m a) -> m (Matrix v a)
sequence (Matrix r c tda offset vec) = liftM (Matrix r c tda offset) . G.sequence $ vec
{-# INLINE sequence #-}

sequence_ :: (G.Vector v (m a), Monad m)
          => Matrix v (m a) -> m ()
sequence_ (Matrix _ _ _ _ vec) = G.sequence_ vec
{-# INLINE sequence_ #-}

generate :: G.Vector v a => (Int, Int) -> ((Int, Int) -> a) -> Matrix v a
generate (r,c) f = MG.fromVector (r,c) . G.generate (r*c) $ \i -> f (i `div` c, i `mod` c)
{-# INLINE generate #-}
