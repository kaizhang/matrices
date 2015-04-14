{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE Rank2Types #-}
module Data.Matrix.Generic
    ( Mutable
    , Matrix(..)

    -- * Derived mothods
    , rows
    , cols
    , (!)
    , fromVector
    , empty
    , toList
    , fromLists
    , matrix
    , fromRows
    , toRows
    , toColumns
    , toLists
    , create
    ) where

import Control.Monad.ST (runST, ST)
import Control.Monad.Primitive (PrimMonad, PrimState)
import qualified Data.Vector.Generic as G

import qualified Data.Matrix.Generic.Mutable as MM

type family Mutable (m :: (* -> *) -> * -> *) :: (* -> * -> *) -> * -> * -> *

class (MM.MMatrix (Mutable m) (G.Mutable v) a, G.Vector v a) => Matrix m v a where
    dim :: m v a -> (Int, Int)

    unsafeIndex :: m v a -> (Int, Int) -> a

    unsafeFromVector :: (Int, Int) -> v a -> m v a

    -- | Default algorithm is O((m*n) * O(unsafeIndex)).
    flatten :: m v a -> v a
    flatten mat = G.generate (r*c) $ \i -> unsafeIndex mat (i `div` c, i `mod` c)
      where
        (r,c) = dim mat
    {-# INLINE flatten #-}

    -- | Extract a row. Default algorithm is O(n * O(unsafeIndex)).
    takeRow :: m v a -> Int -> v a
    takeRow mat i = G.generate c $ \j -> unsafeIndex mat (i,j)
      where
        (_,c) = dim mat
    {-# INLINE takeRow #-}

    -- | Extract a column. Default algorithm is O(m * O(unsafeIndex)).
    takeColumn :: m v a -> Int -> v a
    takeColumn mat j = G.generate r $ \i -> unsafeIndex mat (i,j)
      where
        (r,_) = dim mat
    {-# INLINE takeColumn #-}

    -- | Extract the diagonal. Default algorithm is O(min(m,n) * O(unsafeIndex)).
    takeDiag :: m v a -> v a
    takeDiag mat = G.generate n $ \i -> unsafeIndex mat (i,i)
      where
        n = uncurry min . dim $ mat
    {-# INLINE takeDiag #-}

    thaw :: PrimMonad s => m v a -> s ((Mutable m) (G.Mutable v) (PrimState s) a)

    unsafeThaw :: PrimMonad s
               => m v a -> s ((Mutable m) (G.Mutable v) (PrimState s) a)

    freeze :: PrimMonad s
           => (Mutable m) (G.Mutable v) (PrimState s) a -> s (m v a)

    unsafeFreeze :: PrimMonad s
                 => (Mutable m) (G.Mutable v) (PrimState s) a -> s (m v a)

    {-# MINIMAL dim, unsafeIndex, unsafeFromVector, thaw, unsafeThaw, freeze, unsafeFreeze #-}

-- | Derived methods

-- | Return the number of rows
rows :: Matrix m v a => m v a -> Int
rows = fst . dim
{-# INLINE rows #-}

-- | Return the number of columns
cols :: Matrix m v a => m v a -> Int
cols = snd . dim
{-# INLINE cols #-}

-- | Indexing
(!) :: Matrix m v a => m v a -> (Int, Int) -> a
(!) mat (i,j) | i >= r || j >= c = error "Index out of bounds"
              | otherwise = unsafeIndex mat (i,j)
  where
    (r,c) = dim mat
{-# INLINE (!) #-}

-- | O(m*n) Create a list by concatenating rows
toList :: Matrix m v a => m v a -> [a]
toList = G.toList . flatten
{-# INLINE toList #-}

empty :: Matrix m v a => m v a
empty = fromVector (0,0) G.empty
{-# INLINE empty #-}

fromVector :: Matrix m v a => (Int, Int) -> v a -> m v a
fromVector (r,c) vec | r*c /= G.length vec = error "incorrect length"
                     | otherwise = unsafeFromVector (r,c) vec
{-# INLINE fromVector #-}

-- | O(m*n) Matrix construction
matrix :: Matrix m v a
       => Int  -- ^ number of columns
       -> [a]  -- ^ row list
       -> m v a
matrix ncol xs | n `mod` ncol /= 0 = error "incorrect length"
               | otherwise = unsafeFromVector (nrow,ncol) vec
  where
    vec = G.fromList xs
    nrow = n `div` ncol
    n = G.length vec
{-# INLINE matrix #-}

-- | O(m*n) Create matrix from list of lists, it doesn't check if the list of
-- list is a valid matrix
fromLists :: Matrix m v a => [[a]] -> m v a
fromLists xs | null xs = empty
             | otherwise = fromVector (r,c) . G.fromList . concat $ xs
  where
    r = length xs
    c = length . head $ xs
{-# INLINE fromLists #-}

-- | O(m*n) Create matrix from rows
fromRows :: Matrix m v a => [v a] -> m v a
fromRows xs | null xs = empty
            | otherwise = fromVector (r,c) . G.concat $ xs
  where
    r = length xs
    c = G.length . head $ xs
{-# INLINE fromRows #-}

-- | O(m) Return the rows
toRows :: Matrix m v a => m v a -> [v a]
toRows mat = map (takeRow mat) [0..r-1]
  where
    (r,_) = dim mat
{-# INLINE toRows #-}

-- | O(m*n) Return the columns
toColumns :: Matrix m v a => m v a -> [v a]
toColumns mat = map (takeColumn mat) [0..c-1]
  where
    (_,c) = dim mat
{-# INLINE toColumns #-}

-- | O(m*n) List of lists
toLists :: Matrix m v a => m v a -> [[a]]
toLists = map G.toList . toRows
{-# INLINE toLists #-}

create :: Matrix m v a => (forall s . ST s ((Mutable m) (G.Mutable v) s a)) -> m v a
create m = runST $ unsafeFreeze =<< m
{-# INLINE create #-}
