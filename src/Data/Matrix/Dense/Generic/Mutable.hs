{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
module Data.Matrix.Dense.Generic.Mutable
   ( -- * Mutable Matrix
     MMatrix(..)
   , C.fromMVector
   , dim
   , flatten
   , takeRow
   , thaw
   , unsafeThaw
   , freeze
   , unsafeFreeze
   , C.write
   , C.unsafeWrite
   , C.read
   , C.unsafeRead
   , replicate
   , C.new
   , create
   ) where

import Prelude hiding (read, replicate)
import Control.Monad
import Control.Monad.ST
import qualified Data.Vector.Generic as G
import qualified Data.Vector.Generic.Mutable as GM
import Control.Monad.Primitive

import qualified Data.Matrix.Generic.Mutable as C
import Data.Matrix.Dense.Generic hiding (forM_, takeRow)

-- | mutable matrix
data MMatrix v s a = MMatrix !Int !Int !Int !Int !(v s a)

instance GM.MVector v a => C.MMatrix MMatrix v a where
    dim (MMatrix r c _ _ _) = (r,c)
    {-# INLINE dim #-}

    fromMVector (r,c) = MMatrix r c c 0
    {-# INLINE fromMVector #-}

    flatten (MMatrix m n tda offset vec)
        | n == tda = return $ GM.slice offset (m * n) vec
        | otherwise = do
            vec' <- GM.new (m*n)
            forM_ [0 .. m*n] $ \i ->
                GM.unsafeRead vec (offset + (i `div` n) * tda + i `mod` n) >>=
                    GM.unsafeWrite vec' i
            return vec'
    {-# INLINE flatten #-}

    unsafeRead (MMatrix _ _ tda offset v) (i,j) = GM.unsafeRead v idx
      where idx = offset + i * tda + j
    {-# INLINE unsafeRead #-}

    unsafeWrite (MMatrix _ _ tda offset v) (i,j) = GM.unsafeWrite v idx
      where idx = offset + i * tda + j
    {-# INLINE unsafeWrite #-}
    

-- to be removed in GHC-7.10
(<$>) :: Monad m => (a -> b) -> m a -> m b
(<$>) = liftM

takeRow :: GM.MVector v a => MMatrix v m a -> Int -> v m a
takeRow (MMatrix _ c tda offset vec) i = GM.slice i' c vec
  where
    i' = offset + i * tda
{-# INLINE takeRow #-}


unsafeThaw :: (G.Vector v a, PrimMonad m)
           => Matrix v a -> m (MMatrix (G.Mutable v) (PrimState m) a)
unsafeThaw (Matrix r c tda offset v) = MMatrix r c tda offset <$> G.unsafeThaw v
{-# INLINE unsafeThaw #-}

freeze :: (PrimMonad m, G.Vector v a) => MMatrix (G.Mutable v) (PrimState m) a -> m (Matrix v a)
freeze (MMatrix r c tda offset v) = Matrix r c tda offset <$> G.freeze v
{-# INLINE freeze #-}

unsafeFreeze :: (PrimMonad m, G.Vector v a) => MMatrix (G.Mutable v) (PrimState m) a -> m (Matrix v a)
unsafeFreeze (MMatrix r c tda offset v) = Matrix r c tda offset <$> G.unsafeFreeze v
{-# INLINE unsafeFreeze #-}

replicate :: (PrimMonad m, GM.MVector v a)
          => (Int, Int) -> a -> m (MMatrix v (PrimState m) a)
replicate (r,c) x = C.fromMVector (r,c) <$> GM.replicate (r*c) x
{-# INLINE replicate #-}

create :: G.Vector v a => (forall s . ST s (MMatrix (G.Mutable v) s a)) -> Matrix v a
create m = runST $ unsafeFreeze =<< m
{-# INLINE create #-}
