{-# LANGUAGE GADTs #-}
{-# LANGUAGE Rank2Types #-}
module Data.Matrix.Generic.Mutable
   ( fromMVector
   , thaw
   , unsafeThaw
   , freeze
   , unsafeFreeze
   , write
   , unsafeWrite
   , read
   , unsafeRead
   , replicate
   , new
   , create
   ) where

import Prelude hiding (read, replicate)
import Control.Monad
import Control.Monad.ST
import Data.Matrix.Generic.Types
import qualified Data.Vector.Generic as G
import qualified Data.Vector.Generic.Mutable as GM
import Control.Monad.Primitive

-- to be removed in GHC-7.10
(<$>) :: Monad m => (a -> b) -> m a -> m b
(<$>) = liftM

fromMVector :: GM.MVector v a => (Int, Int) -> v m a -> MMatrix v m a
fromMVector (r,c) = MMatrix r c c 0
{-# INLINE fromMVector #-}

thaw :: PrimMonad m => Matrix v a -> m (MMatrix (G.Mutable v) (PrimState m) a)
thaw (Matrix r c tda offset v) = MMatrix r c tda offset <$> G.thaw v
{-# INLINE thaw #-}

unsafeThaw :: PrimMonad m => Matrix v a -> m (MMatrix (G.Mutable v) (PrimState m) a)
unsafeThaw (Matrix r c tda offset v) = MMatrix r c tda offset <$> G.unsafeThaw v
{-# INLINE unsafeThaw #-}

freeze :: (PrimMonad m, G.Vector v a) => MMatrix (G.Mutable v) (PrimState m) a -> m (Matrix v a)
freeze (MMatrix r c tda offset v) = Matrix r c tda offset <$> G.freeze v
{-# INLINE freeze #-}

unsafeFreeze :: (PrimMonad m, G.Vector v a) => MMatrix (G.Mutable v) (PrimState m) a -> m (Matrix v a)
unsafeFreeze (MMatrix r c tda offset v) = Matrix r c tda offset <$> G.unsafeFreeze v
{-# INLINE unsafeFreeze #-}

write :: (PrimMonad m, GM.MVector v a)
      => MMatrix v (PrimState m) a -> (Int, Int) -> a -> m ()
write (MMatrix _ _ tda offset v) (i,j) = GM.write v idx
  where idx = offset + i * tda + j
{-# INLINE write #-}

unsafeWrite :: (PrimMonad m, GM.MVector v a)
            => MMatrix v (PrimState m) a -> (Int, Int) -> a -> m ()
unsafeWrite (MMatrix _ _ tda offset v) (i,j) = GM.unsafeWrite v idx
  where idx = offset + i * tda + j
{-# INLINE unsafeWrite #-}

read :: (PrimMonad m, GM.MVector v a)
     => MMatrix v (PrimState m) a -> (Int, Int) -> m a
read (MMatrix _ _ tda offset v) (i,j) = GM.read v idx
  where idx = offset + i * tda + j
{-# INLINE read #-}

unsafeRead :: (PrimMonad m, GM.MVector v a)
           => MMatrix v (PrimState m) a -> (Int, Int) -> m a
unsafeRead (MMatrix _ _ tda offset v) (i,j) = GM.unsafeRead v idx
  where idx = offset + i * tda + j
{-# INLINE unsafeRead #-}

replicate :: (PrimMonad m, GM.MVector v a)
          => Int -> Int -> a -> m (MMatrix v (PrimState m) a)
replicate r c x = fromMVector (r,c) <$> GM.replicate (r*c) x
{-# INLINE replicate #-}

new :: (PrimMonad m, GM.MVector v a)
    => Int -> Int -> m (MMatrix v (PrimState m) a)
new r c = fromMVector (r,c) <$> GM.new (r*c)
{-# INLINE new #-}

create :: G.Vector v a => (forall s . ST s (MMatrix (G.Mutable v) s a)) -> Matrix v a
create m = runST $ unsafeFreeze =<< m
{-# INLINE create #-}
