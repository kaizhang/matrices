{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}

module Data.Matrix.Generic.Mutable
    ( MMatrix(..)
    , write
    , read
    ) where

import Prelude hiding (read)
import Control.Monad.Primitive (PrimMonad, PrimState)
import qualified Data.Vector.Generic.Mutable as GM

class GM.MVector v a => MMatrix m v a where
    dim ::  m v s a -> (Int, Int)

    unsafeRead :: PrimMonad s => m v (PrimState s) a -> (Int, Int) -> s a

    unsafeWrite :: PrimMonad s => m v (PrimState s) a -> (Int, Int) -> a -> s ()

    -- | Create a mutable matrix without initialization
    new :: PrimMonad s => (Int, Int) -> s (m v (PrimState s) a)

    replicate :: PrimMonad s => (Int, Int) -> a -> s (m v (PrimState s) a)

    {-# MINIMAL dim, unsafeRead, unsafeWrite, new, replicate #-}

-- | Derived methods

write :: (PrimMonad s, MMatrix m v a)
      => m v (PrimState s) a -> (Int, Int) -> a -> s ()
write mat (i,j) | i >= r || j >= c = error "Index out of bounds"
                | otherwise = unsafeWrite mat (i,j)
  where
    (r,c) = dim mat
{-# INLINE write #-}

read :: (PrimMonad s, MMatrix m v a)
     => m v (PrimState s) a -> (Int, Int) -> s a
read mat (i,j) | i >= r || j >= c = error "Index out of bounds"
               | otherwise = unsafeRead mat (i,j)
  where
    (r,c) = dim mat
{-# INLINE read #-}

{-
replicate :: (PrimMonad s, MMatrix m v a)
          => (Int, Int) -> a -> s (m v (PrimState s) a)
replicate (r,c) x = do
    mat <- new (r,c)
 -}   
