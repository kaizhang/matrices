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
    
    fromMVector :: (Int, Int) -> v s a -> m v s a

    flatten :: PrimMonad s => m v (PrimState s) a -> s (v (PrimState s) a)

    unsafeRead :: PrimMonad s => m v (PrimState s) a -> (Int, Int) -> s a

    unsafeWrite :: PrimMonad s => m v (PrimState s) a -> (Int, Int) -> a -> s ()

    new :: PrimMonad s => (Int, Int) -> s (m v (PrimState s) a)
    new (r,c) = do v <- GM.new (r*c)
                   return $ fromMVector (r,c) v
    {-# INLINE new #-}

    {-# MINIMAL dim, fromMVector, flatten, unsafeRead, unsafeWrite #-}

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
