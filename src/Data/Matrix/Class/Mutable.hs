{-# LANGUAGE MultiParamTypeClasses #-}

module Data.Matrix.Class.Mutable where

import Control.Monad.Primitive (PrimMonad, PrimState)
import qualified Data.Vector.Generic.Mutable as GM

class GM.MVector v a => MMatrix m v a where
    dim ::  m v s a -> (Int, Int)
    
    fromMVector :: (Int, Int) -> v s a -> m v s a

    flatten :: PrimMonad s => m v (PrimState s) a -> s (v (PrimState s) a)

    unsafeRead :: PrimMonad s => m v (PrimState s) a -> (Int, Int) -> s a

    unsafeWrite :: PrimMonad s => m v (PrimState s) a -> (Int, Int) -> a -> s ()
