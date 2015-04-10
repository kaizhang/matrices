{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}
module Data.Matrix.Sparse.Generic
    ( Zero(..)
    , CSR(..)
    , fromAscAL
    , fromAscStream
    , (!)
    ) where

import Control.Monad (when, forM_)
import Control.Monad.ST (runST)
import Data.Matrix.Generic
import qualified Data.Vector.Generic as G
import qualified Data.Vector.Generic.Mutable as GM
import qualified Data.Vector.Unboxed as U
import qualified Data.Vector.Fusion.Stream as S
import Data.Bits (shiftR)

class Eq a => Zero a where
    zero :: a

instance Zero Int where
    zero = 0

instance Zero Double where
    zero = 0.0

instance Eq a => Zero ([] a) where
    zero = []

-- | Compressed Sparse Row (CSR) matrix
data CSR v a = CSR !Int  -- rows
                   !Int  -- cols
                   !(v a)  -- values
                   !(U.Vector Int)  -- column index of values
                   !(U.Vector Int)  -- row pointer
    deriving (Show)

instance (Zero a, G.Vector v a) => Matrix CSR v a where
    dim (CSR r c _ _ _) = (r,c)
    {-# INLINE dim #-}

    unsafeIndex (CSR _ _ vec ci rp) (i,j) =
        case binarySearchByBounds ci j r0 r1 of
            Nothing -> zero
            Just k -> vec `G.unsafeIndex` k
      where
        r0 = rp `U.unsafeIndex` i
        r1 = rp `U.unsafeIndex` (i+1) - 1
    {-# INLINE unsafeIndex #-}

    unsafeFromVector (r,c) vec =
        CSR r c (G.generate n (G.unsafeIndex vec . U.unsafeIndex nz))
                (U.map (`mod` c) nz)
                (U.fromList . g . U.foldr f ((r-1,n-1), [n]) $ nz)
      where
        nz = U.filter (\i -> vec `G.unsafeIndex` i /= zero) . U.enumFromN 0 $ (r*c)
        f i ((!prev,!acc), xs) | stride == 0 = ((prev, acc-1), xs)
                               | otherwise = ((current, acc-1), replicate stride (acc+1) ++ xs)
          where
            stride = prev - current
            current = i `div` c
        g ((a, _), xs) | a == 0 = 0 : xs
                       | otherwise = replicate (a+1) 0 ++ xs
        n = U.length nz
    {-# INLINE unsafeFromVector #-}

type AssocList a = [((Int, Int), a)]

fromAscAL :: G.Vector v a => (Int, Int) -> Int -> AssocList a -> CSR v a
fromAscAL (r,c) n al = fromAscStream (r,c) n . S.fromList $ al
{-# INLINE fromAscAL #-}

fromAscStream :: (GM.MVector (G.Mutable v) a, G.Vector v a) => (Int, Int) -> Int -> S.Stream ((Int,Int),a) -> CSR v a
fromAscStream (r,c) n al = CSR r c values ci rp
  where
    (values, ci, rp) = runST $ do
        v <- GM.new n
        col <- GM.new n
        row <- GM.new (r+1)

        let f (i',acc) ((i,j),x) = do
                GM.write v acc x
                GM.write col acc j

                let stride = i - i'
                when (stride > 0) $ forM_ [0..stride-1] $ \s -> GM.write row (i-s) acc
                
                return (i,acc+1)

        _ <- S.foldM f (0,0) al
        GM.write row r n

        v' <- G.unsafeFreeze v
        col' <- G.unsafeFreeze col
        row' <- G.unsafeFreeze row
        return (v', col', row')
{-# INLINE fromAscStream #-}
    
binarySearchByBounds :: U.Vector Int -> Int -> Int -> Int -> Maybe Int
binarySearchByBounds vec x = loop
  where
    loop !l !u
        | l > u = Nothing
        | x == x' = Just k
        | x < x' = loop l (k-1)
        | otherwise = loop (k+1) u
      where
        k = (u+l) `shiftR` 1
        x' = vec `U.unsafeIndex` k
{-# INLINE binarySearchByBounds #-}
