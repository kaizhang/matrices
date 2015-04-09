{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
module Data.Matrix.Sparse.Generic where

import Data.Matrix.Generic
import qualified Data.Vector.Generic as G
import qualified Data.Vector.Unboxed as U
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

