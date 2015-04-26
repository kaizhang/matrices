{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
module Data.Matrix.Sparse.Generic
    ( Zero(..)
    , CSR(..)
    , fromAscAL
    , fromAscStream
    , (!)
    ) where

import Control.Applicative ((<$>))
import Control.Monad (when, forM_, guard)
import Control.Monad.ST (runST)
import Data.Binary (Binary(..), Get, Put, Word32)
import Data.Binary.IEEE754 (getFloat64le, putFloat64le)
import Data.Binary.Get (getWord64le, getWord32le)
import Data.Binary.Put (putWord64le, putWord32le)
import qualified Data.Vector.Generic as G
import qualified Data.Vector.Generic.Mutable as GM
import qualified Data.Vector.Unboxed as U
import qualified Data.Vector.Fusion.Stream as S
import Data.Bits (shiftR)

import Data.Matrix.Generic
import Data.Matrix.Dense.Generic.Mutable (MMatrix)

class Eq a => Zero a where
    zero :: a

instance Zero Int where
    zero = 0

instance Zero Double where
    zero = 0.0

instance Eq a => Zero ([] a) where
    zero = []

-- | mutable sparse matrix not implemented
type instance Mutable CSR = MMatrix

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

    unsafeTakeRow (CSR _ c vec ci rp) i = G.fromList $ loop (-1) r0
      where
        loop !prev !n
            | n > r1 = replicate (c-prev-1) zero
            | otherwise = replicate (cur-prev-1) zero ++ (x : loop cur (n+1))
          where
            cur = ci `U.unsafeIndex` n
            x = vec `G.unsafeIndex` n
        r0 = rp `U.unsafeIndex` i
        r1 = rp `U.unsafeIndex` (i+1) - 1
    {-# INLINE unsafeTakeRow #-}

    thaw = undefined
    unsafeThaw = undefined
    freeze = undefined
    unsafeFreeze = undefined

magic :: Word32
magic = 0x177BFFA0

instance (G.Vector v a, Binary a) => Binary (CSR v a) where
    put = putMatrix put
    get = getMatrix get

instance G.Vector v Double => Binary (CSR v Double) where
    put = putMatrix putFloat64le
    get = getMatrix getFloat64le

instance G.Vector v Int => Binary (CSR v Int) where
    put = putMatrix $ putWord64le . fromIntegral
    get = getMatrix $ fromIntegral <$> getWord64le

putMatrix :: G.Vector v a => (a -> Put) -> CSR v a -> Put
putMatrix putElement (CSR r c vec ci rp) = do
    putWord32le magic
    putWord64le . fromIntegral $ r
    putWord64le . fromIntegral $ c
    putWord64le . fromIntegral $ n
    G.mapM_ putElement vec
    G.mapM_ (putWord64le . fromIntegral) ci
    G.mapM_ (putWord64le . fromIntegral) rp
  where
    n = G.length vec
{-# INLINE putMatrix #-}

getMatrix :: G.Vector v a => Get a -> Get (CSR v a)
getMatrix getElement = do
    m <- getWord32le
    guard $ m == magic
    r <- fromIntegral <$> getWord64le
    c <- fromIntegral <$> getWord64le
    n <- fromIntegral <$> getWord64le
    vec <- G.replicateM n getElement
    ci <- G.replicateM n $ fromIntegral <$> getWord64le
    rp <- G.replicateM c $ fromIntegral <$> getWord64le
    return $ CSR r c vec ci rp
{-# INLINE getMatrix #-}

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
