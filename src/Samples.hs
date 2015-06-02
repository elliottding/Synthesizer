module Samples (Samples(..)
               , compose
               , composeAll
               , zipWithPadding
               , zipWithOffset
               ) where

import qualified Data.Vector as V

type Samples = V.Vector Double

modulate :: Num a => V.Vector a -> V.Vector a -> V.Vector a
modulate = V.zipWith (*)

series :: Double -> Double -> Int -> V.Vector Double
series a b n = V.generate n f where
    f x = (fromIntegral x) / nf * (b - a) + a
    nf = fromIntegral n

-- Compose two samples using addition.
compose :: Samples -> Samples -> Samples
compose = zipWithPadding (+) 0 0

-- Compose all samples in the list using addition.
composeAll :: [Samples] -> Samples
composeAll = foldr compose V.empty

-- zipWith on different lengths by right-padding the Vectors using the given
-- padding elements.
zipWithPadding :: (a -> b -> c) -> a -> b -> V.Vector a -> V.Vector b -> V.Vector c
zipWithPadding f x y xs ys
    | m >= n    = V.zipWith f xs (ys V.++ V.replicate (m - n) y)
    | otherwise = V.zipWith f (xs V.++ V.replicate (n - m) x) ys
    where
        m = V.length xs
        n = V.length ys

-- zipWithPadding, preprending the Vectors with m, n padding elements
zipWithOffset :: (a -> b -> c) -> a -> b -> Int -> Int -> V.Vector a -> V.Vector b -> V.Vector c
zipWithOffset f x y m n xs ys = zipWithPadding f x y xs' ys' where
    xs' = V.replicate m x V.++ xs
    ys' = V.replicate n y V.++ ys
