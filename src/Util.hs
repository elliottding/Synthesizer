module Util (upsample, interpolate, divf, (~!)) where

import qualified Data.Vector as V

-- Upsample a Vector to the new sample rate.
upsample :: Int -> V.Vector Double -> V.Vector Double
upsample m v = V.generate m ((~!) v . (*) a . fromIntegral) where
    n = V.length v
    a = (fromIntegral n) / (fromIntegral m)

-- Interpolate between two values.
interpolate :: Double -> Double -> Double -> Double
interpolate a b x = a + (b - a) * x

-- Float division on Ints.
divf :: Int -> Int -> Double
divf a b = (fromIntegral a) / (fromIntegral b)

-- Fractional index operator with interpolation.
-- e.g. <1,2,3> ~! 1.5 == 2.5
(~!) :: V.Vector Double -> Double -> Double
xs ~! k
    | k >= fromIntegral n = error "Out of bounds."
    | otherwise = interpolate a b (k - fromIntegral i)
    where
        n = V.length xs
        i = truncate k
        j = truncate (k + 1)
        a = xs V.! i
        b = if j >= n then 0 else xs V.! j
