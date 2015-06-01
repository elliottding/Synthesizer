module Envelope (ADSR(..)
                , envelop) where

import Samples (Samples(..))

import qualified Data.Vector as V

data ADSR = ADSR { adsrAttack :: Double
                 , adsrDecay :: Double
                 , adsrSustain :: Double
                 , adsrRelease :: Double
                 } deriving (Show)

-- Interpolate x between a and b.
interpolate :: Double -> Double -> Double -> Double
interpolate a b x = x * (b - a) + a

-- Float division on Ints.
divf :: Int -> Int -> Double
divf a b = (fromIntegral a) / (fromIntegral b)

-- Modulate the given Samples by the ADSR envelope.
envelop :: ADSR -> Double -> Samples -> Samples
envelop (ADSR a d s r) sr samples = V.imap f samples where
    [an, dn, rn] = map (truncate . (*) sr) [a, d, r]
    di = an + dn
    si = V.length samples - rn
    f i x
        | i < an    = x * (interpolate 0 1 (i `divf` an))
        | i < di    = x * (interpolate 1 s ((i - an) `divf` dn))
        | i < si    = x * s
        | otherwise = x * (interpolate s 0 ((i - si) `divf` rn))
