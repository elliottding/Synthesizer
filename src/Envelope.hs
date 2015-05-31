module Envelope where

import BasicTypes (Sample)

import Data.List.Split (splitPlaces)

data ASDR = ASDR { asdrAttack :: Double
                 , asdrDecay :: Double
                 , asdrSustain :: Double
                 , asdrRelease :: Double
                 }

modulate :: (Fractional a, Enum a) => (a -> a) -> [a] -> [a]
modulate f as = zipWith (*) as bs where
    bs = fmap (f . flip (/) n) [0..n]
    n = fromIntegral $ length as

{-
attack :: ASDR -> Double -> [Double]
attack (ASDR a _ _ _) sr = modulate id $ replicate n 1.0 where
    n = truncate $ sr * a

decay :: ASDR -> Double -> [Double]
decay (ASDR _ d s _) sr = modulate ((+) s . (*) (1 - s) . (-) 1)
-}

envelop :: ASDR -> Double -> [Double] -> [Double]
envelop (ASDR a d s r) rate samples = concat [asp, dsp, ssp, rsp] where
    n = length samples
    [an, dn, rn] = map (truncate . (*) rate) [a, d, r]
    sn = n - (an + dn + rn)
    [as, ds, ss, rs] = splitPlaces [an, dn, sn, rn] samples
    asp = modulate id as
    dsp = modulate ((+) s . (*) (1 - s) . (-) 1) ds
    ssp = map (s *) ss
    rsp = modulate ((*) s . (-) 1) rs
