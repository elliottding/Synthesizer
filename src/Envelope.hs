{-# LANGUAGE TemplateHaskell #-}

module Envelope (ADSR(..)
                , attack
                , decay
                , sustain
                , release
                , envelop
                ) where

import Samples (Samples)
import Util (interpolate, divf)

import Control.Lens (makeLenses)
import qualified Data.Vector as V

data ADSR = ADSR { _attack :: Double
                 , _decay :: Double
                 , _sustain :: Double
                 , _release :: Double
                 } deriving (Show)

makeLenses ''ADSR

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
