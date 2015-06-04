{-# LANGUAGE TemplateHaskell #-}

module Oscillator (Oscillator(..)
                  , wave
                  , amplitude
                  , phase
                  , pitch
                  , sine
                  , triangle
                  , sawtooth
                  , makeDefault
                  , sample
                  ) where

import Samples (Samples)
import qualified WaveTable as WT

import Control.Lens (makeLenses)

import qualified Data.Vector as V

data Oscillator = Oscillator { _wave :: WT.WaveTable
                             , _amplitude :: Double
                             , _phase :: Double
                             , _pitch :: Double
                             } deriving (Show)

makeLenses ''Oscillator

-- The default amplitude of an Oscillator.
defaultAmplitude :: Double
defaultAmplitude = 0.1

-- The default phase of an Oscillator.
defaultPhase :: Double
defaultPhase = 0

-- The default pitch of an Oscillator.
defaultPitch :: Double
defaultPitch = 0

-- Create a default Oscillator from a WaveTable.
makeDefault :: WT.WaveTable -> Oscillator
makeDefault wt = Oscillator wt defaultAmplitude defaultPhase defaultPitch

-- Generate n samples from the Oscillator using the provided sample rate and
-- frequency.
sample :: Oscillator -> Double -> Double -> Int -> Samples
sample (Oscillator wt amp _ p) sr freq n = V.map (amp *) samples where
    samples = WT.sample wt sr freq' n
    freq' = freq * (2 ** (p / 12))

-- A default sine Oscillator.
sine :: Oscillator
sine = makeDefault WT.sine

-- A default triangle Oscillator.
triangle :: Oscillator
triangle = makeDefault WT.triangle

-- A default sawtooth Oscillator.
sawtooth :: Oscillator
sawtooth = makeDefault WT.sawtooth
