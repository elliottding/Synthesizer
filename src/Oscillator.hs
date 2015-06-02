{-# LANGUAGE TemplateHaskell #-}

module Oscillator (Oscillator(..)
                  , wave
                  , amplitude
                  , phase
                  , sine
                  , triangle
                  , sawtooth
                  , makeDefault
                  , sample
                  ) where

import Samples (Samples(..))
import qualified WaveTable as WT

import Control.Lens (makeLenses)

import qualified Data.Vector as V

data Oscillator = Oscillator { _wave :: WT.WaveTable
                             , _amplitude :: Double
                             , _phase :: Double
                             } deriving (Show)

makeLenses ''Oscillator

defaultAmplitude :: Double
defaultAmplitude = 0.1

defaultPhase :: Double
defaultPhase = 0

makeDefault :: WT.WaveTable -> Oscillator
makeDefault wt = Oscillator wt defaultAmplitude defaultPhase

sample :: Oscillator -> Double -> Double -> Int -> Samples
sample (Oscillator wt amp phase) sr freq n = V.map (amp *) samples where
    samples = WT.sample wt sr freq n

sine :: Oscillator
sine = makeDefault WT.sine

triangle :: Oscillator
triangle = makeDefault WT.triangle

sawtooth :: Oscillator
sawtooth = makeDefault WT.sawtooth
