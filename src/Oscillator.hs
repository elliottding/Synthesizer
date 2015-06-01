module Oscillator where

import qualified WaveTable as WT
import Samples (Samples(..))

import qualified Data.Vector as V

data Oscillator = Oscillator { oscWaveTable :: WT.WaveTable
                             , oscAmp :: Double
                             , oscPhase :: Double
                             }

defaultAmplitude :: Double
defaultAmplitude = 0.1

sample :: Oscillator -> Double -> Double -> Int -> Samples
sample (Oscillator wt amp phase) sr freq n = V.map (amp *) samples where
    samples = WT.sample wt sr freq n

sine :: Oscillator
sine = Oscillator WT.sine defaultAmplitude 0

triangle :: Oscillator
triangle = Oscillator WT.triangle defaultAmplitude 0

sawtooth :: Oscillator
sawtooth = Oscillator WT.sawtooth defaultAmplitude 0

oscFromString :: String -> Oscillator
oscFromString name = case name of
    "sine" -> sine
    "triangle" -> triangle
    "sawtooth" -> sawtooth
    _ -> sine
