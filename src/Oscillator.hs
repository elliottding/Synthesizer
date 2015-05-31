module Oscillator where

import BasicTypes (Sample, Frequency)
import qualified WaveTable as WT

data Oscillator = Oscillator { oscWaveTable :: WT.WaveTable
                             , oscVolume :: Double
                             , oscPhase :: Double
                             }

defaultVolume :: Double
defaultVolume = 0.1

sample :: Oscillator -> Frequency -> Int -> [Sample]
sample (Oscillator wt vol phase) freq n = map (vol *) samples where
    samples = WT.sample wt freq n

sine :: Oscillator
sine = Oscillator WT.sine defaultVolume 0

triangle :: Oscillator
triangle = Oscillator WT.triangle defaultVolume 0

sawtooth :: Oscillator
sawtooth = Oscillator WT.sawtooth defaultVolume 0

oscFromString :: String -> Oscillator
oscFromString name = case name of
    "sine" -> sine
    "triangle" -> triangle
    "sawtooth" -> sawtooth
    _ -> error "Unrecognized Oscillator name."
