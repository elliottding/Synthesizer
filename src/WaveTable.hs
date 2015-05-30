module WaveTable (WaveTable, sine, triangle, sawtooth, sample,
                  sampleDuration) where

import BasicTypes (Sample, SampleRate, Frequency, defaultSampleRate)

-- A list of Doubles, representing the positional data of an oscillating wave
-- over a single period.
type WaveTable = [Double]

-- Create a WaveTable from a sample rate, a period, and a time-to-position
-- oscillating function.
makeWaveTable :: SampleRate -> Double -> (Double -> Double) -> WaveTable
makeWaveTable sampleRate period f =
    take (truncate sampleRate) $ map f [0 :: Double, (period / sampleRate)..]

-- Create a WaveTable using the default sample rate.
makeWaveTable' :: Double -> (Double -> Double) -> WaveTable
makeWaveTable' = makeWaveTable defaultSampleRate

-- A sine WaveTable.
sine :: WaveTable
sine = makeWaveTable' (2 * pi) sin

-- The triangle wave position function.
triangleF :: Double -> Double
triangleF t = 2 * c * (t - (fromIntegral thalf)) where
    thalf = truncate $ t + 0.5
    c = if even thalf then 1.0 else (-1.0)

-- A triangle WaveTable.
triangle :: WaveTable
triangle = makeWaveTable' 2 triangleF

-- A sawtooth WaveTable.
sawtooth :: WaveTable
sawtooth = makeWaveTable' 1 f where
    f t = t - fromIntegral (truncate t)

-- Take every nth element from a list.
takeEvery :: Int -> [a] -> [a]
takeEvery n [] = []
takeEvery n (x:xs) = x : (takeEvery n $ drop (n-1) xs)

-- Generate (infinite) samples from a WaveTable at a given frequency.
sampleForever :: WaveTable -> Frequency -> [Sample]
sampleForever wt freq = takeEvery n $ cycle wt where
    n = truncate $ defaultSampleRate / freq

-- Generate n samples from a WaveTable at a given frequency.
sample :: WaveTable -> Frequency -> Int -> [Sample]
sample wt freq n = take n $ sampleForever wt freq

-- Generate samples from a WaveTable at a given frequency for the provided
-- duration in seconds.
sampleDuration :: WaveTable -> Frequency -> Double -> [Sample]
sampleDuration wt freq sec = sample wt freq n where
    n = truncate $ defaultSampleRate * sec
