module WaveTable (WaveTable
                 , save
                 , load
                 , sampleRate
                 , sample
                 , sine
                 , triangle
                 , sawtooth
                 , fromString
                 ) where

import Samples (Samples(..))

import qualified Data.Vector as V

-- A WaveTable represents the positional data of an oscillating wave over a
-- single period. A WaveTable's sample rate is equal to the length of the
-- data vector.
type WaveTable = V.Vector Double

-- The default WaveTable sample rate.
defaultSampleRate :: Double
defaultSampleRate = 96000

-- Create a WaveTable from a sample rate, a period, and a time-to-position
-- oscillating function.
makeWaveTable :: Double -> Double -> (Double -> Double) -> WaveTable
makeWaveTable sr per f = V.generate (truncate sr) $ f . (*) (per / sr) . fromIntegral

-- Create a WaveTable using the default sample rate.
makeWaveTable' :: Double -> (Double -> Double) -> WaveTable
makeWaveTable' = makeWaveTable defaultSampleRate

-- A sine WaveTable.
sine :: WaveTable
sine = makeWaveTable' (2 * pi) sin

-- The triangle wave position function.
triangleF :: Double -> Double
triangleF t = 2 * c * (t - (fromIntegral thalf)) where
    thalf = truncate $ t + 0.5 :: Int
    c = if even thalf then 1.0 else (-1.0)

-- A triangle WaveTable.
triangle :: WaveTable
triangle = makeWaveTable' 2 triangleF

-- The sawtooth wave position function.
sawtoothF :: Double -> Double
sawtoothF t = t - fromIntegral (truncate t :: Int)

-- A sawtooth WaveTable.
sawtooth :: WaveTable
sawtooth = makeWaveTable' 1 sawtoothF

-- Return the sample rate of a WaveTable.
sampleRate :: WaveTable -> Double
sampleRate = fromIntegral . V.length

-- Load a WaveTable from a file.
load :: FilePath -> IO (WaveTable)
load = fmap (V.fromList . map read . words) . readFile

-- Save a WaveTable to a file.
save :: FilePath -> WaveTable -> IO ()
save path = writeFile path . unwords . V.toList . V.map show

-- Generate n samples from the WaveTable at a new sample rate and a provided
-- frequency.
sample :: WaveTable -> Double -> Double -> Int -> Samples
sample wt sr freq n = V.generate n f where
    m = V.length wt
    s = freq * (fromIntegral m) / sr
    f = (V.!) wt . flip mod m . truncate . (*) s . fromIntegral

-- Return a default WaveTable from a String.
fromString :: String -> WaveTable
fromString name = case name of
    "sine"     -> sine
    "triangle" -> triangle
    "sawtooth" -> sawtooth
    _          -> error "Unrecognized WaveTable name."
