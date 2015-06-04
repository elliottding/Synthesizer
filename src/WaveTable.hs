module WaveTable (WaveTable
                 , save
                 , load
                 , sampleRate
                 , sample
                 , sine
                 , triangle
                 , sawtooth
                 , fromString
                 , loadFromImage
                 ) where

import Samples (Samples)
import Util (upsample)

import Codec.Picture (DynamicImage(..), Image(..), PixelRGB8(..), readPng)
import Codec.Picture.Types (pixelFold)
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

-- Threshold a pixel to a bit value.
threshold :: PixelRGB8 -> Bool
threshold (PixelRGB8 r _ _) = r == 0

-- Update the Vector with a Pixel and its coordinates.
updatePixelVector :: V.Vector Double -> Int -> Int -> PixelRGB8 -> V.Vector Double
updatePixelVector v x y p
    | threshold p = v V.// [(x, y')]
    | otherwise   = v
    where
        y' = (fromIntegral y) / 127.5 - 1.0

-- Convert a wave Image to a Vector.
imageToVector :: Image PixelRGB8 -> V.Vector Double
imageToVector img = pixelFold updatePixelVector (V.generate width (\_ -> 0)) img where
    width = imageWidth img

-- Unwrap an Image from a DynamicImage.
unwrapImage :: DynamicImage -> Image PixelRGB8
unwrapImage (ImageRGB8 img) = img
unwrapImage _ = error "Unhandled image format."

-- Create a WaveTable from an Image.
fromImage :: Image PixelRGB8 -> WaveTable
fromImage = upsample 96000 . imageToVector

-- Load a WaveTable from an image.
loadFromImage :: FilePath -> IO WaveTable
loadFromImage path = do
    eimg <- readPng path
    case eimg of
        Left  err  -> error err
        Right dimg -> do
            let wt = fromImage $ unwrapImage dimg
            return wt
