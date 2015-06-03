{-# LANGUAGE TemplateHaskell #-}

module Note (Note(..)
            , time
            , duration
            , frequency
            , absoluteFrequency
            , pitchToFrequency
            , makeNote
            ) where

import Control.Lens (makeLenses)

import Data.Char (isDigit)

data Note = Note { _time :: Double
                 , _duration :: Double
                 , _frequency :: Double
                 } deriving (Show)

makeLenses ''Note

-- Create a Note from a time, duration, and pitch as String.
makeNote :: Double -> Double -> String -> Note
makeNote t d p = Note t d (pitchToFrequency p)

-- Frequency of A4 at concert pitch.
baseFrequency :: Double
baseFrequency = 440

-- Pitch value of A4.
basePitch :: Int
basePitch = 9

-- Octave of A4.
baseOctave :: Int
baseOctave = 4

-- Tone value of A4.
baseTone :: Int
baseTone = absoluteTone basePitch baseOctave

-- Calculate the tone value from pitch and octave.
absoluteTone :: Int -> Int -> Int
absoluteTone pitch octave = pitch + (octave * 12)

-- Calculate the frequency from a tone, based on A4.
absoluteFrequency :: Int -> Double
absoluteFrequency tone = baseFrequency * (2.0 ** power) where
    power = (fromIntegral $ tone - baseTone) / 12.0 :: Double

-- Return the pitch value for the given character.
pitchValue :: Char -> Int
pitchValue c = case c of
    'C' -> 0
    'D' -> 2
    'E' -> 4
    'F' -> 5
    'G' -> 7
    'A' -> 9
    'B' -> 11
    _ -> error "Unrecognized pitch character."

-- Return the pitch augmentation value of the given accidental.
accidentalValue :: Char -> Int
accidentalValue c = case c of
    '#' -> 1
    'b' -> (-1)
    _ -> 0

-- Calculate the frequency of a pitch represented as a String.
pitchToFrequency :: String -> Double
pitchToFrequency s
    | length s < 2 = error "Invalid pitch string format."
    | otherwise = absoluteFrequency (absoluteTone pitch octave) where
        hasAccidental = not $ isDigit $ s !! 1
        pitch
            | hasAccidental = pitchValue (s !! 0) + accidentalValue (s !! 1)
            | otherwise = pitchValue (s !! 0)
        octave
            | hasAccidental = read $ drop 2 s :: Int
            | otherwise = read $ drop 1 s :: Int
