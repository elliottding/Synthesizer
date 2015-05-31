module Note where

import Data.Char (isDigit)

data Note = Note { noteTime :: Double
                 , noteDuration :: Double
                 , noteFrequency :: Double
                 }

baseFrequency :: Double
baseFrequency = 440

basePitch :: Int
basePitch = 9

baseOctave :: Int
baseOctave = 4

baseTone :: Int
baseTone = absoluteTone basePitch baseOctave

absoluteTone :: Int -> Int -> Int
absoluteTone pitch octave = pitch + (octave * 12)

absoluteFrequency :: Int -> Int -> Double
absoluteFrequency pitch octave = baseFrequency * (2.0 ** power) where
    power = (fromIntegral $ tone - baseTone) / 12.0 :: Double
    tone = absoluteTone pitch octave

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

accidentalValue :: Char -> Int
accidentalValue c = case c of
    '#' -> 1
    'b' -> (-1)
    _ -> 0

pitchToFrequency :: String -> Double
pitchToFrequency s
    | length s < 2 = error "Invalid pitch string format."
    | otherwise = absoluteFrequency pitch octave where
        hasAccidental = not $ isDigit $ s !! 1
        pitch
            | hasAccidental = pitchValue (s !! 0) + accidentalValue (s !! 1)
            | otherwise = pitchValue (s !! 0)
        octave
            | hasAccidental = read $ drop 2 s :: Int
            | otherwise = read $ drop 1 s :: Int
