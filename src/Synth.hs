{-# LANGUAGE TemplateHaskell #-}

module Synth (Synth(..)
             , oscillators
             , amplitude
             , envelope
             , sampleRate
             , bpm
             , synthesizeNotes
             ) where

import Envelope (ADSR(..), envelop)
import Note (Note(..))
import Oscillator (Oscillator, sample)
import Samples (Samples, composeAll)

import Control.Lens (makeLenses)

import qualified Data.Vector as V

data Synth = Synth { _oscillators :: [Oscillator]
                   , _amplitude   :: Double
                   , _envelope    :: ADSR
                   , _sampleRate  :: Double
                   , _bpm         :: Double
                   } deriving (Show)

-- Represents the Samples of a processed Note, and its lower and upper sample
-- time indices.
data Processed = Processed { _samples :: V.Vector Double
                           , _lower   :: Int
                           , _upper   :: Int
                           }

makeLenses ''Synth

-- Convert a Note into Processed sample data.
processNote :: Synth -> Note -> Processed
processNote synth@(Synth _ _ (ADSR _ _ _ rel) sr b) (Note t d f) =
    Processed samples lower upper where
        n = truncate $ sr * (d * 60 / b + rel)
        samples = synthesize synth f n
        lower = truncate $ sr * t * 60 / b
        upper = lower + n

-- Index into Processed data. If the index is within range, return the sample.
-- Otherwise, return 0.
(!?) :: Processed -> Int -> Double
(Processed s l u) !? i
    | (i >= l) && (i < u) = s V.! (i - l)
    | otherwise           = 0

-- Sum all samples at the given index in all given Processed data.
sampleAt :: [Processed] -> Int -> Double
sampleAt ps i = foldr ((+) . flip (!?) i) 0 ps

-- Combine all Processed into a Vector of Doubles.
combineProcessed :: [Processed] -> V.Vector Double
combineProcessed ps = V.generate (m-1) (sampleAt ps) where
    m = foldr maxUpper 0 ps
    maxUpper (Processed _ _ u) b = max u b

-- Synthesize n samples at the provided frequency.
synthesize :: Synth -> Double -> Int -> Samples
synthesize (Synth oscs amp adsr sr _) freq n = enveloped where
    oscSamples = map (\o -> sample o sr freq n) oscs
    samples = composeAll oscSamples
    amped = V.map (amp *) samples
    enveloped = envelop adsr sr amped

-- Synthesize samples for all provided Notes.
synthesizeNotes :: Synth -> [Note] -> Samples
synthesizeNotes = (combineProcessed .) . (map . processNote)
