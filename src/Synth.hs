{-# LANGUAGE TemplateHaskell #-}

module Synth (Synth(..)
             , oscillators
             , amplitude
             , envelope
             , sampleRate
             , synthesizeNotes
             ) where

import Envelope (ADSR, envelop, release)
import Note (Note(..))
import Oscillator (Oscillator, sample)
import Samples (Samples, composeAll, zipWithOffset)

import Control.Lens (makeLenses, (^.))

import qualified Data.Vector as V

data Synth = Synth { _oscillators :: [Oscillator]
                   , _amplitude :: Double
                   , _envelope :: ADSR
                   , _sampleRate :: Double
                   } deriving (Show)

makeLenses ''Synth

-- Synthesize n samples at the provided frequency.
synthesize :: Synth -> Double -> Int -> Samples
synthesize (Synth oscs amp adsr sr) freq n = enveloped where
    oscSamples = map (\o -> sample o sr freq n) oscs
    samples = composeAll oscSamples
    amped = V.map (amp *) samples
    enveloped = envelop adsr sr amped

-- Synthesize samples at the provided frequency for the provided duration in
-- seconds.
synthesizeDuration :: Synth -> Double -> Double -> Samples
synthesizeDuration synth freq secs = synthesize synth freq n where
    n = truncate $ secs * (synth ^. sampleRate)

-- Synthesize samples for a given Note.
synthesizeNote :: Synth -> Note -> Samples
synthesizeNote synth (Note _ duration freq) = 
    synthesizeDuration synth freq secs where 
        secs = duration + (synth ^. envelope . release)

-- Combine samples with the synthesized samples from a Note.
combineNote :: Synth -> Note -> Samples -> Samples
combineNote synth note@(Note time _ _) samples =
    zipWithOffset (+) 0 0 0 offset samples synthesized where
        offset = truncate $ time * (synth ^. sampleRate)
        synthesized = synthesizeNote synth note

-- Synthesize samples for all provided Notes.
synthesizeNotes :: Synth -> [Note] -> Samples
synthesizeNotes synth notes = foldr (combineNote synth) V.empty notes
