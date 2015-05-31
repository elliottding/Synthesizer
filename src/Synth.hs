module Synth where

import BasicTypes (Sample, Synthesis, Frequency, additive, defaultSampleRate)
import Envelope (ASDR, envelop, asdrRelease)
import Oscillator (Oscillator, sample)
import Note (Note(..))
import Util (zipWithOffset)

data Synth = Synth { synthOsc :: [Oscillator]
                   , synthVol :: Double
                   , synthMethod :: Synthesis
                   , synthEnvelope :: ASDR
                   }

synthesize :: Synth -> Frequency -> Int -> [Sample]
synthesize (Synth oscs vol method asdr) freq n = enveloped where
    oscSamples = fmap (\o -> sample o freq n) oscs
    samples = foldr method [] oscSamples
    volumed = map (vol *) samples
    enveloped = envelop asdr defaultSampleRate volumed

synthesizeDuration :: Synth -> Frequency -> Double -> [Sample]
synthesizeDuration synth freq secs = synthesize synth freq n where
    n = truncate $ secs * defaultSampleRate

synthesizeNote :: Synth -> Note -> [Sample]
synthesizeNote synth (Note _ duration freq) = 
    synthesizeDuration synth freq secs where 
        secs = duration + (asdrRelease (synthEnvelope synth))

combineNote :: Synth -> Note -> [Sample] -> [Sample]
combineNote synth note@(Note time duration freq) samples =
    zipWithOffset (+) 0 offset samples synthesized where
        offset = truncate $ time * defaultSampleRate
        synthesized = synthesizeNote synth note

synthesizeNotes :: Synth -> [Note] -> [Sample]
synthesizeNotes synth notes = foldr (combineNote synth) [] notes
