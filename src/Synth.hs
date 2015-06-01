module Synth where

import Envelope (ADSR, envelop, adsrRelease)
import Oscillator (Oscillator, sample)
import Note (Note(..))
import Samples (Samples(..), composeAll, zipWithOffset)
-- import Util (zipWithOffset, zipWithPadding)

import qualified Data.Vector as V

data Synth = Synth { synthOsc :: [Oscillator]
                   , synthAmp :: Double
                   , synthEnv :: ADSR
                   , synthSR :: Double
                   }

synthesize :: Synth -> Double -> Int -> Samples
synthesize (Synth oscs amp adsr sr) freq n = enveloped where
    oscSamples = map (\o -> sample o sr freq n) oscs
    samples = composeAll oscSamples
    amped = V.map (amp *) samples
    enveloped = envelop adsr sr amped

synthesizeDuration :: Synth -> Double -> Double -> Samples
synthesizeDuration synth freq secs = synthesize synth freq n where
    n = truncate $ secs * (synthSR synth)

synthesizeNote :: Synth -> Note -> Samples
synthesizeNote synth (Note _ duration freq) = 
    synthesizeDuration synth freq secs where 
        secs = duration + (adsrRelease (synthEnv synth))

combineNote :: Synth -> Note -> Samples -> Samples
combineNote synth note@(Note time duration freq) samples =
    zipWithOffset (+) 0 0 0 offset samples synthesized where
        offset = truncate $ time * (synthSR synth)
        synthesized = synthesizeNote synth note

synthesizeNotes :: Synth -> [Note] -> Samples
synthesizeNotes synth notes = foldr (combineNote synth) V.empty notes
