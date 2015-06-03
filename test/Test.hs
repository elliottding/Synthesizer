module Main (main) where

import Test.Hspec

import qualified TestEnvelope
import qualified TestNote
import qualified TestOscillator
import qualified TestSamples
import qualified TestSynth
import qualified TestWaveTable

main :: IO ()
main = hspec $ describe "Synthesizer" $ do
    TestEnvelope.test
    TestNote.test
    TestOscillator.test
    TestSamples.test
    TestSynth.test
    TestWaveTable.test
