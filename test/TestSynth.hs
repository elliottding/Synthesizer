module TestSynth (test) where

import Test.Hspec
import Data.Vector as V

import TestUtil (doublesShouldBeEqual)
import Envelope (ADSR(..))
import Note (Note(..))
import Oscillator (Oscillator(..))
import WaveTable (sine)
import Synth (Synth(..), synthesizeNotes)

test :: SpecWith ()
test = describe "Synth" $ do
    describe "synthesizeNotes" $ do
        it "synthesizes a single Note" $ do
            let osc = Oscillator sine 1 0
            let adsr = ADSR 0 0 1 0
            
            -- Sample rate is 44000 Hz
            let synth = Synth [osc] 1 adsr 44000 60
            
            -- Note is 440 Hz
            let note = Note 0 1 440
            
            -- Every 100 samples, the value should be 0
            let samples = synthesizeNotes synth [note]
            let e100 = V.generate 430 ((V.!) samples . (*) 100)
            V.mapM_ (doublesShouldBeEqual 0) e100
            
            -- Every 100 samples offset by 25, the value should be 1
            let e100o25 = V.generate 430 ((V.!) samples . (+) 25 . (*) 100)
            V.mapM_ (doublesShouldBeEqual 1) e100o25

            -- Every 100 samples offset by 75, the value should be -1
            let e100o75 = V.generate 430 ((V.!) samples . (+) 75 . (*) 100)
            V.mapM_ (doublesShouldBeEqual (-1)) e100o75

        it "produces Samples of the correct length on multiple Notes" $ do
            let osc = Oscillator sine 1 0
            let adsr = ADSR 0 0 1 0
            let synth = Synth [osc] 1 adsr 1000 60
            let note = Note 0 1 100
            let note' = Note 1 2 100

            -- Total duration should be 3 seconds, or 3000 samples
            let samples = synthesizeNotes synth [note, note']
            V.length samples `shouldBe` 3000
