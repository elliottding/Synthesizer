module TestMidi (test) where

import Test.Hspec
import Control.Lens ((^.))
import Control.Monad (zipWithM_)

import TestUtil (doublesShouldBeEqual)
import Note (Note(..), frequency, pitchToFrequency)
import Midi (loadMidiNotes)

noteShouldBe :: Note -> String -> IO ()
noteShouldBe note p = 
    doublesShouldBeEqual (note ^. frequency) (pitchToFrequency p)

test :: SpecWith ()
test = describe "Midi" $ do
    describe "loadMidiNotes" $ do
        it "loads a .midi file" $ do
            _ <- loadMidiNotes "test/c.midi"
            return ()

        it "reads and creates Notes from a .midi file" $ do
            notes <- loadMidiNotes "test/c.midi"
            zipWithM_ noteShouldBe notes ["C4", "D4", "E4", "F4"]
