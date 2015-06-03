module TestNote (test) where

import Test.Hspec
import TestUtil (eq)
import Note (pitchToFrequency)

-- Test if the calculated frequency for the pitch is equal to the actual
-- documented frequency.
testPitchToFrequency :: String -> Double -> IO ()
testPitchToFrequency pitch freq = 
    (pitchToFrequency pitch `eq` freq) `shouldBe` True

test :: SpecWith ()
test = describe "Note" $ do
    describe "pitchToFrequency" $ do
        it "returns the frequency of A4 at concert pitch (440 Hz)" $ do
            testPitchToFrequency "A4" 440.00
        
        it "calculates the frequency of other notes" $ do
            testPitchToFrequency "C4" 261.626
            testPitchToFrequency "E6" 1318.51
        
        it "calculates the frequency of notes with accidentals" $ do
            testPitchToFrequency "C#7" 2217.46
            testPitchToFrequency "Ab1" 51.9131
