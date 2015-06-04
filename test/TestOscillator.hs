module TestOscillator (test) where

import Test.Hspec
import Data.Vector as V

import TestUtil (samplesShouldBeEqual)
import Oscillator (Oscillator(..), sample)
import qualified WaveTable as WT

test :: SpecWith ()
test = describe "Oscillator" $ do
    describe "sample" $ do
        it "is equivalent to WaveTable sample at amplitude 1 and phase 0" $ do
            let wt = V.fromList [4 :: Double, 8, 9, 3, 4, 3, 1, 5, 2]
            let osc = Oscillator wt 1 0 0
            let samples = WT.sample wt 7 5 12
            let samples' = sample osc 7 5 12
            samplesShouldBeEqual samples samples'

        it "multiplicatively adjusts levels according to amplitude" $ do
            let wt = V.fromList [4 :: Double, 8, 9, 3, 4, 3, 1, 5, 2]
            let osc = Oscillator wt 0.5 0 0
            let samples = V.map ((*) 0.5) $ WT.sample wt 7 5 12
            let samples' = sample osc 7 5 12
            samplesShouldBeEqual samples samples'

        it "outputs 0 at amplitude 0" $ do
            let wt = V.fromList [4 :: Double, 8, 9, 3, 4, 3, 1, 5, 2]
            let osc = Oscillator wt 0 31 0
            let samples = sample osc 9 1 12
            V.mapM_ (shouldBe 0) samples
