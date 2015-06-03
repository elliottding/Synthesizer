module TestWaveTable (test) where

import Test.Hspec
import Data.Vector as V
import System.Directory (removeFile)

import TestUtil (samplesShouldBeEqual)
import WaveTable (save, load, sample)

test :: SpecWith ()
test = describe "WaveTable" $ do
    describe "save, load" $ do
        it "WaveTables are the same after saving and loading" $ do
            let path = "test/test.wavetable.tmp"
            let wt = V.fromList [4 :: Double, 5, 1, 2]
            save path wt
            wt' <- load path
            samplesShouldBeEqual wt wt'
            removeFile path

    describe "sample" $ do
        it "is equivalent to cycle at frequency 1 and the same sample rate" $ do
            let wt = V.fromList [4 :: Double, 8, 9, 3, 4, 3, 1, 5, 2]
            let wt' = sample wt 9 1 12
            let wt'' = V.fromList [4 :: Double, 8, 9, 3, 4, 3, 1, 5, 2, 4, 8, 9]
            samplesShouldBeEqual wt' wt''

        it "resamples from the WaveTable" $ do
            let wt = V.fromList [4 :: Double, 8, 9, 3, 4, 3, 1, 5]
            let wt' = sample wt 4 2 8
            let wt'' = V.fromList [4 :: Double, 4, 4, 4, 4, 4, 4, 4]
            samplesShouldBeEqual wt' wt''
