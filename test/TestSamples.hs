module TestSamples (test) where

import Test.Hspec
import Data.Vector as V

import TestUtil (samplesShouldBeEqual)
import Samples (zipWithPadding, zipWithOffset)

test :: SpecWith ()
test = describe "Samples" $ do
    describe "zipWithPadding" $ do
        it "is equivalent to zipWith on two vectors of equal length" $ do
            let xs = V.fromList [1.0, 2, 3]
            let ys = V.fromList [3.0, 1, 9]
            let zs = zipWithPadding (+) 5 5 xs ys
            let zs' = V.zipWith (+) xs ys
            samplesShouldBeEqual zs zs'

        it "right pads the shorter vector with the padding element" $ do
            let xs = V.fromList [1.0, 2, 3, 4, 5]
            let ys = V.fromList [3.0, 5, 2]
            let zs = zipWithPadding (*) 0 8 xs ys
            let zs' = V.fromList [3.0, 10, 6, 32, 40]
            samplesShouldBeEqual zs zs'

    describe "zipWithOffset" $ do
        it "is equivalent to zipWithPadding when offsets are 0" $ do
            let xs = V.fromList [1.0, 2, 3, 4]
            let ys = V.fromList [3.0, 1, 9]
            let zs = zipWithOffset (+) 5 5 0 0 xs ys
            let zs' = zipWithPadding (+) 5 5 xs ys
            samplesShouldBeEqual zs zs'

        it "left pads vectors by the offsets with the padding element" $ do
            let xs = V.fromList [1.0, 2, 3, 4]
            let ys = V.fromList [3.0, 1, 9]
            let zs = zipWithOffset (+) 5 4 1 4 xs ys
            let zs' = V.fromList [9.0, 5, 6, 7, 7, 6, 14]
            samplesShouldBeEqual zs zs'
