module TestUtil (eq, samplesShouldBeEqual, doublesShouldBeEqual) where

import Test.Hspec (shouldBe)
import qualified Data.Vector as V

import Samples (Samples)

tolerance :: Double
tolerance = 0.01

-- Compare Doubles given an error tolerance.
eq' :: Double -> Double -> Double -> Bool
eq' e a b = e > abs (a - b)

eq :: Double -> Double -> Bool
eq a b = eq' tolerance a b

-- Test if Doubles are (roughly) equal.
doublesShouldBeEqual :: Double -> Double -> IO ()
doublesShouldBeEqual a b = (eq a b) `shouldBe` True

-- Test if all elements in two Samples are pairwise equal.
samplesShouldBeEqual :: Samples -> Samples -> IO ()
samplesShouldBeEqual = V.zipWithM_ doublesShouldBeEqual
