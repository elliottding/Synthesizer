module TestEnvelope (test) where

import Test.Hspec
import Data.Vector as V

import TestUtil (eq, samplesShouldBeEqual)
import Envelope (ADSR(..), envelop)

samples :: V.Vector Double
samples = V.replicate 100 1

attack :: Double
attack = 5

decay :: Double
decay = 10

sustain :: Double
sustain = 0.4

release :: Double
release = 20

adsr :: ADSR
adsr = ADSR attack decay sustain release

sampleRate :: Double
sampleRate = 1

test :: SpecWith ()
test = describe "Envelope" $ do
    describe "envelop" $ do
        it "does not change the signal with base parameters" $ do
            let adsr' = ADSR 0 0 1 0
            let samples' = V.fromList [4 :: Double, 3, 2, 4, 1, 5]
            let env = envelop adsr' sampleRate samples'
            samplesShouldBeEqual samples' env

        it "linearly increases the signal over the attack duration" $ do
            let env = envelop adsr sampleRate samples
            env V.! 0 `shouldBe` 0
            env V.! 3 `eq` 0.6 `shouldBe` True
            env V.! (round attack) `shouldBe` 1
        
        it "linearly decreases the signal over the decay duration" $ do
            let env = envelop adsr sampleRate samples
            env V.! (round attack) `shouldBe` 1
            env V.! 8 `eq` 0.82 `shouldBe` True
            env V.! (round $ attack + decay) `shouldBe` sustain
        
        it "holds the signal at the sustain level" $ do
            let env = envelop adsr sampleRate samples
            env V.! (round $ attack + decay + 5) `shouldBe` sustain

        it "linearly decreases the signal over the release duration" $ do
            let env = envelop adsr sampleRate samples
            let releaseStart = env V.! (100 - round release)
            releaseStart `shouldBe` sustain
            env V.! 99 < releaseStart `shouldBe` True
