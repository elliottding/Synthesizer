module BasicTypes where

import Util (zipWithPadding)

type SampleRate = Double

defaultSampleRate :: SampleRate
defaultSampleRate = 44100

type Frequency = Double

type Sample = Double

type Synthesis = [Sample] -> [Sample] -> [Sample]

additive :: Synthesis
additive = zipWithPadding (+) 0
