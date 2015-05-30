module Main where

import Control.Concurrent.Async
import Control.Concurrent

import Sound

import qualified WaveTable as WT

import qualified Synthesizer.Basic.Wave as Wave
import qualified Synthesizer.Plain.Control as Ctrl
import qualified Synthesizer.Plain.Filter.NonRecursive as Filt
import qualified Synthesizer.Plain.Filter.Recursive as FiltRec
import qualified Synthesizer.Plain.Filter.Recursive.Universal as UniFilter
import qualified Synthesizer.Plain.Oscillator as Osci
import qualified Synthesizer.Plain.Play as Play
import qualified Synthesizer.Plain.Signal as Sig

import qualified Data.ByteString.Char8 as Char8

import System.Process (createProcess, shell, CreateProcess(..), StdStream(..))
import System.IO (hSetBuffering, hSetBinaryMode, hPutStrLn, hGetLine, stdin, hFlush, stdout, BufferMode(..), Handle)

--import qualified Graphics.UI.SDL.Events as Events

import System.Exit (ExitCode)

import Data.Time
import Data.Time.Clock.POSIX (getPOSIXTime, POSIXTime)

delay :: Int -> IO ()
delay = threadDelay . (1000 *)

{-
loop :: IO ()
loop = do
    let basicSound = signalToSound $ sine 200
    c <- getChar
    basicSound <- play basicSound
    threadDelay 1000000
    _ <- stop basicSound
    loop
-}
main :: IO ()
main = play $ WT.sampleDuration WT.sawtooth 441 2

loop :: Handle -> POSIXTime -> Bool -> IO ()
loop handle time True = do
    now <- getPOSIXTime
    --putStrLn $ show now
    let elapsed = realToFrac $ now - time :: Double
    Char8.hPutStr handle $ prepareSound $ sine 44100 440 1
    loop handle now True

loop handle time False = do
    now <- getPOSIXTime
    let elapsed = realToFrac $ now - time :: Double
    Char8.hPutStr handle $ prepareSound $ sine 44100 440 elapsed
    loop handle now True

{-
-- Play a signal at a sample rate of 44100 Hz.
play :: Sig.T Double -> IO ExitCode
play = Play.monoToInt16 (44100 :: Double)


oscillator :: IO ExitCode
oscillator = play (Osci.static Wave.sine 0 (0.02 :: Double))

-- sine :: Int -> IO ExitCode
-- sine t = play $ take (t * 44100) $ map sin [0 :: Double, 0.1..]

frequencyToPhase :: Double -> Double -> Double
frequencyToPhase sampleRate frequency = frequency * 2 * pi / sampleRate

frequencyToPhaseList :: Double -> Double -> [Double]
frequencyToPhaseList sampleRate frequency = [0 :: Double, phase..]
    where phase = frequencyToPhase sampleRate frequency

sine :: Double -> Double -> Double -> IO ExitCode
sine sampleRate frequency duration =
    let sampleCount = round $ duration * sampleRate / 1000.0
        phaseList = frequencyToPhaseList sampleRate frequency
    in play $ take sampleCount $ map sin phaseList

pingSig :: Sig.T Double
pingSig =
    Filt.envelope (Ctrl.exponential 50000 1) (Osci.static Wave.sine 0 (0.01::Double))

fmPing :: IO ExitCode
fmPing =
    play (Osci.phaseMod Wave.sine (0.01::Double) $ map (2*) pingSig)

filterSaw :: IO ExitCode
filterSaw =
    play (map UniFilter.lowpass $ UniFilter.run (map (\f -> UniFilter.parameter $ FiltRec.Pole 10 (0.04+0.02*f)) $ Osci.static Wave.sine 0 (0.00001::Double)) $ Osci.static Wave.saw 0 (0.002::Double))
-}