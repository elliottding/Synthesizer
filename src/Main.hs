module Main where

-- import Control.Concurrent.Async
-- import Control.Concurrent

import BasicTypes (additive)
import Envelope (ASDR(..))
import Note (Note(..), pitchToFrequency)
import qualified Oscillator as Osc
import Play (play)
import Synth (Synth(..), synthesizeNotes)

import Control.Monad (forever)

-- import qualified Data.ByteString.Char8 as Char8

-- import System.Process (createProcess, shell, CreateProcess(..), StdStream(..))
-- import System.IO (hSetBuffering, hSetBinaryMode, hPutStrLn, hGetLine, stdin, hFlush, stdout, BufferMode(..), Handle)

-- import System.Exit (ExitCode)

-- import Data.Time
-- import Data.Time.Clock.POSIX (getPOSIXTime, POSIXTime)

{-
delay :: Int -> IO ()
delay = threadDelay . (1000 *)
-}

defaultAttack :: Double
defaultAttack = 0.200

defaultDecay :: Double
defaultDecay = 0.200

defaultSustain :: Double
defaultSustain = 0.8

defaultRelease :: Double
defaultRelease = 0.500

defaultASDR :: ASDR
defaultASDR = ASDR defaultAttack defaultDecay defaultSustain defaultRelease

defaultVolume :: Double
defaultVolume = 1.0

defaultOscs :: [Osc.Oscillator]
defaultOscs = [Osc.sine, Osc.sawtooth, Osc.triangle]

defaultSynth :: Synth
defaultSynth = Synth defaultOscs defaultVolume additive defaultASDR

main :: IO ()
main = do
    let synth = defaultSynth
    playCommand synth "play C3 D3 E3 F3 G3 A3 B3 C4"
    forever $ do
        getLine >>= playCommand synth

playCommand :: Synth -> String -> IO ()
playCommand synth command = do
    let pitches = drop 1 $ words command
    let n = fromIntegral $ length pitches :: Double
    let freqs = map pitchToFrequency pitches
    let notes = map (\(t, freq) -> Note t 1 freq) $ zip [1.0..n] freqs
    play $ synthesizeNotes synth notes

