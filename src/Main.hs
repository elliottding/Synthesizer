module Main where

import Envelope (ADSR(..))
import Note (Note(..), pitchToFrequency)
import Oscillator (Oscillator(..), sine, triangle, sawtooth, oscFromString
                  , oscWaveTable)
import Play (play)
import Synth (Synth(..), synthesizeNotes, synthSR)

import Control.Monad (forever)
import qualified Data.Vector as V

defaultSampleRate :: Double
defaultSampleRate = 44100

defaultAttack :: Double
defaultAttack = 0.200

defaultDecay :: Double
defaultDecay = 0.200

defaultSustain :: Double
defaultSustain = 0.8

defaultRelease :: Double
defaultRelease = 0.500

defaultADSR :: ADSR
defaultADSR = ADSR defaultAttack defaultDecay defaultSustain defaultRelease

defaultVolume :: Double
defaultVolume = 1.0

defaultOscs :: [Oscillator]
defaultOscs = [sine, sawtooth, triangle]

defaultSynth :: Synth
defaultSynth = Synth defaultOscs defaultVolume defaultADSR defaultSampleRate

-- Play Notes using the provided Synth at the given sample rate.
playSynth :: Synth -> [Note] -> IO ()
playSynth synth notes = play (synthSR synth) $ synthesizeNotes synth notes

main :: IO ()
main = do
    let synth = defaultSynth
    playCommand synth "play C4 D4 E4 F4 G4 A4 B4 C5"
    loop synth

playCommand :: Synth -> String -> IO (Synth)
playCommand synth line = do
    let pitches = drop 1 $ words line
        n = fromIntegral $ length pitches :: Double
        freqs = map pitchToFrequency pitches
        notes = map (\(t, freq) -> Note t 1 freq) $ zip [1.0..n] freqs
    playSynth synth notes
    return synth

setCommand :: Synth -> String -> IO (Synth)
setCommand synth@(Synth oscs amp adsr sr) line = do
    let params = words line
    case params !! 1 of
        "amp" -> return $ Synth oscs (read $ params !! 2 :: Double) adsr sr
        "osc" -> setOscCommand synth line
        _ -> return synth

oscModifier :: String -> String -> Oscillator -> Oscillator
oscModifier arg value osc@(Oscillator wt amp phase) = case arg of
    "amp"   -> Oscillator wt (read value :: Double) phase
    "phase" -> Oscillator wt amp (read value :: Double)
    "wave"  -> Oscillator (oscWaveTable $ oscFromString value) amp phase
    _       -> osc

setOscCommand :: Synth -> String -> IO (Synth)
setOscCommand synth@(Synth oscs _ _ _) line = do
    let params = words line
    let index = read $ params !! 2 :: Int
    let arg = params !! 3
    let value = params !! 4
    return $ modifyOsc synth index (oscModifier arg value)

modifyOsc :: Synth -> Int -> (Oscillator -> Oscillator) -> Synth
modifyOsc (Synth oscs amp adsr sr) index f = Synth oscs' amp adsr sr where
    (xs, ys) = splitAt index oscs
    osc' = f $ last xs
    oscs' = (init xs) ++ (osc' : ys)

loop :: Synth -> IO ()
loop synth = do
    line <- getLine
    let params = words line
    if length params < 1 then loop synth else
        case head (words line) of
            "play" -> do
                synth' <- playCommand synth line
                loop synth'
            "set" -> do
                synth' <- setCommand synth line
                loop synth'
            "end" -> return ()
            _ -> do
                putStrLn "Unrecognized command."
                loop synth

