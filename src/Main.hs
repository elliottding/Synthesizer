module Main where

import BasicTypes (additive)
import Envelope (ASDR(..))
import Note (Note(..), pitchToFrequency)
import Oscillator (Oscillator(..), sine, triangle, sawtooth)
import Play (play)
import Synth (Synth(..), synthesizeNotes)

import Control.Monad (forever)

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

defaultOscs :: [Oscillator]
defaultOscs = [sine, sawtooth, triangle]

defaultSynth :: Synth
defaultSynth = Synth defaultOscs defaultVolume additive defaultASDR

main :: IO ()
main = do
    let synth = defaultSynth
    playCommand synth "play C3 D3 E3 F3 G3 A3 B3 C4"
    loop synth

playCommand :: Synth -> String -> IO (Synth)
playCommand synth line = do
    let pitches = drop 1 $ words line
        n = fromIntegral $ length pitches :: Double
        freqs = map pitchToFrequency pitches
        notes = map (\(t, freq) -> Note t 1 freq) $ zip [1.0..n] freqs
    play $ synthesizeNotes synth notes
    return synth

setCommand :: Synth -> String -> IO (Synth)
setCommand synth@(Synth oscs vol method asdr) line = do
    let params = words line
    case params !! 1 of
        "volume" -> return $ Synth oscs (read $ params !! 2 :: Double) method asdr
        "osc" ->
        _ -> return synth

setOscCommand :: Synth -> String -> IO (Synth)

modifyOsc :: Synth -> Int -> (Oscillator -> Oscillator) -> Synth
modifyOsc (Synth oscs vol m asdr) index f = Synth oscs' vol m asdr where
    (xs, ys) = splitAt index oscs
    osc' = f $ last xs
    oscs' = (init xs) ++ (osc' : ys)

loop :: Synth -> IO ()
loop synth = do
    line <- getLine
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

