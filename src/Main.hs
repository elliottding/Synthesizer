module Main where

import qualified Envelope as Env
import qualified Oscillator as Osc
import qualified Synth as Syn
import Note (Note(..), pitchToFrequency)
import Play (play)

import Control.Lens ((.~), element)
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

defaultADSR :: Env.ADSR
defaultADSR = Env.ADSR defaultAttack defaultDecay defaultSustain defaultRelease

defaultVolume :: Double
defaultVolume = 1.0

defaultOscs :: [Osc.Oscillator]
defaultOscs = [Osc.sine, Osc.sawtooth, Osc.triangle]

defaultSynth :: Syn.Synth
defaultSynth = Syn.Synth defaultOscs defaultVolume defaultADSR defaultSampleRate

-- Play Notes using the provided Synth at the given sample rate.
playSynth :: Syn.Synth -> [Note] -> IO ()
playSynth synth notes = 
    play (Syn._sampleRate synth) $ Syn.synthesizeNotes synth notes

synthModifier :: [String] -> (Syn.Synth -> Syn.Synth)
synthModifier params = case arg of
    "samplerate" -> Syn.sampleRate .~ value
    "amp"        -> Syn.amplitude .~ value
    "attack"     -> Syn.envelope . Env.attack .~ value
    "decay"      -> Syn.envelope . Env.decay .~ value
    "sustain"    -> Syn.envelope . Env.sustain .~ value
    "release"    -> Syn.envelope . Env.release .~ value
    "osc"        -> oscModifier params
    _            -> id
    where
        arg = params !! 0
        value = read $ params !! 1 :: Double

oscModifier :: [String] -> (Syn.Synth -> Syn.Synth)
oscModifier params = case arg of
    "amp"   -> Syn.oscillators . element i . Osc.amplitude .~ read value
    "wave"  -> Syn.oscillators . element i . Osc.wave .~ (Osc._wave $ Osc.oscFromString value)
    "phase" -> Syn.oscillators . element i . Osc.phase .~ read value
    _       -> id
    where
        i = read $ params !! 1 :: Int
        arg = params !! 2
        value = params !! 3

playCommand :: Syn.Synth -> [String] -> IO (Syn.Synth)
playCommand synth params = do
    let n = fromIntegral $ length params
        freqs = map pitchToFrequency params
        notes = map (\(t, freq) -> Note t 1 freq) $ zip [1..n] freqs
    playSynth synth notes
    return synth

setCommand :: Syn.Synth -> [String] -> IO (Syn.Synth)
setCommand synth params = return $ synthModifier params synth

loop :: Syn.Synth -> IO ()
loop synth = do
    line <- getLine
    let params = words line
    if length params < 1 then loop synth else
        case head (words line) of
            "play" -> do
                synth' <- playCommand synth $ tail params
                loop synth'
            "set" -> do
                synth' <- setCommand synth $ tail params
                loop synth'
            "end" -> return ()
            _ -> do
                putStrLn "Unrecognized command."
                loop synth

main :: IO ()
main = loop defaultSynth
