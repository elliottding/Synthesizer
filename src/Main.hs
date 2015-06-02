module Main where

import qualified Envelope as Env
import qualified Oscillator as Osc
import qualified Synth as Syn
import qualified WaveTable as WT
import Note (Note(..), pitchToFrequency)
import Play (play)

import Control.Lens ((.~), (^.), element)
import Control.Monad (forever)

import Data.List (isSuffixOf)
import qualified Data.Map as M
import qualified Data.Vector as V

import System.Directory (createDirectoryIfMissing, getDirectoryContents, doesFileExist)

type WaveTableMap = M.Map String WT.WaveTable

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

-- Load a WaveTable and return a name, table pair.
loadWaveTable :: String -> IO (String, WT.WaveTable)
loadWaveTable path = do
    wt <- WT.load path
    let name = take (length path - length ".wavetable") path
    return (name, wt)

-- Load all WaveTables into a Map of name, table pairs.
loadWaveTables :: IO WaveTableMap
loadWaveTables = do
    allFiles <- getDirectoryContents "./data"
    let wtFiles = map ((++) "./data/") $ filter (isSuffixOf ".wavetable") allFiles
    pairs <- mapM loadWaveTable wtFiles
    return $ M.fromList pairs

generateWaveTableIfMissing :: String -> IO ()
generateWaveTableIfMissing name = do
    let path = "./data/" ++ name ++ ".wavetable"
    missing <- doesFileExist path
    if missing
        then WT.save path (WT.fromString name)
        else return ()

-- Generate default WaveTables.
generateDefaultWaveTables :: IO ()
generateDefaultWaveTables = do
    createDirectoryIfMissing True "./data"
    generateWaveTableIfMissing "sine"
    generateWaveTableIfMissing "triangle"
    generateWaveTableIfMissing "sawtooth"

waveTableLookup :: String -> WaveTableMap -> WT.WaveTable
waveTableLookup name wtMap = case M.lookup name wtMap of
    Just wt -> wt
    Nothing -> error "Invalid WaveTable name."

-- Play Notes using the provided Synth at the given sample rate.
playSynth :: Syn.Synth -> [Note] -> IO ()
playSynth synth notes = 
    play (synth ^. Syn.sampleRate) $ Syn.synthesizeNotes synth notes

synthModifier :: WaveTableMap -> [String] -> (Syn.Synth -> Syn.Synth)
synthModifier wtMap params = case arg of
    "samplerate" -> Syn.sampleRate .~ value
    "amp"        -> Syn.amplitude .~ value
    "attack"     -> Syn.envelope . Env.attack .~ value
    "decay"      -> Syn.envelope . Env.decay .~ value
    "sustain"    -> Syn.envelope . Env.sustain .~ value
    "release"    -> Syn.envelope . Env.release .~ value
    "osc"        -> oscModifier wtMap params
    _            -> id
    where
        arg = params !! 0
        value = read $ params !! 1 :: Double

oscModifier :: WaveTableMap -> [String] -> (Syn.Synth -> Syn.Synth)
oscModifier wtMap params = case arg of
    "amp"   -> Syn.oscillators . element i . Osc.amplitude .~ read value
    "wave"  -> Syn.oscillators . element i . Osc.wave .~ (waveTableLookup value wtMap) 
    "phase" -> Syn.oscillators . element i . Osc.phase .~ read value
    _       -> id
    where
        i = read $ params !! 1 :: Int
        arg = params !! 2
        value = params !! 3

playCommand :: Syn.Synth -> WaveTableMap -> [String] -> IO Syn.Synth
playCommand synth _ params = do
    let n = fromIntegral $ length params
        freqs = map pitchToFrequency params
        notes = map (\(t, freq) -> Note t 1 freq) $ zip [1..n] freqs
    playSynth synth notes
    return synth

setCommand :: Syn.Synth -> WaveTableMap -> [String] -> IO Syn.Synth
setCommand synth wtMap params = return $ synthModifier wtMap params synth

loop :: Syn.Synth -> WaveTableMap -> IO ()
loop synth wtMap = do
    line <- getLine
    let params = words line
    if length params < 1
        then loop synth wtMap 
        else case head (words line) of
            "play" -> do
                synth' <- playCommand synth wtMap $ tail params
                loop synth' wtMap
            "set" -> do
                synth' <- setCommand synth wtMap $ tail params
                loop synth' wtMap
            "end" -> return ()
            _ -> do
                putStrLn "Unrecognized command."
                loop synth wtMap

main :: IO ()
main = do
    generateDefaultWaveTables
    wtMap <- loadWaveTables
    loop defaultSynth wtMap
