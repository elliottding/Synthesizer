module Main where

import qualified Envelope as Env
import qualified Midi as Midi
import qualified Oscillator as Osc
import qualified Synth as Syn
import qualified WaveTable as WT
import Note (Note(..), pitchToFrequency)
import Play (play, output)

import Control.Lens ((.~), (^.), (%~), element)

import Data.List (isSuffixOf)
import qualified Data.Map as M

import System.Directory (createDirectoryIfMissing
                        , getDirectoryContents
                        , doesFileExist
                        )

type WaveTableMap = M.Map String WT.WaveTable

defaultBPM :: Double
defaultBPM = 100

defaultSampleRate :: Double
defaultSampleRate = 48000

defaultAttack :: Double
defaultAttack = 0

defaultDecay :: Double
defaultDecay = 0

defaultSustain :: Double
defaultSustain = 1

defaultRelease :: Double
defaultRelease = 0

defaultADSR :: Env.ADSR
defaultADSR = Env.ADSR defaultAttack defaultDecay defaultSustain defaultRelease

defaultVolume :: Double
defaultVolume = 1

defaultOscs :: [Osc.Oscillator]
defaultOscs = [Osc.sine]

defaultSynth :: Syn.Synth
defaultSynth = Syn.Synth defaultOscs defaultVolume defaultADSR defaultSampleRate defaultBPM

-- Play Notes using the provided Synth at the given sample rate.
playSynth :: Syn.Synth -> [Note] -> IO ()
playSynth synth notes = 
    play (synth ^. Syn.sampleRate) $ Syn.synthesizeNotes synth notes

-- Load a WaveTable and return a name, table pair.
loadWaveTable :: String -> IO (String, WT.WaveTable)
loadWaveTable path = do
    wt <- WT.load path
    let name = drop (length "./data/") $ take (length path - length ".wavetable") path
    return (name, wt)

-- Load a WaveTable from an image and return a name, table pair.
loadWaveTableImage :: String -> IO (String, WT.WaveTable)
loadWaveTableImage path = do
    wt <- WT.loadFromImage path
    let name = drop (length "./data/") $ take (length path - length ".png") path
    return (name, wt)

-- Load all WaveTables into a Map of name, table pairs.
loadWaveTables :: IO WaveTableMap
loadWaveTables = do
    allFiles <- getDirectoryContents "./data"
    let wtFiles = map ((++) "./data/") $ filter (isSuffixOf ".wavetable") allFiles
    let pngFiles = map ((++) "./data/") $ filter (isSuffixOf ".png") allFiles
    pairs <- mapM loadWaveTable wtFiles
    pairs' <- mapM loadWaveTableImage pngFiles
    return $ M.fromList (pairs ++ pairs')

-- Generate the default WaveTable if missing.
generateWaveTableIfMissing :: String -> IO ()
generateWaveTableIfMissing name = do
    let path = "./data/" ++ name ++ ".wavetable"
    missing <- doesFileExist path
    if not missing
        then WT.save path (WT.fromString name)
        else return ()

-- Generate default WaveTables.
generateDefaultWaveTables :: IO ()
generateDefaultWaveTables = do
    createDirectoryIfMissing True "./data"
    generateWaveTableIfMissing "sine"
    generateWaveTableIfMissing "triangle"
    generateWaveTableIfMissing "sawtooth"

-- Return the WaveTable with the given name from the map; error if not found.
waveTableLookup :: String -> WaveTableMap -> WT.WaveTable
waveTableLookup name wtMap = case M.lookup name wtMap of
    Just wt -> wt
    Nothing -> error "Invalid WaveTable name."

-- Return a Synth modifier from the parsed parameters.
synthModifier :: WaveTableMap -> [String] -> (Syn.Synth -> Syn.Synth)
synthModifier wtMap params = case arg of
    "samplerate" -> Syn.sampleRate .~ value
    "bpm"        -> Syn.bpm .~ value
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

-- Return an Oscillator modifier from the parsed parameters.
oscModifier :: WaveTableMap -> [String] -> (Syn.Synth -> Syn.Synth)
oscModifier wtMap params = case arg of
    "amp"   -> Syn.oscillators . element i . Osc.amplitude .~ read value
    "wave"  -> Syn.oscillators . element i . Osc.wave .~ (waveTableLookup value wtMap) 
    "phase" -> Syn.oscillators . element i . Osc.phase .~ read value
    "pitch" -> Syn.oscillators . element i . Osc.pitch .~ read value
    _       -> id
    where
        i = read $ params !! 1 :: Int
        arg = params !! 2
        value = params !! 3

-- Handle a play command.
playCommand :: Syn.Synth -> [String] -> IO ()
playCommand synth params = case head params of
    "midi" -> do
        let path = params !! 1
        notes <- Midi.loadMidiNotes path
        playSynth synth notes
    _      -> do
        let n = fromIntegral $ length params
            freqs = map pitchToFrequency params
            notes = map (\(t, freq) -> Note t 1 freq) $ zip [1..n] freqs
        playSynth synth notes

-- Handle an output command.
outputCommand :: Syn.Synth -> [String] -> IO ()
outputCommand synth params = case head params of
    "midi" -> do
        let inPath = params !! 1
        let outPath = params !! 2
        notes <- Midi.loadMidiNotes inPath
        output outPath (synth ^. Syn.sampleRate) $ Syn.synthesizeNotes synth notes
    _      -> do
        let pitches = init params
            n = fromIntegral $ length pitches
            freqs = map pitchToFrequency pitches
            notes = map (\(t, freq) -> Note t 1 freq) $ zip [1..n] freqs
            path = last params
        output path (synth ^. Syn.sampleRate) $ Syn.synthesizeNotes synth notes

-- Handle an add command.
addCommand :: WaveTableMap -> [String] -> Syn.Synth -> Syn.Synth
addCommand wtMap params = case params !! 0 of
    "osc" -> let osc' = [Osc.makeDefault $ waveTableLookup (params !! 1) wtMap]
             in Syn.oscillators %~ flip (++) osc'
    _     -> id

-- Main execution loop.
loop :: Syn.Synth -> WaveTableMap -> IO ()
loop synth wtMap = do
    line <- getLine
    let params = words line
    if length params < 1
        then loop synth wtMap 
        else case head (words line) of
            "play" -> do
                playCommand synth $ tail params
                loop synth wtMap
            "set" -> do
                let synth' = synthModifier wtMap (tail params) synth
                loop synth' wtMap
            "add" -> do
                let synth' = addCommand wtMap (tail params) synth
                loop synth' wtMap
            "output" -> do
                outputCommand synth $ tail params
                loop synth wtMap
            "end" -> return ()
            _ -> do
                putStrLn "Unrecognized command."
                loop synth wtMap

-- Main program entry point.
main :: IO ()
main = do
    generateDefaultWaveTables
    wtMap <- loadWaveTables
    mapM_ (putStrLn . (++) "Loaded wavetable: ") (M.keys wtMap)
    loop defaultSynth wtMap
