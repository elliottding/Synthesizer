module Main where

import qualified Envelope as Env
import qualified Oscillator as Osc
import qualified Synth as Syn
import qualified WaveTable as WT
import Note (Note(..), pitchToFrequency)
import Play (play)

import Control.Lens ((.~), (^.), (%~), element)
import Control.Monad (forever)

import Data.List (isSuffixOf)
import qualified Data.Map as M
import qualified Data.Vector as V

import System.Directory (createDirectoryIfMissing
                        , getDirectoryContents
                        , doesFileExist
                        )

type WaveTableMap = M.Map String WT.WaveTable

defaultSampleRate :: Double
defaultSampleRate = 44100

defaultAttack :: Double
defaultAttack = 0.050

defaultDecay :: Double
defaultDecay = 0.050

defaultSustain :: Double
defaultSustain = 1.0

defaultRelease :: Double
defaultRelease = 0.100

defaultADSR :: Env.ADSR
defaultADSR = Env.ADSR defaultAttack defaultDecay defaultSustain defaultRelease

defaultVolume :: Double
defaultVolume = 1.0

defaultOscs :: [Osc.Oscillator]
defaultOscs = [Osc.sine]

defaultSynth :: Syn.Synth
defaultSynth = Syn.Synth defaultOscs defaultVolume defaultADSR defaultSampleRate

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

-- Load all WaveTables into a Map of name, table pairs.
loadWaveTables :: IO WaveTableMap
loadWaveTables = do
    allFiles <- getDirectoryContents "./data"
    let wtFiles = map ((++) "./data/") $ filter (isSuffixOf ".wavetable") allFiles
    pairs <- mapM loadWaveTable wtFiles
    return $ M.fromList pairs

-- Generate the default WaveTable if missing.
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

-- Return the WaveTable with the given name from the map; error if not found.
waveTableLookup :: String -> WaveTableMap -> WT.WaveTable
waveTableLookup name wtMap = case M.lookup name wtMap of
    Just wt -> wt
    Nothing -> error "Invalid WaveTable name."

-- Return a Synth modifier from the parsed parameters.
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

-- Return an Oscillator modifier from the parsed parameters.
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

-- Handle a play command.
playCommand :: Syn.Synth -> [String] -> IO ()
playCommand synth params = do
    let n = fromIntegral $ length params
        freqs = map pitchToFrequency params
        notes = map (\(t, freq) -> Note t 1 freq) $ zip [1..n] freqs
    playSynth synth notes

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