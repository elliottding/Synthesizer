module Midi (loadMidiNotes) where

import Note (Note(..), absoluteFrequency)

import Codec.Midi (Midi(..), Track, Ticks, Message(..), TimeDiv(..), Key, importFile)
import qualified Data.Map as M

-- Convert a Midi structure into a list of Notes.
toNotes :: Midi -> [Note]
toNotes (Midi _ (TicksPerBeat tpb) ts) = 
    concatMap (map (adjustTimes tpb) . trackToNotes) ts
toNotes (Midi _ (TicksPerSecond tpb _) ts) =
    concatMap (map (adjustTimes tpb) . trackToNotes) ts

-- Adjust times of a Note by the given ticks per beat parameter.
adjustTimes :: Ticks -> Note -> Note
adjustTimes tpb (Note t d f) = Note (adjust t) (adjust d) f where
    adjust x = x / (fromIntegral tpb)

-- Return the frequency corresponding to the given Midi Key.
keyToFrequency :: Key -> Double
keyToFrequency k = absoluteFrequency (k - 12)

-- Convert a Midi track to a list of Notes.
trackToNotes :: Track Ticks -> [Note]
trackToNotes = (\(_, _, r) -> r) . foldl processEvent (0, M.empty, [])

-- Load Notes from a Midi file.
loadMidiNotes :: FilePath -> IO [Note]
loadMidiNotes path = do
    eMidi <- importFile path
    case eMidi of
        Right midi -> return $ toNotes midi
        Left  msg  -> error msg

-- Create a Note from the given times in Ticks.
makeNote :: Ticks -> Ticks -> Key -> Note
makeNote t t' k = Note time dur freq where
    time = fromIntegral t
    dur = fromIntegral (t' - t)
    freq = keyToFrequency k

-- Process a Midi event.
processEvent :: (Ticks, M.Map Key Ticks, [Note]) -> (Ticks, Message) -> (Ticks, M.Map Key Ticks, [Note])
processEvent (time, keyMap, notes) (offset, m) = case m of
    NoteOn  _ k _ -> case M.lookup k keyMap of
        Just _ -> (newTime, keyMap, notes)
        Nothing    -> (newTime, M.insert k newTime keyMap, notes)
    NoteOff _ k _ -> case M.lookup k keyMap of
        Just stime -> (newTime, M.delete k keyMap, notes ++ [makeNote stime newTime k])
        Nothing    -> (newTime, keyMap, notes)
    _             -> (newTime, keyMap, notes)
    where
        newTime = time + offset
