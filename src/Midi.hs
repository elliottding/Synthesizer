module Midi (Midi
            , toNotes
            ) where

import Note (Note(..))

import Codec.Midi (Midi(..), Track(..), Ticks(..), Message(..), Key(..))
import Data.List (sortBy)

instance Ord Message where
    compare (NoteOn  _ k1 _) (NoteOn  _ k2 _) = compare k1 k2
    compare (NoteOn  _ k1 _) (NoteOff _ k2 _) = compare k1 k2
    compare (NoteOff _ k1 _) (NoteOn  _ k2 _) = compare k1 k2
    compare (NoteOff _ k1 _) (NoteOff _ k2 _) = compare k1 k2
    compare _                _                = EQ

instance Ord a => Ord Track a where
    compare (a1, m1) (a2, m2)
        | cm == EQ  = compare a1 a2
        | otherwise = cm
        where cm = compare m1 m2

toNotes :: Midi -> [Note]
toNotes (Midi _ tpb tracks) = sort tracks

keyToFrequency :: Key -> Double
keyToFrequency k = 0

eventsToNote :: Track Ticks -> Track Ticks -> Note
eventsToNote (t1, (NoteOn _ k _)) (t2, (NoteOff _ _ _)) = 
    Note time duration freq where
        time = t1
        duration = t2 - t1
        freq = keyToFrequency k
