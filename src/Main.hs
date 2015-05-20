module Main where

import qualified Synthesizer.Plain.Play as Play

import System.Exit (ExitCode)

main :: IO ExitCode
main = do
    putStrLn $ "Playing sine wave..."
    Play.monoToInt16 (44100::Double) (map sin [0::Double,0.1..])
