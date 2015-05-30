module Sound (play) where

import BasicTypes (Sample, defaultSampleRate)
import WaveTable (WaveTable)

import qualified Data.ByteString.Char8 as Char8

import System.Process (createProcess, shell, CreateProcess(..), StdStream(..))
import System.IO (hSetBuffering, hSetBinaryMode, BufferMode(..), Handle)

-- Map from [-1.0, 1.0] to [0, 255] to a Char.
doubleToChar8 :: Double -> Char
doubleToChar8 x = toEnum $ truncate $ (x + 1) * 127.5

-- Prepare samples for playback by converting to the correct byte format.
prepareSamples :: [Sample] -> Char8.ByteString
prepareSamples = Char8.pack . map doubleToChar8

-- The SoX command for sound playback.
soxCommand :: String
soxCommand = "play -c 2 -b 8 -e unsigned -t raw -r "
           ++ (show defaultSampleRate)
           ++ " -"

-- Play the samples using SoX.
play :: [Sample] -> IO ()
play samples = do
    let process = (shell soxCommand) { std_in = CreatePipe }
    (Just handle, _, _, _) <- createProcess process
    hSetBuffering handle NoBuffering
    hSetBinaryMode handle True
    Char8.hPutStr handle $ prepareSamples $ samples
