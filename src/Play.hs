module Play (play, output) where

import qualified Samples as S

import qualified Data.ByteString.Char8 as Char8
import qualified Data.Vector as V

import System.Process (createProcess, shell, CreateProcess(..), StdStream(..))
import System.IO (hSetBuffering, hSetBinaryMode, BufferMode(..))

-- Map from [-1.0, 1.0] to [0, 255] to a Char.
doubleToChar8 :: Double -> Char
doubleToChar8 x = toEnum $ truncate $ (x + 1) * 127.5

-- Prepare samples for playback by converting to the correct byte format.
prepareSamples :: S.Samples -> Char8.ByteString
prepareSamples = Char8.pack . map doubleToChar8 . V.toList

-- SoX input parameters.
soxParams :: String
soxParams = "-q -c 2 -b 8 -e unsigned -t raw"

-- The SoX command for sound playback.
soxPlay :: Double -> String
soxPlay sr = "play " ++ soxParams ++ " -r " ++ (show sr) ++ " -"

-- The SoX command for sound output to a file.
soxOutput :: Double -> String -> String
soxOutput sr path = 
    "sox " ++ soxParams ++ " -r " ++ (show sr) ++ " - -t wav " ++ path

-- Output the samples to a .wav file using SoX.
output :: FilePath -> Double -> S.Samples -> IO ()
output path sr samples = do
    let process = (shell $ soxOutput sr path) { std_in = CreatePipe }
    (Just handle, _, _, _) <- createProcess process
    hSetBuffering handle NoBuffering
    hSetBinaryMode handle True
    Char8.hPutStr handle $ prepareSamples $ samples

-- Play the samples using SoX.
play :: Double -> S.Samples -> IO ()
play sr samples = do
    let process = (shell $ soxPlay sr) { std_in = CreatePipe }
    (Just handle, _, _, _) <- createProcess process
    hSetBuffering handle NoBuffering
    hSetBinaryMode handle True
    Char8.hPutStr handle $ prepareSamples $ samples
