module Play (play) where

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

-- The SoX command for sound playback.
soxPlay :: Double -> String
soxPlay sr = "play -q -c 2 -b 8 -e unsigned -t raw -r "
              ++ (show sr)
              ++ " -"

soxOutput :: Double -> String
soxOutput sr = "play "

-- Play the samples using SoX.
play :: Double -> S.Samples -> IO ()
play sr samples = do
    let process = (shell $ soxPlay sr) { std_in = CreatePipe }
    (Just handle, _, _, _) <- createProcess process
    hSetBuffering handle NoBuffering
    hSetBinaryMode handle True
    Char8.hPutStr handle $ prepareSamples $ samples
