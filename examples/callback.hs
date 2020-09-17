import qualified Data.Vector.Storable as VS
import Data.Word (Word8)
import Numeric (showHex)
import Sound.RtMidi (closePort, defaultInput, openPort, setCallback)

callback :: Double -> VS.Vector Word8 -> IO ()
callback delta msg = putStrLn $ (VS.foldr (showHex . fromEnum) "" msg) ++ " - " ++ (show delta)

main :: IO ()
main = do
  i <- defaultInput
  openPort i 0 "RtMidi"
  setCallback i callback
  _ <- getLine
  closePort i
  return ()
