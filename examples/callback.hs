import Data.Word (Word8)
import Sound.RtMidi (closeDevice, closePort, defaultInput, openPort, setCallback)
import Numeric (showHex)

callback :: Double -> [Word8] -> IO ()
callback delta msg = putStrLn $ (foldr (showHex . fromEnum) "" msg) ++ " - " ++ (show delta)

main :: IO ()
main = do
  i <- defaultInput
  openPort i 0 "RtMidi"
  setCallback i callback
  _ <- getLine
  closePort i
  closeDevice i
  return ()
