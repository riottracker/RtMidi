import Sound.RtMidi
import Numeric
import Foreign.C

callback :: CDouble -> [CUChar] -> IO ()
callback delta msg = putStrLn $ (foldr (showHex . fromEnum) "" msg) ++ " - " ++ (show delta)

main = do
  i <- defaultInput
  openPort i 0 "RtMidi"
  setCallback i callback
  _ <- getLine
  return ()
