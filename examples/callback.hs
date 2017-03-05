import Sound.RtMidi
import Numeric
import Foreign.C
import Foreign.Ptr
import Foreign.Marshal.Array

callback :: CDouble -> Ptr CUChar -> IO ()
callback delta msg = do
  message <- peekArray 3 msg
  putStrLn $ (foldr (showHex . fromEnum) "" message) ++ " - " ++ (show delta)

main = do
  i <- defaultInput
  openPort i 0 "RtMidi"
  setCallback i callback
  _ <- getLine
  return ()
