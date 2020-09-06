import Sound.RtMidi (InputDevice, closeDevice, closePort, defaultInput, getMessage, openPort)
import Control.Concurrent (forkIO, killThread)
import Control.Monad (forever)

mainLoop :: InputDevice -> IO ()
mainLoop d = do
  m <- getMessage d
  if length (snd m) > 0 then
    putStrLn $ show m
  else return ()

main :: IO ()
main = do
  i <- defaultInput
  openPort i 0 "RtMidi"
  id <- forkIO $ forever $ mainLoop i
  _ <- getLine
  killThread id
  closePort i
  closeDevice i
  return ()
