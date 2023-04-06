import Control.Concurrent (forkIO, killThread)
import Control.Monad (forever, when)
import qualified Data.Vector.Storable as VS
import Sound.RtMidi (InputDevice, closePort, defaultInput, getMessage, openPort)

mainLoop :: InputDevice -> IO ()
mainLoop d = do
  m <- getMessage d
  when (VS.length (snd m) > 0) $ print m

main :: IO ()
main = do
  i <- defaultInput
  openPort i 0 "RtMidi"
  id <- forkIO $ forever $ mainLoop i
  _ <- getLine
  killThread id
  closePort i
  return ()
