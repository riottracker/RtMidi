import Sound.RtMidi
import Numeric
import Control.Concurrent
import Control.Monad
import Foreign.C

mainLoop :: Device -> IO ()
mainLoop d = do
  m <- getMessage d
  if length (fst m) > 0 then
    putStrLn $ show m
  else return ()

main :: IO ()
main = do
  i <- defaultInput
  openPort i 0 "RtMidi"
  id <- forkIO $ forever $ mainLoop i
  _ <- getLine
  killThread id
  closeInput i
  return ()
