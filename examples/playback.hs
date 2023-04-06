import Control.Concurrent (threadDelay)
import qualified Data.Vector.Storable as VS
import Sound.RtMidi (closePort, defaultOutput, openPort, portCount, portName, sendMessage)

main :: IO ()
main = do
  device <- defaultOutput
  numPorts <- portCount device
  ports <- mapM (portName device) [0 .. numPorts - 1]
  mapM_ print $ zip [0 ..] ports
  putStrLn "select port: "
  selection <- getLine
  openPort device (read selection) "RtMidi"
  let arp0 = take 12 $ cycle [0x51, 0x55, 0x58]
  let arp1 = take 12 $ cycle [0x51, 0x56, 0x5a]
  let arp2 = take 12 $ cycle [0x50, 0x53, 0x58]
  let song = cycle (arp0 ++ arp1 ++ arp2 ++ arp0)
  putStrLn "playing..."
  mapM_ (\x -> sendMessage device (VS.fromList [0x90, x, 0x7f]) >> threadDelay 120000) $ take 240 song
  putStrLn "exiting..."
  closePort device
