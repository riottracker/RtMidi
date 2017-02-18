import Sound.RtMidi
import Control.Concurrent

main = do
  o <- output AlsaApi "rtmidi"
  a <- currentApi o
  putStrLn $ "using api: " ++ show a
  threadDelay 10
  c <- compiledApis
  putStrLn $ "available: " ++ show c
  p <- portCount o
  ports <- mapM (portName o) [0..p-1]
  mapM_ (\t -> putStrLn $ show t) $ zip [0..] ports
  putStrLn "select port: "
  s <- getLine
  openPort o (read s) "RtMidi"
  let arp0 = take 12 $ cycle [0x51, 0x55, 0x58]
  let arp1 = take 12 $ cycle [0x51, 0x56, 0x5a]
  let arp2 = take 12 $ cycle [0x50, 0x53, 0x58]
  let song = cycle (arp0 ++ arp1 ++ arp2 ++ arp0)
  mapM_ (\x -> sendMessage o [0x90, x, 0x7f] >> threadDelay 120000) $ take 480 song
  closePort o
  close o
