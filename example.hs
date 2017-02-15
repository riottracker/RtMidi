import Sound.RtMidi

main = do
  o <- output AlsaApi "riottracker"
  a <- currentApi o
  p <- portCount o
  d <- mapM (portName o) [0..p-1]
  mapM_ putStrLn d
